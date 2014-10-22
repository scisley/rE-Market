#rm(list=ls(all=TRUE))
require(ggplot2)
require(reshape)

########### NOTES ###############
# I can make simplified versions of each firm that just include a single power plant for each fuel
# type. This would be vastly easier to solve, and not have so many discontinuities in the profit 
# function.

# Given that I have estimates for the profit functions, I should be able to find a structural equation
# that relates contributions to some value of the underlying parameters, and then test this 
# econometrically. Some statement like, 'firms that look like this should contribute more/less than ...'

######################### DATA CLEANING ###########################

## Now I need to incorporate the region and maybe even a load curve.
## Or should it be load curve, then region?

getPlantData <- function(file.name) {
  # Read in the parameter ranges
  plant.data <- read.csv(file.name, header=TRUE, na.strings = c("N/A","#N/A","?"), stringsAsFactors=FALSE)

  #Exclude plants with negative annual generation and unknown fuel types
  plant.data <- subset(plant.data, NetGeneration>=0 & Fuel != 'WSTHTOTPUR' & Fuel !='OTHRFOSL' & Fuel != '')

  # Do some unit conversions
  plant.data$HeatRate <- plant.data$HeatRate*1000/10^6 #convert from Btu/kWh to mmBtu/MWh
  plant.data$CO2Intensity <- plant.data$CO2Intensity / 2204.623 # Convert from lb/MWh to metric ton CO2 / MWh

  # Calculate variable + fuel costs for different power types
  # By heat rate
  biomass.var.costs <- 5.196150 # $/mmBtu (RAND Report)
  coal.var.costs <- 2.21 # is avg value from http://www.eia.gov/electricity/annual/html/epa_07_04.html
  gas.var.costs <- 4.74 # $/mmBtu2009 national average for electricity production (http://www.eia.gov/electricity/annual/html/epa_07_04.html)
  oil.var.costs <- 7.02 # $/mmBtu http://www.eia.gov/electricity/annual/html/epa_07_04.html
  
  #oil.var.costs <- (8.49+11.63+10.45)/3 #$/mmBtu (Newcomer and Apt)
  #gas.var.costs <- (9.95+10.52+7.79)/3 #$/mmBtu (Newcomer and Apt)
  #coal.var.costs <- (1.73+1.41+1.29)/3 #$/mmBtu (Newcomer and Apt)
  
  # By final output
  nuclear.var.costs <- 16.5 # $/MWh (Newcomer and Apt), would be nicer to have more resolution here
  wind.var.costs <- 20 # $/MWh (Newcomer and Apt)
  hydro.var.costs <- 10 # $/MWh (Newcomer and Apt)
  geothermal.var.costs <- 13.69 # CRS Power Plants Characteristics and Costs, Table 4
  solar.var.costs <- 13.71 # CRS Power Plants Characteristics and Costs, Table 4

  # Maximum utilization rates, ***guesses*** from wikipedia and my head
  # Better answer for wind is 0.35, see http://www.ceere.org/rerl/about_wind/RERL_Fact_Sheet_2a_Capacity_Factor.pdf pg 27
  # Nuclear, 90.3%, http://www.nei.org/Knowledge-Center/Nuclear-Statistics/US-Nuclear-Power-Plants/US-Nuclear-Capacity-Factors
  
  # http://www.eia.gov/electricity/annual/archive/03482009.pdf, table 5.2. These are actual capacity factors
  # and should not be used for plants that will likely gain operation due to dispatch order changes. 
  # Nuclear, 90.3%; Hydroelectric, 39.8%; 
  #util <-        c(0.85     ,0.85  ,0.85 ,0.95 ,0.93      ,0.3   ,0.5    ,0.9         ,0.2)
  util <-        c(0.85     ,0.85  ,0.85 ,0.85 ,0.93      ,0.35 ,0.5    ,0.85         ,0.2)
  names(util) <- c('BIOMASS','COAL','GAS','OIL','NUCLEAR','WIND','HYDRO','GEOTHERMAL','SOLAR')
  plant.data$MaxGeneration <- plant.data$Capacity*util[as.character(plant.data$Fuel)]*8760
  
  # TODO: Calculate the capacity factor of the top 10% of realized capacity factor for each fuel type. I might
  # be able to assume that these are 'top' performers that are only down for maintence and other issues.
  # Maybe go by the top 10 biggest? Those plants are likely to work a lot. Hard to figure out hydro. What about
  # using the actual value if its higher than the assumed value?
  
  plant.data$VarCosts <- NA

  plant.data[plant.data$Fuel=='OIL','VarCosts'] <- plant.data[plant.data$Fuel=='OIL','HeatRate']*oil.var.costs
  plant.data[plant.data$Fuel=='COAL','VarCosts'] <- plant.data[plant.data$Fuel=='COAL','HeatRate']*coal.var.costs
  plant.data[plant.data$Fuel=='GAS','VarCosts'] <- plant.data[plant.data$Fuel=='GAS','HeatRate']*gas.var.costs
  plant.data[plant.data$Fuel=='BIOMASS','VarCosts'] <- plant.data[plant.data$Fuel=='BIOMASS','HeatRate']*biomass.var.costs

  plant.data[plant.data$Fuel=='HYDRO','VarCosts'] <- hydro.var.costs
  plant.data[plant.data$Fuel=='WIND','VarCosts'] <- wind.var.costs
  plant.data[plant.data$Fuel=='NUCLEAR','VarCosts'] <- nuclear.var.costs
  plant.data[plant.data$Fuel=='GEOTHERMAL','VarCosts'] <- geothermal.var.costs
  plant.data[plant.data$Fuel=='SOLAR','VarCosts'] <- solar.var.costs
  
  plant.data$FuelAgg <- plant.data$Fuel
  plant.data$FuelAgg[plant.data$FuelAgg=='SOLAR'] <- "OTHER RENEWABLE"
  plant.data$FuelAgg[plant.data$FuelAgg=='GEOTHERMAL'] <- "OTHER RENEWABLE"
  plant.data$FuelAgg[plant.data$FuelAgg=='BIOMASS'] <- "OTHER RENEWABLE"
  
  return(plant.data)
}
######################### FUNCTIONS ###########################

# Create a load curve for a specific region, using data from a specific file
# Returns a function that gives the demand (in MWh) for the supplied time ratio.
# The time ratio goes from 0 to 1, with 1 corresponding to the lowest demand
# seen in whatever time period is being considered, and 0 the maximum.
# 'energy' will end up being the total area under the curve.
generateLoadDurationCurve <- function(load.data, energy, check.plots=T) {
  time.ratio <- load.data$TR
  
  # The regional data is reported in MWh, the time data is scaled from 0 to 1.
  # So to keep the vertical axis in MWh, each value needs to be scaled.
  ldc <- load.data$Load * nrow(load.data) 
  interp <- approxfun(time.ratio, y = ldc, method = "linear")
  
  load.area <- integrate(interp, lower=0, upper=1)$value  
  scale.factor <- energy/load.area
  
  loadDuration <- function(x) {
    interp(x)*scale.factor
  }
  # integrate(loadDuration, 0,1) # Check that the total energy works out right.

  if (check.plots==T) {
    # Plot the curve over the original data
    d <- data.frame(Source='Raw Data', Load=ldc, TR=load.data$TR)
    tr.sim <- seq(0,1,length=500)
    fit.data <- sapply(tr.sim, loadDuration)
    d <- rbind(d, data.frame(Source='Scaled', Load=fit.data, TR=tr.sim))
    dev.new()
    print(ggplot(d, aes(TR, Load)) + geom_line(aes(color=Source), size=1) + 
          ggtitle(paste('Load Duration Curve', load.data[1,'Region'])) +
          ylim(0, max(d$Load)))
  }
  
  return(loadDuration)
}

findAverageLoads <- function(loadCurve, load.groups) {
  
  funcMean <- function(func, lower, upper, samples = 100) {
    mean(sapply(seq(lower,upper,length=samples), func))
  }
  
  ans <- rep(NA, length=length(load.groups)-1)
  for(i in 1:(length(load.groups)-1)) {
    ans[i] <- funcMean(loadCurve, load.groups[i], upper=load.groups[i+1])
  }
  
  return(ans)
}
#findAverageLoads(misoLDC, load.groups)


## For the given set of plants, create an object that returns a list saying production, emissions, 
## and profit for any price p and carbon price pc. The ramp is the price needed to go from zero 
## production to full production. It helps smooth out curves.
createPlantFunction <- function(plants, ramp=0.1) {
  force(plants)
  supplyVector <- function(p, pc) {
    marginal.cost <- plants$VarCosts + plants$CO2Intensity*pc
    return(pmin(plants$MaxGeneration, pmax(0,(p-marginal.cost)*plants$MaxGeneration/ramp)))
  }
  
  supply <- function(p, pc) {
    sum(supplyVector(p,pc))
  }
  
  profit <- function(p, pc) {
    marginal.cost <- plants$VarCosts + plants$CO2Intensity*pc
    margin <- p-marginal.cost
    supply.vector <- pmin(plants$MaxGeneration, pmax(0,margin*plants$MaxGeneration/ramp))
    return(sum(supply.vector*margin))
  }
  
  revenue <- function(p, pc) {
    marginal.cost <- plants$VarCosts + plants$CO2Intensity*pc
    margin <- p-marginal.cost
    supply.vector <- pmin(plants$MaxGeneration, pmax(0,margin*plants$MaxGeneration/ramp))
    return(sum(supply.vector*p))
  }
  
  emissions <- function(p, pc) {
    return(sum(supplyVector(p,pc)*plants$CO2Intensity))
  }
  
  return(list(plants=plants, # Necessary reference to plants data
              supply = supply, # Used a bunch
              profit = profit, # Used a bunch
              revenue = revenue,
              emissions = emissions)) # Used primarily to support government
}

# Creat an object representing a specific group of plants operating under a specific demand condition
makeLoadSegment <- function(owner.names, plants, demand, name='Segment', ramp=0.1) {
  owners <- by(plants, owner.names, createPlantFunction, ramp=ramp)
  industryFunction <- createPlantFunction(plants, ramp=ramp)
  
  # The plural is awkward, but necessary to make this interchangeable with more aggregated objects
  clearingPrice <- function(pc) {  
    uniroot(function(x) {industryFunction$supply(x, pc)-demand(x)}, c(5, 300), tol = 1e-9)$root
  }
  
  return(list(owners = owners,
              clearingPrice = clearingPrice, clearingPrices=clearingPrice,
              demand = demand,
              supply = industryFunction$supply,
              emissions = industryFunction$emissions,
              profit = industryFunction$profit,
              revenue = industryFunction$revenue,
              name = name))
}

# Make an industry object that has one sub-industry for each load duration group
assembleSector <- function(load.segments, t.weights=rep(1,length(load.segments)), name='Sector') {
  
  n <- length(load.segments)
  weight <- sum(t.weights)

  # All load.segments will have the same sets of owners (load.segments only differ by load duration group)
  owner.names <- names(load.segments[[1]]$owners)
  
  # Creates a function that gives the profit for a particular owner, weighting the profits
  # from the different load segments appropriately.
  profitFunction <- function(t.weights, owner) {
    force(owner)
    return(function(p.vect, pc) {
      sub.profits <- sapply(1:n, function(i) { load.segments[[i]]$owners[[owner]]$profit(p.vect[i], pc) } )
      sum(t.weights*sub.profits)
    })
  }
  
  profitByOwner <- function(p.vect, pc, owner) {
    if (!owner %in% owner.names) return(0)
    sub.profits <- sapply(1:n, function(i) { load.segments[[i]]$owners[[owner]]$profit(p.vect[i], pc) } )
    sum(t.weights*sub.profits)
  }
  
  owners <- list()  
  for (owner in owner.names) {
    owners[[owner]] <- list(profit=profitFunction(t.weights, owner))
  }
  
  # Return the values for each industry as a vector for components that require p and pc
  values <- function(p.vect, pc, name) {
    sapply(1:n, function(x) load.segments[[x]][[name]](p.vect[x],pc))
  }
  
  weightedValues <- function(p.vect, pc, name) {
    t.weights*sapply(1:n, function(x) load.segments[[x]][[name]](p.vect[x],pc))
  }
  
  clearingPrices <- function(pc) {
    sapply(1:n, function(x) load.segments[[x]]$clearingPrice(pc))
  }
  
  # For aggregating over load segments we need to weight by the *supply* not the
  # amount of time that supply was required.
  clearingPrice <- function(p.vect, pc) {
    supply.values <- values(p.vect, pc, name='supply')
    sum(supply.values * p.vect) / sum(supply.values)
  }
  
  # Return the weighted sum of subindustry profits
  profit <- function(p.vect, pc) {
    sum(t.weights*values(p.vect, pc, name='profit'))
  }
  
  revenue <- function(p.vect, pc) {
    sum(t.weights*values(p.vect, pc, name='revenue'))
  }
  
  # Return the weighted sum of the subindustry emissions
  emissions <- function(p.vect, pc) {
    sum(t.weights*values(p.vect, pc, name='emissions'))
  }
  
  supply <- function(p.vect, pc) {
    sum(t.weights*values(p.vect, pc, name='supply'))
  }
  
  weightedAvgValue <- function(p.vect, pc, name) {
    sum(t.weights*values(p.vect, pc, name))
  }
  
  totalValue <- function(p.vect, pc, name) {
    sum(t.weights*values(p.vect, pc, name))
  }
  
  # Return the total demand (weighting the segments) for the given price vector
  demand <- function(p.vect) {
    sum(t.weights * sapply(1:n, function(x) load.segments[[x]]$demand(p.vect[x])))
  }
  
  return(list(load.segments = load.segments,
              owners = owners,
              profitByOwner = profitByOwner,
              t.weights = t.weights,
              weightedAvgValue = weightedAvgValue,
              totalValue = totalValue,
              values = values,
              weightedValues = weightedValues,
              clearingPrices = clearingPrices,
              clearingPrice = clearingPrice,
              demand = demand,
              supply = supply,
              emissions = emissions,
              profit = profit,
              revenue = revenue,
              name = name))
}

# Creates the grand poo-bah object.
assembleIndustry <- function(sectors, name='Industry') {
  # What do I want this to do? Report back things like profit by firm
  # Emissions, prices, generation
  n <- length(sectors)
  
  # Create a unique list of all the owners in the industry
  owner.names <- unique(unlist(sapply(1:n, function(i) names(sectors[[i]]$owners))))
  
  # Get or create unique names for the sectors
  sector.names <- names(sectors)
  if (length(unique(sector.names)) != n) sector.names <- paste(1:n, sector.names, sep='')
  
  # Return the clearing prices for all the sectors and load segments
  # Returns a list, with each element for a sector that contains a vector
  # of prices for the different load segments.
  clearingPrices <- function(pc) {
    p.clear <- lapply(1:n, function(i) sectors[[i]]$clearingPrices(pc))
    names(p.clear) <- sector.names
    return(p.clear)
  }
  
  clearingPrice <- function(p.list, pc) {
    sector.prices <- sapply(1:n, function(i) sectors[[i]]$clearingPrice(p.list[[i]], pc))
    sector.supply <- unlist(sectorSupply(p.list, pc))
    sum(sector.prices*sector.supply)/sum(sector.supply)
  }
  
  # The total value (including weights) for the specified parameter over all sectors
  # and load segments
  value <- function(p.list, pc, name) {
    sum(unlist(sectorValues(p.list, pc, name)))
  }
  
  # A list with each entry the weighted total over all load segments in a sector
  # p.list is expected as the output of clearingPrices
  sectorValues <- function(p.list, pc, name) {
    sector.values <- lapply(1:n, function(i) sectors[[i]]$totalValue(p.list[[i]], pc, name))
    names(sector.values) <- sector.names
    return(sector.values)
  }
  
  # A list, with each entry a vector of weighted load segment values.
  sectorSegmentValues <- function(p.list, pc, name) {
    sector.values <- lapply(1:n, function(i) sectors[[i]]$weightedValues(p.list[[i]], pc, name))
    names(sector.values) <- sector.names
    sector.values
  }
  
  # A list, one item for each sector, that contains the weights for the load segments
  sector.weights <- lapply(1:n, function(i) sectors[[i]]$t.weights)
  
  profitByOwners <- function(p.list, pc, owners) { 
    owner.profits <- lapply(owners, function(owner) sum(sapply(1:n, function(i) 
           sectors[[i]]$profitByOwner(p.list[[i]], pc, owner))))
    names(owner.profits) <- owners
    return(owner.profits)
  }
  
  demand <- function(p.list) {
    sum(sapply(1:n, function(i) sectors[[i]]$demand(p.list[[i]])))
  }
  
  revenue <- function(p.list, pc) { value(p.list, pc, 'revenue') }
  
  profit <- function(p.list, pc) { value(p.list, pc, 'profit') }
  sectorProfits <- function(p.list, pc) { sectorValues(p.list, pc, 'profit') }
  sectorSegmentProfits <- function(p.list, pc) { sectorSegmentValues(p.list, pc, name='profit') }
  
  supply <- function(p.list, pc) { value(p.list, pc, name='supply') }
  sectorSupply <- function(p.list, pc) { sectorValues(p.list, pc, 'supply') }
  sectorSegmentSupply <- function(p.list, pc) { sectorSegmentValues(p.list, pc, name='supply') }
  
  emissions <- function(p.list, pc) { value(p.list, pc, name='emissions') }
  sectorEmissions <- function(p.list, pc) { sectorValues(p.list, pc, 'emissions') }
  sectorSegmentEmissions <- function(p.list, pc) { sectorSegmentValues(p.list, pc, name='emissions') }
  
  p.0 <- clearingPrices(pc=0)
  s.0 <- sectorSegmentSupply(p.0, pc=0)
  e.0 <- sectorSegmentEmissions(p.0, pc=0)
  no.tax.values <- list(price=p.0, supply=s.0, emissions=e.0)
  
  
  return(list(sectors=sectors, sector.weights=sector.weights,
              clearingPrices=clearingPrices,
              clearingPrice=clearingPrice,
              sectorProfits=sectorProfits,
              sectorSegmentProfits=sectorSegmentProfits,
              profit=profit, emissions=emissions, supply=supply,
              revenue=revenue,
              sectorSupply=sectorSupply,
              sectorSegmentSupply=sectorSegmentSupply,
              sectorEmissions=sectorEmissions,
              sectorSegmentEmissions=sectorSegmentEmissions,
              profitByOwners=profitByOwners,
              name=name,
              demand=demand,
              sector.count <- n,
              no.tax.values=no.tax.values))
}

# Industry is an object, NOT a list of sectors
calcAggContributions <- function(industry, lobby.names, gov) {

  pc.minus <- list()
  eqbm.profits <- list()
  contributions <- list()
  other.data <- list()
  n <- length(industry$sectors)
  
  # Need the 'overall' equilibrium negotiated price to start with for each sector
  pc.eqbm <- aggNegotiate(industry, lobby.names, gov)
  p.eqbm <- industry$clearingPrices(pc.eqbm)
  
  print(paste("Equilibrium Pc", pc.eqbm))

  # Equilibrium profits for each lobby
  eqbm.profits <- industry$profitByOwners(p.eqbm, pc.eqbm, lobby.names) 
  total.eqbm.profits <- sum(unlist(eqbm.profits))
  # Calculate pc.minus for each owner
  for(lobby in lobby.names) {
    sub.names <- lobby.names[lobby.names!=lobby]
    pc <- aggNegotiate(industry, sub.names, gov)
    pc.minus[[lobby]] <- pc
    p.minus <- industry$clearingPrices(pc)    
    profit.minus <- industry$profitByOwners(p.minus, pc, sub.names) 
    
    other.profit.at.eqbm <- total.eqbm.profits - eqbm.profits[[lobby]]
    other.profit.at.pc.minus <- sum(unlist(profit.minus))
    change.in.gov.welfare <- gov(pc, industry, p.minus) - gov(pc.eqbm, industry, p.eqbm)
    contributions[[lobby]] <- other.profit.at.pc.minus - other.profit.at.eqbm + change.in.gov.welfare
    
    other.data[[lobby]] <- list(other.profit.at.pc.minus=other.profit.at.pc.minus,
                                other.profit.at.eqbm=other.profit.at.eqbm,
                                change.in.gov.welfare=change.in.gov.welfare)
    print(paste(lobby,"Contribution:",contributions[[lobby]],"pc.minus",pc))
  }
  
  return(list(contributions = contributions,
              pc.minus = pc.minus,
              pc.eqbm = pc.eqbm,
              p.eqbm = p.eqbm,
              eqbm.profits = eqbm.profits,
              other.data = other.data))
  
}

# Industry is an industry object, NOT a list of sectors like before
aggNegotiate <- function(industry, lobbyers, gov) { 

  n <- length(industry$sectors)

  welfare <- function(pc) {
    # The first two lines take the bulk of the time.
    p.list <- industry$clearingPrices(pc)
    lobby.profit <- sum(unlist(industry$profitByOwners(p.list, pc, lobbyers)))
    lobby.profit + gov(pc, industry, p.list)
  }

  eps <- 0
  rough.pc <- seq(eps, 50-eps, length=500) # Tried 1000, didn't improve results much
  rough.welfare <- sapply(rough.pc, welfare)
  rough.pc.max.i <- which.max(rough.welfare)
  lbound <- rough.pc[max(1, rough.pc.max.i-1)]
  ubound <- rough.pc[min(length(rough.pc), rough.pc.max.i+1)]
  init.val <- mean(c(lbound, ubound))
  
  lobbied.pc <- optim(init.val, welfare, 
                    method='Brent', lower=lbound, upper=ubound, 
                    control=list(fnscale=-1, reltol=1e-12))
  
  # Double check if the welfare at zero is higher.
  if (welfare(0) > lobbied.pc$value)
    return(0)
  else
    return(lobbied.pc$par)
}

## Updated to include price spikes, revenue, emissions, and production losses
#componentGov <- function(scc, weight) {
componentGov <- function(weights) {
  scc <- weights[1]
  spike.weight <- weights[2]
  revenue.weight <- weights[3]
  production.weight <- weights[4]
  
  # The p.list is provided so that the government can quickly calculate anything else
  # it needs, rather than recomputing it, which can be very expensive. 
  function(pc, industry, p.list) {
    p.0 <- industry$no.tax.values$price # pre-computed, no penalty
    s.0 <- industry$no.tax.values$supply # pre-computed, no penalty
    t.w <- industry$sector.weights
    s   <- industry$sectorSegmentSupply(p.list, scc)
    
    emissions <- industry$emissions(p.list, pc)
    revenue <- emissions*pc
    
    spike.loss <- calcSpikeLoss(p.0, p.list, s.0)
    production.loss <- calcProductionLoss(p.0, s, s.0)
    return(-scc*emissions + revenue.weight*revenue
           -spike.weight*spike.loss + production.weight*production.loss)
  }

}

calcProductionLoss <- function(p.0, p, s.0) {
  # Looks weird, for each sector i, find revenue lost by industry due to the carbon price
  # and add it all up. The interior sum is over load segments, the outer sum is over sectors.
  #sum(sapply(1:length(p.0), function(i) sum(p.0[[i]]*(s.0[[i]]-s[[i]]))))
  return(0)
}

calcSpikeLoss <- function(p.0, p, s.0) {
  # Looks weird, for each sector i, find extra money spent to buy original amount
  # and add it all up. The interior sum is over load segments, the outer sum is over sectors.
  sum(sapply(1:length(p.0), function(i) sum(s.0[[i]]*(p[[i]]-p.0[[i]]))))
}

# Returns a constant elasticity function using the supplied e and either a utilization
# ratio and a list of plants, or set plants equal to some initial supply level.
ceDemand <- function(e, plants, util, ini.supply) {
  A <- 0
  supply <- createPlantFunction(plants)$supply # NO RAMP!
  if (missing(ini.supply)) {
    # Assume the plants is specifying a bunch of generators.
    max.supply <- supply(Inf,0)
    ini.supply <- max.supply*util
  }
  
  # Find p that results in ini.supply (pc=0)
  ini.p <- uniroot(function(x) { supply(x,0)-ini.supply }, c(0, 400))$root
  A <- ini.supply/ini.p^e
    
  return( function(price) { A*price^e } )
}

exclude <- function(char.vect, drop) {
  char.vect[!char.vect %in% drop]
}

# Returns an industry object based on the NERC regions, minus Alaska and Hawaii
makeNercIndustry <- function(pd, load.data, elasticity, ramp=5, load.groups = c(0,1), owner.column='ParentPAC') {
  
  t.weights <- load.groups[-1]-load.groups[-length(load.groups)]
  nerc.regions <- unique(pd$NERC)
  nerc.regions <- nerc.regions[nerc.regions != 'ASCC' & nerc.regions !='HICC']
  pd.nerc <- list()
  loadCurve <- list()
  nerc.names <- list()
  generation <- list()
  averages <- list()
  sectors <- list()
  for (nerc in nerc.regions) {
    pd.nerc[[nerc]] <- subset(pd, NERC==nerc)
    generation[[nerc]] <- sum(pd.nerc[[nerc]]$NetGeneration)
    loadCurve[[nerc]] <- generateLoadDurationCurve(subset(load.data, Region=='AVG'), generation[[nerc]], check.plots=F)
    
    averages[[nerc]] <- findAverageLoads(loadCurve[[nerc]], load.groups)
    nerc.names[[nerc]] <- paste(nerc, load.groups[-length(load.groups)], load.groups[-1])

    load.segments <- lapply(1:length(t.weights), function(i) {
      sub.demand <- ceDemand(e=elasticity, pd.nerc[[nerc]], ini.supply=averages[[nerc]][i])
      makeLoadSegment(pd.nerc[[nerc]][owner.column], pd.nerc[[nerc]], sub.demand, 
                      name=nerc.names[[nerc]][i], ramp=ramp )
    })
    sectors[[nerc]] <- assembleSector(load.segments, t.weights, name=paste(nerc,'Sector'))
  }
  assembleIndustry(sectors)
}

makeSingleIndustry <- function(pd, load.data, elasticity, ramp, load.groups=c(0,1)) {
  t.weights <- load.groups[-1]-load.groups[-length(load.groups)]
  generation <- sum(pd$NetGeneration)
  loadCurve <- generateLoadDurationCurve(subset(load.data, Region=='AVG'), generation, check.plots=F)
  averages <- findAverageLoads(loadCurve, load.groups)

  demand <- ceDemand(e=elasticity, pd, ini.supply=averages)
  lg.names <- paste('LG', load.groups[-length(load.groups)], load.groups[-1])
  load.segments <- lapply(1:length(t.weights), function(i) {
      sub.demand <- ceDemand(e=elasticity, pd, ini.supply=averages[i])
      makeLoadSegment(pd['ParentPAC'], pd, sub.demand, 
                      name=lg.names[i], ramp=ramp )
    })
  
  sector <- assembleSector(load.segments, t.weights=1, name='single sector')
  industry <- assembleIndustry(list(sector), name='Entire US')
}













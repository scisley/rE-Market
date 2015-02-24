#rm(list=ls(all=TRUE))
require(ggplot2)
require(reshape)
library(scales)

# These are functions to help with general purpose plotting for the rE-market simulation

makeIndustryDiagnosticPlots <- function(industry, gov, lobbies, pc.seq=seq(0, 50, length=200), 
           p.clear=NULL, supply.pc.list=seq(0,100,20)) {
  if (is.null(p.clear)) p.clear <- lapply(pc.seq, industry[['clearingPrices']])
  
  dev.new(); print(makeSupplyDemandPlot(industry, p.clear))
  dev.new(); print(makeWelfarePlot(industry, gov, lobbies, pc.seq=pc.seq, p.clear=p.clear))
  dev.new(); print(makeStandardPlot(industry, type='emissions', pc.seq=pc.seq, p.clear=p.clear))
  dev.new(); print(makeStandardPlot(industry, type='supply', pc.seq=pc.seq, p.clear=p.clear))
}

makeSupplyDemandPlot <- function(industry, p.clear=NULL, supply.pc.list=seq(0,100,20), data=F) {
  # only needed as a template
  if (is.null(p.clear)) p.clear <- industry[['clearingPrices']](0)
  # Overall Supply and Demand curves. Overall demand is summed demand over all sectors for the
  # given prices.
  p.seq <- seq(0.01, 300, length=200)
  demand <- sapply(p.seq, function(x) industry$demand(constantPriceList(p.clear[[1]], x)) )
  
  dd <- data.frame(price=p.seq, demand=demand)
  
  ds <- data.frame()
  for (pc in supply.pc.list) {
    supply <- sapply(p.seq, function(x) industry$supply(constantPriceList(p.clear[[1]], x), pc=pc))
    ds <- rbind(ds, data.frame(price=p.seq, supply=supply, pc=pc))
  }
  
  if (data) return(list(demand=dd, supply=ds))
  ds$pc <- as.factor(ds$pc)
  ggplot(ds, aes(supply, price)) + geom_line(size=1, aes(color=pc, linetype=pc)) +
         geom_line(data=dd[dd$demand<max(ds$supply),], size=1, mapping=aes(demand, price)) + 
         ggtitle(paste('Supply and Demand for', industry$name)) + 
         ylab('SRMC\n($/MWh)') + xlab('Supply (MWh)') + scale_colour_grey()
  
}

# Plot industry total profit, profit for the given lobbies, and government welfare
makeWelfarePlot <- function(industry, gov, lobbies, pc.seq=seq(0, 50, length=200), 
                            p.clear=NULL, data=F) {
  if (is.null(p.clear)) p.clear <- lapply(pc.seq, industry[['clearingPrices']])
  # Profit
  profit <- mapply(industry$profit, p.clear, pc.seq)
  
  # Government welfare
  gov.welfare <- mapply(function(x,y) gov(x, industry, y), pc.seq, p.clear)
  gov.welfare <- gov.welfare - min(gov.welfare) # Shift to min zero
  
  lobby.profit <- getProfitByOwner(industry, lobbies, pc.seq, p.clear)
  lobby.total.profit <- aggregate(lobby.profit$profit, list(lobby.profit$pc), sum)
  lobby.total.profit$entity='Lobbies'
  names(lobby.total.profit) <- c('pc','welfare','entity')
  
  d <- rbind(lobby.total.profit,
             data.frame(pc=pc.seq, welfare=profit, entity='Owners'),
             data.frame(pc=pc.seq, welfare=gov.welfare, entity='Gov'),
             data.frame(pc=pc.seq, welfare=lobby.total.profit$welfare+gov.welfare, entity='All'))
  if (data) return(d)
  ggplot(d, aes(pc, welfare)) + geom_line(size=1, aes(color=entity)) +
         ggtitle(paste('Welfare for ', industry$name))
  
}


constantPriceList <- function(p.list, num) {
  #p.list is a list, with each item containing a vector of possibly (but unlikely) different lengths
  for (i in 1:length(p.list)) p.list[[i]] <- rep(num, length(p.list[[i]]))
  return(p.list)
}

# Take a list of dataframes and compile them into a data frame
# ids is the names for each of the 'chunks' and will be in the resulting table
# col.names are the names of the items within each 'chunk' and will be column names
makeDataFrameFromList <- function(a.list, ids=names(a.list), col.names = names(a.list[[1]]), id.column.name='ID') {
  if (is.null(ids)) ids <- paste('ID',1:length(a.list),sep='')
  if (class(ids)=='list') ids <- unlist(ids)
  if (is.null(col.names)) col.names <- paste('C',1:length(a.list[[1]]),sep='')
  col.names <- c(col.names, id.column.name)
  d <- data.frame()
  for(i in 1:length(a.list)) {
    dd <- data.frame(as.matrix(a.list[[i]],nrow=1), c2=ids[i]) #FUCK this line...
    names(dd) <- col.names
    d <- rbind(d, dd)
  }
  return(d)
}

# Takes any market structure (load segment, sector, or industry) and plots either 
# a profit, supply, or emissions curve. Setting plot to false will return the data frame
# rather than make a plot
makeStandardPlot <- function(market, type='profit', pc.seq=seq(0, 50, length=200), p.clear=NULL, plot=T ) {
  if (!type %in% c('profit','emissions','supply')) stop('Cannot support the type indicated')
  # Calculate clearing prices if not supplied
  if (is.null(p.clear)) p.clear <- lapply(pc.seq, market[['clearingPrices']])
  
  outcome <- mapply(market[[type]], p.clear, pc.seq)
  d <- data.frame(pc=pc.seq, result=outcome)
  if (!plot) return(d)
  # Now make the ggplot, and add some test code, one for each type of market structure.
  ggplot(d, aes(pc, result)) + geom_line(size=1) + ggtitle(paste(type,'for',market$name)) + ylab('')
}
#dev.new(); makeStandardPlot(industry)


makePlotsForChildren <- function(child.list, plotFunction, type='profit', ..., 
                                 weights=rep(1,length(child.list)), combine=F) {
  if (combine) {
    child.data <- lapply(child.list, plotFunction, type=type, ..., plot=!combine)
    c.data <- data.frame()
    for (i in 1:length(child.data)) { 
      ch <- child.data[[i]]
      c.data <- rbind(c.data, data.frame(pc=ch$pc, result=ch$result*weights[i], name=child.list[[i]]$name))
    }
    out.plot <- ggplot(c.data, aes(pc, result)) + geom_line(size=1, aes(color=name)) + 
                ggtitle(paste('Dissaggregated', type)) + ylab('')
    return(out.plot)
  }
  for (child.plot in child.plots) { dev.new(); print(child.plot) }
}
#makePlotsForChildren(industry$sectors, makeStandardPlot, combine=T)
#makePlotsForChildren(industry$sectors[[2]]$load.segments, makeStandardPlot, weights=industry$sectors[[2]]$t.weights, combine=T)

## TODO: Plots for profit by owner, government welfare

# Market can be either an industry or sector. Owners is a vector of owner names
profitByOwnerPlot <- function(market, owners, pc.seq=seq(0, 50, length=200), p.clear=NULL, plot=T) {
  if (is.null(colors)) colors <- owners
  if (is.null(p.clear)) p.clear <- lapply(pc.seq, market[['clearingPrices']])
  n <- length(owners)
  
  d <- getProfitByOwner(market, owners, pc.seq, p.clear)
  
  if (plot==FALSE) return(d)
  
  d$owner <- factor(d$owner, ordered=TRUE, levels=owners)
  
  ggplot(d, aes(pc, profit)) + geom_line(size=1, aes(color=owner)) + 
                ggtitle('Profit by Owners') + ylab('')
}

getProfitByOwner <- function(market, owners, pc.seq, p.clear) {
  profit.list <- lapply(owners, function(owner) mapply(market$profitByOwner, p.clear, pc.seq, owner))
  
  # Aggregate them all into one data frame
  d <- data.frame()
  for (i in 1:length(owners)) {
    d <- rbind(d, data.frame(pc=pc.seq, unlist(profit.list[[i]]), owner=owners[i]))
  }
  names(d) <- c('pc','profit','owner')
  return(d)
}

supplyByOwnerPlot <- function(industry, pc.seq=seq(0, 50, length=200), 
           p.clear=NULL, supply.pc.list=seq(0,100,20)) {
  if (is.null(p.clear)) p.clear <- lapply(pc.seq, industry[['clearingPrices']])
  # Overall Supply and Demand curves. Overall demand is summed demand over all sectors for the
  # given prices.
  
  ds <- data.frame()
  for (pc in supply.pc.list) {
    supply <- sapply(p.seq, function(x) industry$supply(constantPriceList(p.clear[[1]], x), pc=pc))
    ds <- rbind(ds, data.frame(price=p.seq, supply=supply, pc=pc))
  }
  
  dev.new()
  print( ggplot(ds, aes(supply, price)) + geom_line(size=1, aes(color=as.factor(pc))) +
         geom_line(data=dd, size=1, mapping=aes(demand, price)) + 
         ggtitle(paste('Supply and Demand for', industry$name)) + ylab('Q') + xlim(0,max(supply)) )
  
}

combinePlots <- function(plot.list, geom) {
  d <- combinePlotData(plot.list, 'Plot')
  ggplot(d, aes(color=Plot)) + geom
}
#dev.new(); combinePlots(price.pc.list, geom_line(aes(x=pc,y=clearing.price)))

# Take a list of plot and pull out the data from each. Then add a column named
# var.name for each name in the supplied plot.list.
combinePlotData <- function(plot.list, var.name='Plot') {
  if (is.null(names(plot.list)))
    names(plot.list) <- paste('Plot', 1:length(plot.list))
  
  d <- data.frame()
  for (name in names(plot.list)) {
    # Pull out the data
    my.factors <- factor(names(plot.list), levels=names(plot.list), ordered=T)
    tmp <- plot.list[[name]]$data
    tmp[,var.name] <- name
    d <- rbind(d, tmp)
  }
  d
}

# Plot supply curves for a set of owners
# plotOwnerSupply <- function(industry, owners, price=seq(0,300,length=200), pc=0) {
  # d <- data.frame()
  # for (owner in owners) {
    # plant.function <- industry$owners[[owner]]
    # supply <- sapply(price, plant.function$supply, pc=pc)
    # d <- rbind(d, data.frame(supply=supply, price=price, owner=owner))
  # }
  # p <- ggplot(d, aes(supply, price)) + geom_line(aes(color=owner)) + 
         # ggtitle(paste('Supply by Owner at Pc =', pc)) + guides(color=FALSE)
  # return(p)
# }
#plotSupply(ind.pac, names(ind.pac$owners), price=seq(0,200,length=200)) + xlim(0,42000)

# Plot the profit of a single set of plants.
plotProfit <- function(industry, owners, pc.seq=seq(0,50,length=200)) {
  p.clear <- sapply(pc.seq, industry$clearingPrice)
  d <- data.frame()
  for (owner in owners) {
    plant.function <- industry$owners[[owner]]$profit
    profit <- mapply(plant.function, p=p.clear, pc=pc.seq)
    d <- rbind(d, data.frame(profit=profit, pc=pc.seq, owner=owner))
  }
  p <- ggplot(d, aes(pc, profit)) + geom_line(aes(color=owner)) + 
       ggtitle(paste('Profit for', owner))
  return(p)
}

## Test code
#pd <- getPlantData()
#demand <- ceDemand(e=-0.2, 0.8, pd)
#ind <- assembleIndustry(pd$Fuel, pd, demand)
#plots <- lapply(ind$owners, plotSupply)

# Plot the combined welfare of the lobbbyst in an industry and the government.
plotWelfare <- function(industry, lobbyers, gov, pc.seq = seq(0,50,length=100)) { 
  profit.functions <- list()
  for (lobby in lobbyers) profit.functions[[lobby]] <- industry$owners[[lobby]]$profit
  
  welfare <- function(pc) {
    p <- industry$clearingPrice(pc)
    # Get total profit of all lobbyers
    lobby.profit <- sum(mapply(do.call, profit.functions, list(args=list(p=p,pc=pc))))
    lobby.profit + gov(pc)
  }
  
  profit <- sapply(pc.seq, welfare)
  d <- data.frame(pc=pc.seq, profit=profit)
  p <- ggplot(d, aes(pc, profit)) + geom_line() + 
       ggtitle('Welfare by Pc')
  return(p)
}
#out <- plotWelfare(owners, lobbyers, demand, pc.seq <- seq(0,60,length=500))


#pd <- getPlantData()
#demand <- ceDemand(A=1946017, e=-0.2)
#ind <- industry(pd$Fuel, pd, demand)
#negotiate(ind, names(ind$owners), function(x) {0})

makeDiagnosticPlots <- function(industry, lobbyers, gov, pc.seq = seq(0, 100, length=200), supply.pc.list=seq(0,100,20), showPlots=T) {
  
  price <- seq(1, 250, length=length(pc.seq))
  p.0 <- industry$clearingPrice(pc=0)
  s.0 <- industry$emissions(p.0, 0)
  # Supply at pc.supply.list and Demand
  d <- data.frame()
  for (pc in supply.pc.list) {
    supply <- sapply(price, industry$supply, pc=pc)
    d <- rbind(d, data.frame(supply=supply, price=price, pc=pc))
  }
  demand.data <- data.frame(price=price, demand=industry$demand(price))
  
  max.supply <- industry$supply(Inf,0)
  supply.plot <- ggplot(d, aes(supply, price)) + geom_line(aes(colour = as.factor(pc)), size=1) + 
         geom_line(data=demand.data, mapping=aes(x=demand,y=price), size=1) + 
         ggtitle(paste('Supply and Demand\n',industry$name)) + xlim(0, max.supply) #+ ylim(0,250) #+ theme_bw()
  
  # Government welfare, owner profits
  p.clear <- sapply(pc.seq, industry$clearingPrice)
  gov.welfare <- rep(NA, length(pc.seq))
  for (i in 1:length(pc.seq)) {
    spike.loss <- (p.0-p.clear[i])*s.0
    econ <- list(spike.loss=spike.loss, emissions=industry$emissions(p.clear[i],pc.seq[i]))
    gov.welfare[i] <- gov(pc.seq[i], econ)
  }
  
  d.profit <- data.frame()
  for (owner in names(industry$owners)) {
    welfare <- mapply(industry$owners[[owner]]$profit, p.clear, pc.seq)
    d.profit <- rbind(d.profit, data.frame(pc=pc.seq, welfare=welfare, owner=owner))
  }
  
  
  d.lobby.profit <- data.frame()
  for (owner in lobbyers) {
    welfare <- mapply(industry$owners[[owner]]$profit, p.clear, pc.seq)
    d.lobby.profit <- rbind(d.lobby.profit, data.frame(pc=pc.seq, welfare=welfare, entity='lobbyers'))
  }
  
  if (length(industry$owners)>15) {
    legend.status <- FALSE
  } else {
    legend.status <- TRUE
  }
  
  # Put the government welfare in a data frame
  d.gov <- data.frame(pc=pc.seq, welfare=gov.welfare-min(gov.welfare), entity='government')
  
  owner.profit.plot <- ggplot(d.profit, aes(pc, welfare)) + 
         geom_line(aes(color=owner), show_guide = legend.status) + 
         geom_line(data=d.gov, mapping=aes(x=pc,y=welfare), size=1) +
         ggtitle(paste('Owner Profit and Govt Welfare (Shifted to min zero)\n',industry$name))
  
  ## I want, gov welfare, lobby welfare, all owners welfare, gov+lobby welfare
  
  # All owners profit, lobbyer profit, government welfare, and total welfare
  # Now, (after the prev agg) add in the lobby profits
  final.names <- c('pc','welfare','entity')
  
  # Start with the lobbyers
  lobby.agg <- aggregate(d.lobby.profit[,c('welfare')], by=list(pc=d.lobby.profit$pc),sum) 
  lobby.agg$entity <- 'lobbyers'
  names(lobby.agg) <- c('pc','welfare','entity')
  
  # Combine lobbyers and government
  lobby.gov <- rbind(lobby.agg, d.gov)
  lobby.gov.agg <- aggregate(lobby.gov$welfare, by=list(pc=lobby.gov$pc), sum)
  lobby.gov.agg$entity <- 'Gov and Lobbyers'
  names(lobby.gov.agg) <- final.names

  # Owner profits
  owner.agg <- aggregate(d.profit[,c('welfare')], by=list(pc=d.profit$pc),sum) 
  owner.agg$entity <- 'owners'
  names(owner.agg) <- final.names
  
  plot.agg <- rbind(lobby.agg, lobby.gov.agg, owner.agg, d.gov)
  agg.welfare.plot <- ggplot(plot.agg, aes(pc, welfare)) + geom_line(aes(color=entity), size=1) + 
          ggtitle(paste('Combined Welfare\n',industry$name))
  
  # price vs. pc
  price.pc.plot <- ggplot(data.frame(clearing.price=p.clear,pc=pc.seq), aes(pc,clearing.price)) + 
          geom_line(size=1) + ggtitle(paste('Clearing Price vs. Pc\n',industry$name))
          
  # emissions vs. pc
  emissions <- mapply(industry$emissions, p.clear, pc.seq)
  emissions.plot <- ggplot(data.frame(emissions=emissions,pc=pc.seq), aes(pc,emissions)) + 
         geom_line(size=1) + ggtitle(paste('Emissions vs. Pc\n',industry$name))
  
  # Revenue vs. pc
  revenue <- emissions*pc.seq
  revenue.plot <- ggplot(data.frame(revenue=revenue,pc=pc.seq), aes(pc,revenue)) + 
         geom_line(size=1) + ggtitle(paste('Revenue vs. Pc\n',industry$name))
         
  if (showPlots) {
    dev.new()
    print(supply.plot)
    dev.new()
    print(owner.profit.plot)
    dev.new()
    print(agg.welfare.plot)
    dev.new()
    print(price.pc.plot)
    dev.new()
    print(emissions.plot)
    dev.new()
    print(revenue.plot)
  }
  
  return(list(p.clear=p.clear, pc=pc.seq, gov.welfare=gov.welfare, owner.profit=d.profit,
         agg.owner.welfare=owner.agg, agg.lobby.welfare=lobby.agg, revenue=revenue, emissions=emissions,
         plots=list(owner.profit=owner.profit.plot,
                    supply=supply.plot,
                    agg.welfare=agg.welfare.plot,
                    emissions=emissions.plot,
                    price.pc=price.pc.plot,
                    revenue=revenue.plot)))
}
# ind <- industry[[1]]$industries[[4]]; lobbyers <- names(ind$owners)
# t <- makeDiagnosticPlots(ind, lobbyers, gov)

# Industry should be a list, with sectors as items, each sector containing at least one
# load duration group.
makeWeightedDiagnosticPlots <- function(sector, lobbyers, gov, pc.seq = seq(0, 100, length=200), 
                                        supply.pc.list=seq(0,100,20), showPlots=T) {
  price <- seq(1, 250, length=length(pc.seq))
  # Show plots using the weights for any sub-industries in sector
  
  w.p.clear <- sapply(pc.seq, sector$clearingPrices) # one column
  p.clear <- sapply(pc.seq, sector$clearingPrices) # one column per pc
  
  # Weighted Price vs. Pc
  d.price.pc <- data.frame(clearing.price=w.p.clear, pc=pc.seq)
  plot.price.pc <- ggplot(d.price.pc, aes(pc,clearing.price)) + 
          geom_line(size=1) + ggtitle(paste('Clearing Price vs. Pc\n',sector$name))
  
  # Total emissions vs. Pc
  emissions <- sapply(1:length(pc.seq), function(i) sector$totalValue(p.vect=p.clear[,i], pc=pc.seq[i], name='emissions'))
  d.emissions.pc <- data.frame(emissions=emissions, pc=pc.seq)
  plot.emissions.pc <- ggplot(d.emissions.pc, aes(pc, emissions)) + 
          geom_line(size=1) + ggtitle(paste('Total Emissions vs. Pc\n',sector$name))
  
  # Total Welfare vs. Pc
  welfare <- aggregateWelfare(sector, lobbyers, gov)
  agg.welfare <- sapply(pc.seq, welfare)
  d.welfare.pc <- data.frame(pc=pc.seq, welfare=agg.welfare, entity='Agg')
  
  # gov.welfare <- sapply(pc.seq, govWelfare(sector, gov))
  # d.welfare.pc <- rbind(d.welfare.pc, data.frame(pc=pc.seq, welfare=gov.welfare, entity='Gov'))
  
  # plot.agg.welfare.plot <- ggplot(d.welfare.pc, aes(pc, welfare)) + 
         # geom_line(aes(color=entity)) + 
         # ggtitle(paste('Aggregate and Government Welfare\n',sector$name))
  
  if (showPlots) {
    dev.new(); print(plot.price.pc)
    dev.new(); print(plot.emissions.pc)
    # dev.new(); print(plot.agg.welfare.pc)
  }

}
#makeWeightedDiagnosticPlots(wecc.industry, gov)

makeDisaggregatedDiagnosticPlots <- function(industry, gov, pc.seq = seq(0, 100, length=200), 
                                        supply.pc.list=seq(0,100,20), showPlots=T) {
  n <- length(industry$industries)
  price <- seq(1, 250, length=length(pc.seq))
  ind.names <- sapply(1:n, function(i) industry$industries[[i]]$name)
  if (length(unique(ind.names)) != n)
    ind.names <- paste(1:n, ind.names)
  
  # Show plots for each subindustry
  
  # Price vs. Pc for each industry
  p.clear <- sapply(pc.seq, industry$clearingPrices)
  d.price.pc <- data.frame()
  for (i in 1:n)
    d.price.pc <- rbind(d.price.pc, data.frame(clearing.price=p.clear[i,], pc=pc.seq, ind.name=ind.names[i]))
  
  plot.price.pc <- ggplot(d.price.pc, aes(pc,clearing.price,color=ind.name)) + 
          geom_line(size=1) + ggtitle(paste('Clearing Price vs. Pc\n',industry$name))
  
  # Emissions vs. Pc for each industry
  emissions <- sapply(1:length(pc.seq), function(x) industry$values(p.vect=p.clear[,x], pc=pc.seq[x], name='emissions'))
  d.emissions.pc <- data.frame()
  for (i in 1:n) {
    d.emissions.pc <- rbind(d.emissions.pc, data.frame(emissions=emissions[i,], pc=pc.seq, ind.name=ind.names[i]))
  }
  
  plot.emissions.pc <- ggplot(d.emissions.pc, aes(pc,emissions,color=ind.name)) + 
          geom_line(size=1) + ggtitle(paste('Emissions vs. Pc\n',industry$name))
  
  
  
  if (showPlots) {
    dev.new(); print(plot.price.pc)
    dev.new(); print(plot.emissions.pc)
  }

}
#industry <- wecc.industry; pc.seq = seq(0, 100, length=200)
#makeDisaggregatedDiagnosticPlots(wecc.industry, gov)


plotLoadDurationCurve <- function(loadCurve, load.groups, group.averages) {
  g <- load.groups
  xmin <- g[-length(g)]
  xmax <- g[-1]
  ymin <- rep(0, length(g)-1)
  ymax <- group.averages
  rect.data <- data.frame(xl=xmin,xu=xmax,yl=ymin,yu=ymax)
  
  tr <- seq(0,1,length=200)
  load.curve <- loadCurve(tr)
  d <- data.frame(tr=tr, load=load.curve)
  yrng <- range(load.curve)
  #b <- ggplot(d, aes(x = tr, y = load)) + geom_line() + geom_vline(xintercept = c(load.groups,1))
  b <- ggplot(d, aes(x = tr, y = load)) + geom_line(size=1) + 
       geom_rect(data=rect.data, mapping=aes(xmin=xl, xmax=xu, ymin=yl, ymax=yu, alpha=0.2), 
                 color='black', inherit.aes = FALSE) + guides(alpha=FALSE)
       #geom_line(data=rect.data, mapping=aes(x=xl,y=yu), size=1)
  b
}
#dev.new(); plotLoadDurationCurve(misoLDC, load.groups, group.averages)




## Generate a supply curve. Returns a list with Dispatch being the plants dataframe sorted by unit cost
## and Supply an interpolated supply function (last value held constant)
supplyCurve <- function(pc, plants) {
  # Order plant data by marginal cost
  marginal.cost <- plants$VarCosts + plants$CO2Intensity*pc
  marginal.order <- order(marginal.cost)
  dispatch.pd <- plants[marginal.order,]
  dispatch.pd$MC <- marginal.cost[marginal.order]
  dispatch.pd$Emissions <- dispatch.pd$CO2Intensity*dispatch.pd$MaxGeneration
  dispatch.pd$Supply <- cumsum(dispatch.pd$MaxGeneration)
  dispatch.pd$TotalEmissions <- cumsum(dispatch.pd$Emissions)
  supply.func <- approxfun(x=dispatch.pd$MC, y = dispatch.pd$Supply, method = "linear", rule = 2)
  emission.func <- approxfun(x=dispatch.pd$MC, y=dispatch.pd$TotalEmissions, method='linear', rule=2)
  # Format return object
  supply.list <- list(Dispatch = dispatch.pd, Supply = supply.func, TotalEmissions = emission.func)
  return(supply.list)
}

makeDispatchCurve <- function(pc, plant.data, data=F) {

  #scale_colour_manual(values = rhg_cols)

  # Make a dispatch curve for reference
  supply <- supplyCurve(pc, plant.data)$Dispatch
  
  supply$Fuel[supply$Fuel=='GEOTHERMAL'] <- 'OTHER RENEWABLE'
  supply$Fuel[supply$Fuel=='SOLAR'] <- 'OTHER RENEWABLE'
  supply$Fuel[supply$Fuel=='BIOMASS'] <- 'OTHER RENEWABLE'
  
  # Create short names
  supply$ShortFuelName <- substr(supply$Fuel, 1, 1)
  
  #Make a column showing the short name only when the value changes
  short <- rep('', nrow(supply))
  f <- supply$Fuel
  for (i in 2:length(short)) {
    if (f[i] == 'GAS' & f[i-1] == 'COAL' )
      short[i] <- '|'
    if (f[i] == 'COAL' & f[i-1] == 'GAS' )
      short[i] <- '|'
  }
  supply$SparseLabels <- short
  
  # changes <- supply$Fuel[-nrow(supply)] != supply$Fuel[-1]
  #supply[c(TRUE,changes[-length(changes)]), 'SparseLabels'] <- supply[changes, 'ShortFuelName']
  # supply[c(TRUE,changes[-length(changes)]), 'SparseLabels'] <- '|'
  # supply$SparseLabels[supply$Fuel!='GAS'] <- ''
  
  #supply$Fuel <- factor(supply$Fuel, ordered=TRUE,
  #               levels=c('COAL','GAS','OIL','NUCLEAR','HYDRO','WIND','BIOMASS','GEOTHERMAL','SOLAR'))
  supply$Fuel <- factor(supply$Fuel, ordered=TRUE,
                 levels=c('COAL','GAS','OIL','NUCLEAR','HYDRO','WIND','OTHER RENEWABLE'))
  fuel.cols <- c('black','yellow','grey','red','blue','lightblue','green')
  # Make supply give use an ordered factor for fuel type
  
  if (data) return(supply)
  
  p <- ggplot(supply, aes(Supply, MC)) + geom_line() + geom_point(aes(color=Fuel)) +
       geom_text(aes(label=SparseLabels, y=0), size=2) +
       #geom_text(aes(label=SparseLabels, y=MC, x=0, angle=90), size=2) +
       ylim(0, 300) + scale_colour_manual(values = fuel.cols) + ggtitle(paste('Dispatch Order Pc =',pc)) + 
       guides(color=guide_legend(override.aes=list(size=4)))
  return(p)
}



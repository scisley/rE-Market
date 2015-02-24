rm(list=ls(all=TRUE))
require(ggplot2)
require(ggthemes)

theme_set(theme_gdocs())
theme_update(plot.background = element_blank())
#theme_update(legend.key.width=unit(3,"lines"))

setwd("~/RAND/Thesis/Empirical/rE-market")


##### DEFINE FUNCTIONS #####

createPlantFunction <- function(plants, ramp=0.0000000001) {
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

ceDemand <- function(e, plants, ini.supply) {
  A <- 0
  supply <- createPlantFunction(plants)$supply # NO RAMP!
  
  # Find p that results in ini.supply (pc=0) MODIFY FOR REVENUE RECYCLING?
  ini.p <- uniroot(function(x) { supply(x,0)-ini.supply }, c(0, 400))$root
  A <- ini.supply/ini.p^e
  
  return( function(price) { A*price^e } )
}

##### ANALYSIS #####
ind.data <- data.frame(MaxGeneration=c(100, 50), VarCosts=c(0.5, 1), CO2Intensity=c(1,0.5))

plant.1 <- createPlantFunction(ind.data[1,])
plant.2 <- createPlantFunction(ind.data[2,])

demand <- ceDemand(e=-0.5, plants=ind.data, ini.supply=149.9)


supply <- function(p, pc) plant.1$supply(p,pc) + plant.2$supply(p,pc)

p.vect <- seq(0.5, 1.4, length=3000)
d <- data.frame(p = p.vect,
                demand = demand(p.vect), 
                supply = sapply(p.vect, supply, pc=0))

ggplot(d) + geom_line(aes(supply, p), size=1) + geom_line(aes(demand, p), size=1, color='blue')

# NOW, implement the algorithm I put on the white board. Use the uniroot command (see linle 69)


# ggplot(ds, aes(supply, price)) + geom_line(size=1, aes(color=pc, linetype=pc)) +
#   geom_line(data=dd[dd$demand<max(ds$supply),], size=1, mapping=aes(demand, price)) + 
#   ggtitle(paste('Supply and Demand for', ind.data$name)) + 
#   ylab('SRMC\n($/MWh)') + xlab('Supply (MWh)') + scale_colour_grey()

















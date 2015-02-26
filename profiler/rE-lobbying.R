
# All the lobby.names should be owners specified when the industry was created.
calcAggContributions1 <- function(industry, lobby.names, gov) {
  
  pc.minus <- list()
  eqbm.profits <- list()
  contributions <- list()
  other.data <- list()
  n <- length(industry$sectors)
  
  # Need the 'overall' equilibrium negotiated price to start with for each sector
  pc.eqbm <- aggNegotiate1(industry, lobby.names, gov)
  p.eqbm <- industry$clearingPrices(pc.eqbm)
  
  print(paste("Equilibrium Pc", pc.eqbm))
  
  # Equilibrium profits for each lobby
  eqbm.profits <- industry$profitByOwners(p.eqbm, pc.eqbm, lobby.names) 
  total.eqbm.profits <- sum(unlist(eqbm.profits))
  
  # Calculate pc.minus for each owner
  for(lobby in lobby.names) {
    sub.names <- lobby.names[lobby.names!=lobby]
    pc <- aggNegotiate1(industry, sub.names, gov)
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
    print(paste(lobby,"Contribution:",round(contributions[[lobby]]),"pc.minus",pc))
  }
  
  return(list(contributions = contributions,
              pc.minus = pc.minus,
              pc.eqbm = pc.eqbm,
              p.eqbm = p.eqbm,
              eqbm.profits = eqbm.profits,
              other.data = other.data))
  
}

# rough.pc determines the points where the welfare is initially evaluated. The highest point
# and its neighbors are then used as the starting point and limits for a Brent optimization.
# The default rough pc range should be fairly good for weights between 0 and 5. Those that
# go past 100 would go past 150. default is seq(0, 100, length=100)
aggNegotiate1 <- function(industry, lobby.names, gov) { 
  
  n <- length(industry$sectors)
  
  welfare <- function(pc, p.list) {
    # This line can take a while
    if (missing(p.list)) p.list <- industry$clearingPrices(pc)
    lobby.profit <- sum(unlist(industry$profitByOwners(p.list, pc, lobby.names)))
    lobby.profit + gov(pc, industry, p.list)
  }
  
  rough.welfare <- mapply(welfare, industry$pc.precalc, industry$p.precalc)
  rough.pc.max.i <- which.max(rough.welfare)
  rough.pc <- industry$pc.precalc
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








# All the lobby.names should be owners specified when the industry was created.
calcAggContributions <- function(industry, lobby.names, gov, rough.pc = seq(0, 250, length=250)) {

  pc.minus <- list()
  eqbm.profits <- list()
  contributions <- list()
  other.data <- list()
  n <- length(industry$sectors)
  
  # Need the 'overall' equilibrium negotiated price to start with for each sector
  pc.eqbm <- aggNegotiate(industry, lobby.names, gov, rough.pc)
  p.eqbm <- industry$clearingPrices(pc.eqbm)
  
  print(paste("Equilibrium Pc", pc.eqbm))

  # Equilibrium profits for each lobby
  eqbm.profits <- industry$profitByOwners(p.eqbm, pc.eqbm, lobby.names) 
  total.eqbm.profits <- sum(unlist(eqbm.profits))
  # Calculate pc.minus for each owner
  for(lobby in lobby.names) {
    sub.names <- lobby.names[lobby.names!=lobby]
    pc <- aggNegotiate(industry, sub.names, gov, rough.pc=rough.pc)
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
    print(paste(lobby,"Contribution:",round(contributions[[lobby]]),"pc.minus",pc))
  }
  
  return(list(contributions = contributions,
              pc.minus = pc.minus,
              pc.eqbm = pc.eqbm,
              p.eqbm = p.eqbm,
              eqbm.profits = eqbm.profits,
              other.data = other.data))
  
}

# rough.pc determines the points where the welfare is initially evaluated. The highest point
# and its neighbors are then used as the starting point and limits for a Brent optimization.
# The default rough pc range should be fairly good for weights between 0 and 5. Those that
# go past 100 would go past 150. default is seq(0, 100, length=100)
aggNegotiate <- function(industry, lobbyers, gov, rough.pc = seq(0, 250, length=250)) { 

  n <- length(industry$sectors)

  welfare <- function(pc) {
    # The first two lines take the bulk of the time.
    p.list <- industry$clearingPrices(pc)
    lobby.profit <- sum(unlist(industry$profitByOwners(p.list, pc, lobbyers)))
    lobby.profit + gov(pc, industry, p.list)
  }
  
  #rough.pc <- seq(search.range[1], search.range[2], length=ini.density) # Tried 1000, didn't improve results much
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
    s   <- industry$sectorSegmentSupply(p.list, pc)
    
    emissions <- industry$emissions(p.list, pc)
    revenue <- emissions*pc
    
    spike.loss <- calcSpikeLoss(p.0, p.list, s.0)
    production.loss <- calcProductionLoss(p.0, s, s.0)
    return(-scc*emissions + revenue.weight*revenue
           -spike.weight*spike.loss - production.weight*production.loss)
  }

}

calcProductionLoss <- function(p.0, s, s.0) {
  # Looks weird, for each sector i, find revenue lost by industry due to the carbon price
  # and add it all up. The interior sum is over load segments, the outer sum is over sectors.
  sum(sapply(1:length(p.0), function(i) sum(p.0[[i]]*(s.0[[i]]-s[[i]]))))
  #return(0)
}

calcSpikeLoss <- function(p.0, p, s.0) {
  # Looks weird, for each sector i, find extra money spent to buy original amount
  # and add it all up. The interior sum is over load segments, the outer sum is over sectors.
  sum(sapply(1:length(p.0), function(i) sum(s.0[[i]]*(p[[i]]-p.0[[i]]))))
}














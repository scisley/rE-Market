rm(list=ls(all=TRUE))
require(ggplot2)
require(ggthemes)
require(reshape)
require(parallel)
require(tgp)

# TODO:
#  * DONE. Make name, push to git. rE-Market
#  * DONE. Implement 4 component government welfare function
#  * Find somebody at NREL to talk with about who can profit from a carbon tax
#  * DONE. Test welfare function
#  * Examine how much nuclear and hydro can take advantage of market prices (IPP's?)
#  * Learn multi-core package
#  * Do large LHC experiment using AWS big computer
#  * Make a new dispatch curve with histograms/kernels underneath for fuel types for b/w readers

#  * Make a profit by plant map and export the data for viewing in javascript

# Remember traceback!  Helps with debugging!
theme_set(theme_gdocs())
theme_update(plot.background = element_blank()) #, text = element_text(size=14))
#theme_update(legend.key.width=unit(3,"lines"))

setwd("C:\\Users\\sisley\\Documents\\RAND\\Thesis\\Empirical\\rE-market")
source('rE-support.R')
source('rE-plots.R')
options(digits=10)

# Bucket to put the final file
s3.bucket <- 's3://isley-model-results/'

######################### ANALYSIS ###########################


#### Analysis using Fuel Type as owners #####
elasticity <- -1
pd <- getPlantData("C:\\Users\\sisley\\Documents\\RAND\\Thesis\\Empirical\\rE-market\\New_Plant_Data.csv")
owner.column <- 'FuelAgg'

if (is.factor(pd[[owner.column]])) {
  all.lobbies <- levels(pd[[owner.column]])
} else {
  all.lobbies <- unique(pd[owner.column])
}

all.lobbies <- all.lobbies[all.lobbies!='None']

all.lobbies <- c('COAL','GAS','OIL','NUCLEAR','HYDRO','WIND','OTHER RENEWABLE')
fuel.cols <- c('black','yellow','grey','red','blue','lightblue','green')

# Generate some load curves
load.data <- read.csv('Load_Curve_Data.csv', header=TRUE, na.strings = c("N/A","#N/A","?"), stringsAsFactors=FALSE)
# Splits the load curve up into 4 groupings, with small ranges on the ends
#load.groups <- c(0, seq(0.01,0.98,length=3), 1)
load.groups <- c(0, 1) # one load segment

industry <- makeNercIndustry(pd, load.data, load.groups, elasticity, ramp=3, owner.column=owner.column)
  
gov <- componentGov(weights=c(50,1,0,0))

#profitByOwnerPlot(industry, all.lobbies) + scale_color_manual(values=fuel.cols)

# Time how long it takes to compute contributions
system.time( cont.pac.ramp5 <- calcAggContributions(industry, all.lobbies, gov) )


#### Analysis using Owners ####

elasticity <- -1
pd <- getPlantData("New_Plant_Data.csv")

# Exclude from lobbies the trade groups
all.lobbies <- exclude(unique(pd$ParentPAC), c('None','American Public Power Assn','Edison Electric Institute'))

# Generate some load curves
load.data <- read.csv('Load_Curve_Data.csv', header=TRUE, na.strings = c("N/A","#N/A","?"), stringsAsFactors=FALSE)
# Splits the load curve up into 4 groupings, with small ranges on the ends
#load.groups <- c(0, seq(0.01,0.98,length=3), 1)
load.groups <- c(0, 1) # one load segment

industry <- makeNercIndustry(pd, load.data, load.groups, elasticity, ramp=3, owner.column='ParentPAC')

gov <- componentGov(weights=c(50,1,1,1))

#profitByOwnerPlot(industry, all.lobbies)

# Time how long it takes to compute contributions
system.time( cont.pac.ramp5 <- calcAggContributions(industry, all.lobbies, gov) )


#### Generate the Latin Hypercube sample

{ ## Create the Latin hypercube experimental design and save it to a file.
  num.samples <- 4000
  ranges <- data.frame(min=c(0, 0, 0, 0, -1), max=c(75, 1.5, 1.5, 1.5, -0.1))
  sample <- as.data.frame(lhs(num.samples, as.matrix(ranges)))
  names(sample) <- c('alpha.scc', 'alpha.spike', 'alpha.revenue', 'alpha.production', 'elasticity')
  save(sample, file='eaLhc.RData')
}

# Prepare for running the LHC sample
load(file='eaLhc.RData')
pd <- getPlantData("New_Plant_Data.csv")
# Exclude from lobbies the trade groups
all.lobbies <- exclude(unique(pd$ParentPAC), c('None','American Public Power Assn','Edison Electric Institute'))
# Generate some load curves
load.data <- read.csv('Load_Curve_Data.csv', header=TRUE, na.strings = c("N/A","#N/A","?"), stringsAsFactors=FALSE)
# Splits the load curve up into 4 groupings, with small ranges on the ends
load.groups <- c(0, seq(0.01,0.98,length=3), 1)
#load.groups <- c(0, 1) # one load segment

# Define the function to run for each row of the LHC
run <- function(alpha.scc, alpha.spike, alpha.revenue, alpha.production, elasticity, i) {
  
  industry <- makeNercIndustry(pd, load.data, load.groups, elasticity, ramp=0.5, owner.column='ParentPAC')
  gov <- componentGov(weights=c(alpha.scc, alpha.spike, alpha.revenue, alpha.production))
  
  system.time( cont.x <- calcAggContributions(industry, all.lobbies, gov) )
  
  list(cont=cont.x, alpha.scc=alpha.scc, alpha.spike=alpha.spike, 
       alpha.revenue=alpha.revenue, alpha.production=alpha.production, 
       elasticity=elasticity, i=i)
}
# system.time( ans.1 <- run(50, 1, 1, 1, -0.5, 1) )
# 1 load group, ramp=5, 4.86 m
# 1 load group, ramp=3, 4.72 m
# 5 load groups, ramp=0.5, 16.1 m but several had very negative contributions
#"Equilibrium Pc 17.2936151161634"
#[1] "Southern Co Contribution: 24102007.8825073 pc.minus 16.2334838107649"
#                                6895865.38476181

# Simple function to checkpoint data out to S3 storage
save_data <- function(ans, i) {
  file.name <- paste('lhcResults',i,'.RData',sep='')
  save(ans, sample, file=file.name)
  system(paste('aws s3 cp', file.name, s3.bucket), wait=TRUE)
}

# Perform the experiment, but do it in chunks so we can checkpoint every 200 runs
for (count in 1:20) {
  end.i <- count*200
  start.i <- end.i-200+1
  #results <- mclapply(start.i:end.i, function(x) { CHANGE LOAD!
  results <- mclapply(1:4, function(i) {
    result <- run(sample[i,'alpha.scc'], sample[i,'alpha.spike'], sample[i,'alpha.revenue'],
                  sample[i,'alpha.production'], sample[i,'elasticity'], i)
    print(paste('Finished with run',i))
    return(result)
  }, mc.cores = 32)
  
  save_data(results, count)
}

# DONE!


















#### Diagnostic plots for Government Welfare #####
pc.seq <- seq(0,100,length=50)
industry <- makeNercIndustry(pd, load.data, load.groups, elasticity=-1, ramp=3, owner.column=owner.column)

makeIndustryDiagnosticPlots(industry, gov, all.lobbies, pc.seq=pc.seq)

# Return a list of government welfare for a supplied pc.seq and industry structure
govWelfareVector <- function(industry, gov, pc.seq) {
  p.clear <- lapply(pc.seq, industry[['clearingPrices']])
  
  # Government welfare
  gov.welfare <- mapply(function(x,y) gov(x, industry, y), pc.seq, p.clear)
  #gov.welfare <- gov.welfare - min(gov.welfare) # Shift to min zero
  return(gov.welfare)
}

# weights in componentGov go as: scc, spike, revenue, production
components <-            c('SCC',              'Spike',              'Revenue',           'Production');
component.values <- list(c(25, 50, 75, 150), c(0.5,0.75,1.0,1.25), c(0.5,0.75,1.0,1.25), c(0.5,0.75,1.0,1.25))

# This loops through the components and the values I want to test above and makes a plot showing how the
# government welfare changes for the different specifications as pc changes. This should highlight any
# blatant errrors. Note that the industry elasticity is important, as well as the default values
# used for the other components.
default.weights <- c(25, 1, 1, 1)
for (i in 1:4) {
  
  gov.results <- as.data.frame(lapply(component.values[[i]], function(x) {
    weights <- default.weights
    weights[i] <- x;
    gov <- componentGov(weights)
    return(govWelfareVector(industry, gov, pc.seq=pc.seq))
  }))
  
  colnames(gov.results) <- component.values[[i]]
  gov.results$pc <- pc.seq
  
  gov.results <- melt(gov.results, id=c("pc"), variable_name=components[i]) 
  
  print(ggplot(gov.results, aes(x=pc, y=value)) + geom_line(size=1, aes_string(color=components[i])) + scale_color_grey() +
    ggtitle(paste('Government Welfare for various',components[i])))
}



### Debugging code

{ # This debugging code was used to look at different ramp rates and rough.pc ranges and densities
  # I've settled on ramp=3 rather than 5, and 100 points going from 0-150
  
  pd <- getPlantData("New_Plant_Data.csv")
  pc.seq <- seq(0,100,length=50)
  all.lobbies <- exclude(unique(pd$ParentPAC), c('None','American Public Power Assn','Edison Electric Institute'))
  load.data <- read.csv('Load_Curve_Data.csv', header=TRUE, na.strings = c("N/A","#N/A","?"), stringsAsFactors=FALSE)
  load.groups <- c(0, 1) # one load segment
  
  industry <- makeNercIndustry(pd, load.data, load.groups, elasticity=-0.8, ramp=0.5, owner.column='ParentPAC')
  gov <- componentGov(weights=c(50,1,1,1))
  
  n <- length(industry$sectors)
  
  welfare <- function(pc) {
    # The first two lines take the bulk of the time.
    p.list <- industry$clearingPrices(pc)
    lobby.profit <- sum(unlist(industry$profitByOwners(p.list, pc, all.lobbies)))
    lobby.profit + gov(pc, industry, p.list)
  }
  
  # I've hard coded in an upper bound of 50. EXAMINE THIS DECISION AGAIN!
  eps <- 0
  max.pc <- 150
  rough.pc <- seq(eps, max.pc-eps, length=100) # Tried 1000, didn't improve results much
  rough.welfare <- sapply(rough.pc, welfare)
  plot(rough.pc, rough.welfare)
  
  rough.pc.max.i <- which.max(rough.welfare)
  lbound <- rough.pc[max(1, rough.pc.max.i-1)]
  ubound <- rough.pc[min(length(rough.pc), rough.pc.max.i+1)]
  init.val <- mean(c(lbound, ubound))
  
  lobbied.pc <- optim(init.val, welfare, 
                      method='Brent', lower=lbound, upper=ubound, 
                      control=list(fnscale=-1, reltol=1e-12))
  abline(v=lobbied.pc$par)
  lobbied.pc$par
  
}

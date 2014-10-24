rm(list=ls(all=TRUE))
require(ggplot2)
require(ggthemes)
require(reshape)

# TODO:
#  * DONE. Make name, push to git. rE-Market
#  * DONE. Implement 4 component government welfare function
#  * Find somebody at NREL to talk with about who can profit from a carbon tax
#  * DONE. Test welfare function
#  * Examine how much nuclear and hydro can take advantage of market prices (IPP's?)
#  * Learn multi-core package
#  * Do large LHC experiment using AWS big computer


#  * Make a profit by plant map and export the data for viewing in javascript

# Remember traceback!  Helps with debugging!
theme_set(theme_gdocs())
theme_update(plot.background = element_blank()) #, text = element_text(size=14))
#theme_update(legend.key.width=unit(3,"lines"))

setwd("C:\\Users\\sisley\\Documents\\RAND\\Thesis\\Empirical\\rE-market")
source('rE-support.R')
source('rE-plots.R')
options(digits=10)
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

industry <- makeNercIndustry(pd, load.data, elasticity, owner.column=owner.column)
gov <- componentGov(weights=c(50,1,0,0))

#profitByOwnerPlot(industry, all.lobbies) + scale_color_manual(values=fuel.cols)

# Time how long it takes to compute contributions
system.time( cont.pac.ramp5 <- calcAggContributions(industry, all.lobbies, gov) )


#### Analysis using Owners ####

elasticity <- -1
pd <- getPlantData("C:\\Users\\sisley\\Documents\\RAND\\Thesis\\Empirical\\rE-market\\New_Plant_Data.csv")

# Exclude from lobbies the trade groups
all.lobbies <- exclude(unique(pd$ParentPAC), c('None','American Public Power Assn','Edison Electric Institute'))

# Generate some load curves
load.data <- read.csv('Load_Curve_Data.csv', header=TRUE, na.strings = c("N/A","#N/A","?"), stringsAsFactors=FALSE)
# Splits the load curve up into 4 groupings, with small ranges on the ends
#load.groups <- c(0, seq(0.01,0.98,length=3), 1)
load.groups <- c(0, 1) # one load segment

industry <- makeNercIndustry(pd, load.data, elasticity, owner.column=owner.column)
gov <- componentGov(weights=c(50,1,1,1))

#profitByOwnerPlot(industry, all.lobbies)

# Time how long it takes to compute contributions
system.time( cont.pac.ramp5 <- calcAggContributions(industry, all.lobbies, gov) )








#### Is the government welfare function working? #####
pc.seq <- seq(0,100,length=50)
industry <- makeNercIndustry(pd, load.data, elasticity=-1, owner.column=owner.column)

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
#p.clear <- lapply(pc.seq, industry$clearingPrices)
#outcome <- mapply(function(x,y) as.numeric(industry$sectorSegmentSupply(x,y)), p.clear, pc.seq)
#
#sum(sapply(1:length(p.0), function(i) 
#              sum(p.0[[i]]*
#              (industry$no.tax.values$supply[[i]]-industry$sectorSegmentSupply(p.clear[[50]], pc=100)[[i]]))))






# Splits the load curve up into 4 groupings, with small ranges on the ends
#load.groups <- c(0, seq(0.01,0.98,length=3), 1)
load.groups <- c(0, 1) # one load segment
t.weights <- load.groups[-1]-load.groups[-length(load.groups)]


all.region <- unique(pd$NERC)
all.region <- all.region[all.region != 'ASCC' & all.region !='HICC']
pd.region <- list()
loadCurve <- list()
region.names <- list()
generation <- list()
averages <- list()
industry <- list()
for (region in all.region) {
  pd.region[[region]] <- subset(pd, NERC==region)
  generation[[region]] <- sum(pd.region[[region]]$NetGeneration)
  loadCurve[[region]] <- generateLoadDurationCurve(subset(load.data, Region=='AVG'), generation[[region]], check.plots=F)
  
  averages[[region]] <- findAverageLoads(loadCurve[[region]], load.groups)
  region.names[[region]] <- paste(region, load.groups[-length(load.groups)], load.groups[-1])
  dev.new(); print(plotLoadDurationCurve(loadCurve[[region]], load.groups, averages[[region]]) + ggtitle(paste("Load Duration Curve",region)))
  
  sectors <- lapply(1:length(t.weights), function(i) {
    sub.demand <- ceDemand(e=-0.2, pd.region[[region]], ini.supply=averages[[region]][i])
    makeLoadSegment(pd.region[[region]][owner.column], pd.region[[region]], sub.demand, 
                    name=region.names[[region]][i], ramp=5 )
  })
  industry[[region]] <- assembleSector(sectors, t.weights)
}

gov <- componentGov(weights=c(1,1,1,1))
system.time( cont.pac.ramp5 <- calcAggContributions(industry, all.lobbies, gov) )

write.table(unlist(cont.pac.ramp5$contributions), "clipboard", sep="\t")
unlist(cont.pac.ramp5$contributions)
cont.pac.ramp5$pc.eqbm 

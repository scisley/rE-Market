rm(list=ls(all=TRUE))
require(ggplot2)
require(ggthemes)
require(reshape)

# TODO:
#  * Make name, push to git. rE-Market
#  * Implement 4 component government welfare function
#  * Test welfare function
#  * Examine how much nuclear and hydro can take advantage of market prices (IPP's?)
#  * Learn multi-core package
#  * Do large LHC experiment using AWS big computer


# Remember traceback!  Helps with debugging!
theme_set(theme_gdocs())
theme_update(plot.background = element_blank()) #, text = element_text(size=14))
#theme_update(legend.key.width=unit(3,"lines"))

# The DOE produced is scaled from 0 to 1, so first I need to scale those to my actual
# parameter ranges
setwd("C:\\Users\\sisley\\Documents\\RAND\\Thesis\\Empirical\\Analysis")
source('EmpiricalAnalysisFunctions5.R')
source('EmpiricalAnalysisPlots5.R')
options(digits=10)
######################### ANALYSIS ###########################

elasticity <- -1
pd <- getPlantData("C:\\Users\\sisley\\Documents\\RAND\\Thesis\\Empirical\\Analysis\\New_Plant_Data.csv")
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

profitByOwnerPlot(industry, all.lobbies) + scale_color_manual(values=fuel.cols)

system.time( cont.pac.ramp5 <- calcAggContributions(industry, all.lobbies, gov) )
makeIndustryDiagnosticPlots(industry, gov, all.lobbies)














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

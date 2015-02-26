
#########################################################################################
# A simple function I can profile using lineprof
#########################################################################################
go <- function() {
  pd <- getPlantData("New_Plant_Data.csv")
  industry <- makeNercIndustry(pd=pd, 
                               load.data=read.csv('Load_Curve_Data.csv', header=TRUE, na.strings = c("N/A","#N/A","?"), stringsAsFactors=FALSE),
                               load.groups=c(0, seq(0.01,0.98,length=3), 1),
                               elasticity=-0.5,
                               ramp=0.5,
                               owner.column='Fuel',
                               pc.precalc=seq(0, 150, 1))
  
  cont.x <- calcAggContributions1(industry, 
                                  lobby.names=unique(pd$Fuel), 
                                  gov=componentGov(weights=c(50,1,1,0)))
}
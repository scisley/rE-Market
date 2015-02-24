rm(list=ls(all=TRUE))
require(ggplot2)
require(ggthemes)
require(grid)
require(reshape)
library(scales)

###$ TRY REVENUE AND COMPARE TO ACTUAL EIA DATA!.
# Remember traceback!  Helps with debugging!

# The DOE produced is scaled from 0 to 1, so first I need to scale those to my actual
# parameter ranges
setwd("~/RAND/Thesis/Empirical/rE-market")
source('rE-support.R')
source('rE-plots.R')
options(digits=10)
theme_set(theme_gdocs())
theme_update(plot.background = element_blank())
te <- element_text(family="sans", face="plain", colour="black", size=16, 
                    hjust=0.5, vjust=0.5, angle=0, lineheight=0.9)
theme_update(legend.key.width=unit(3,"lines"), 
             text=te, axis.title=te)


pd <- getPlantData("New_Plant_Data.csv")

owner.column <- 'ParentPAC'
all.lobbies <- unique(pd[owner.column])
all.lobbies <- all.lobbies[all.lobbies!='None']
all.lobbies <- all.lobbies[order(all.lobbies)]
# Generate some load curves
load.data <- read.csv('Load_Curve_Data.csv', header=TRUE, na.strings = c("N/A","#N/A","?"), stringsAsFactors=FALSE)
fuel.cols <- c('black','yellow','grey','red','blue','lightblue','green')

# Make a new fuel type that lumps solar and geothermal, and biomass
pd$FuelAgg <- pd$Fuel
pd$FuelAgg[pd$FuelAgg=='SOLAR'] <- "OTHER RENEWABLE"
pd$FuelAgg[pd$FuelAgg=='GEOTHERMAL'] <- "OTHER RENEWABLE"
pd$FuelAgg[pd$FuelAgg=='BIOMASS'] <- "OTHER RENEWABLE"

#### CODE TO REPRODUCE IMAGES FROM THE PAPER #####


{ # FIGURE 1
  pd1 <- pd
  pd1$MaxGeneration <- pd$MaxGeneration/10^9
  pc0 <- makeDispatchCurve(pc=0, plant.data=pd1, data=T)
  pc50 <- makeDispatchCurve(pc=50, plant.data=pd1, data=T)
  
  ggplot(pc0, aes(Supply, MC)) + geom_line() + 
    geom_point(aes(color=Fuel), size=3) +
    geom_point(data=pc50, aes(color=Fuel), size=3) +
    geom_text(aes(label=SparseLabels, y=0), size=2) +
    geom_text(data=pc50, aes(label=SparseLabels, y=200), size=2) +
    ylim(0, 200) + scale_colour_manual(values = fuel.cols) + 
    xlab('Supply (Billions of MWh)') + ylab('Short Run\nMarginal Cost\n($/MWh)') + 
    guides(color=guide_legend(override.aes=list(size=5))) +
    theme(legend.position=c(0.25,0.78))
  ggsave(file=".\\plots\\DispatchCurvePc0and50.png", width=10, height=8) 
  
  ### Not in the paper, but useful plot. Shows profit for two different supply curves.
  average <- 4
  demand <- ceDemand(e=-0.5, pd, ini.supply=4e9)
  p <- seq(10, 200, length=100)
  d <- data.frame(p=p, d=sapply(p, demand))
  
  # Find roughly where it intersects the two supply curves
  i <- 2575
  poly0 <- data.frame(Supply=c(pc0[1:i,'Supply'],0), MC=c(pc0[1:i,'MC'],pc0[i,'MC']))
  
  i.1 <- 2810
  poly1 <- data.frame(Supply=c(pc50[1:i.1,'Supply'],0), MC=c(pc50[1:i.1,'MC'],pc50[i.1,'MC']))
  
  ggplot(pc0, aes(Supply, MC)) + geom_line() + 
    geom_point(aes(color=Fuel)) +
    geom_point(data=pc50, aes(color=Fuel)) +
    #geom_text(aes(label=SparseLabels, y=0), size=2) +
    #geom_text(data=pc50, aes(label=SparseLabels, y=200), size=2) +
    ylim(0, 200) + scale_colour_manual(values = fuel.cols) + 
    
    geom_line(data=d, aes(x=d/10^9, y=p), size=1) +
    geom_polygon(data=poly0, fill='red', alpha=0.5) +
    geom_polygon(data=poly1, fill='blue', alpha=0.5) +
    xlab('Supply (Billions of MWh)') + ylab('Short Run\nMarginal Cost\n($/MWh)') + 
    theme(text = element_text(size=16), legend.position='none')
  ggsave(file=".\\plots\\Seminar-DispatchCurvePc0and50withProfitBlobs.png", width=10, height=6) 
  
}

##### STILL TO BE CHECKED ######


{## Dispatch Curve Plots

pd1 <- pd
pd1$MaxGeneration <- pd$MaxGeneration/10^9
dev.new(width=1000, height= 600); makeDispatchCurve(pc=0, pd1) + xlab('Supply (Billions of MWh)') + ylab('Short Run\nMarginal Cost\n($/MWh)') + 
           ggtitle('') + theme(text = element_text(size=14))
# ggsave(file=".\\plots\\DispatchCurvePc0.png") 

pc0 <- makeDispatchCurve(pc=0, plant.data=pd1, data=T)

makeDispatchCurve(pc=50, pd1) + xlab('Supply (Billions of MWh)') + ylab('Short Run\nMarginal Cost\n($/MWh)') + 
           ggtitle('') + theme(text = element_text(size=14))
ggsave(file=".\\plots\\DispatchCurvePc50.png") 
# Dispatch curves by region:

nerc <- exclude(unique(pd$NERC), c('HICC'))
d <- data.frame()
for (region in nerc) {
  plants <- subset(pd, NERC==region)
  s <- makeDispatchCurve(pc=0, plants, data=T)
  s$Region <- region
  d <- rbind(d, s)
}

# Organize by  MAKE SURE TO RE-RUN THE ABOVE CODE!
d$Region <- factor(d$Region, ordered=TRUE,
               levels=c('WECC','MRO','NPCC','SPP','ASCC','RFC','TRE','SERC','FRCC'))
d$Supply <- d$Supply/10^6
dev.new(width=600, height=400) 
ggplot(d, aes(Supply, MC)) + geom_line() + geom_point(aes(color=Fuel)) +
       ylim(0, 300) + scale_colour_manual(values = fuel.cols) + 
       facet_wrap(~Region, ncol=3, scales='free_x') +
       xlab('Supply (Millions of MWh)') +
       theme(strip.text.x = element_blank(), strip.background=element_blank(), 
       panel.grid.major.x = element_blank(),
       panel.grid.major.y = element_blank()) +
       guides(color=guide_legend(override.aes=list(size=4))) + 
       theme(text = element_text(size=14))

# ggsave(file=".\\plots\\FacetedDispatchCurves.png") 

## Put all regions on the same plot, and scale the data to have max capacity of 1

nerc <- exclude(unique(pd$NERC), c('HICC', 'ASCC'))
d <- data.frame()
for (region in nerc) {
  plants <- subset(pd, NERC==region)
  s <- makeDispatchCurve(pc=0, plants, data=T)
  s$Region <- region
  s$ScaledSupply <- s$Supply/max(s$Supply)
  d <- rbind(d, s)
}



d$Region <- factor(d$Region, ordered=TRUE,
               levels=c('WECC','MRO','NPCC','SPP','RFC','TRE','SERC','FRCC'))
d$Supply1 <- d$Supply/10^6

# Find point of maximum supply for each region, this is about where the text label will go
max.supply <- by(subset(d, MC<300), subset(d, MC<300)$Region, function(x) x[which.max(x$Supply),] )
d.max <- as.data.frame(t(sapply(max.supply, function(x) c(x$Supply1, x$MC))))
d.max$Region <- rownames(d.max)
names(d.max) <- c('Supply','MC','Region')
d.max$Supply <- d.max$Supply + 10
d.max[d.max$Region=='MRO','Supply'] <- d.max[d.max$Region=='MRO','Supply'] - 200
d.max[d.max$Region=='SERC','Supply'] <- d.max[d.max$Region=='SERC','Supply'] - 250

dev.new(width=600, height=400)
ggplot(d, aes(Supply1, MC)) + geom_line(aes(group=Region)) + geom_point(aes(color=CO2Intensity), size=2, alpha=1) +
       scale_color_gradient(limits=c(0, 1), low='blue', high='black', na.value='black') +
       ylim(0,300) +
       xlab('Supply (Millions of MWh)') +
       ylab('Short Run\nMarginal Cost\n($/MWh)') +
       theme(strip.text.x = element_blank(), strip.background=element_blank(), 
       panel.grid.major.x = element_blank(),
       panel.grid.major.y = element_blank()) +
       guides(color=guide_legend(title=expression(CO[2]~Intensity), override.aes=list(size=4, alpha=1))) + 
       theme(text = element_text(size=14)) +
       annotate('text', x=d.max$Supply, y=d.max$MC, label=d.max$Region, hjust=0)
       
# ggsave(file=".\\plots\\CombinedDispatchCurves.png") 





}

{ ## Multiple carbon prices and demand curves

elasticity <- -0.2
ramp <- 0.01

generation <- sum(pd$NetGeneration)
loadCurve <- generateLoadDurationCurve(subset(load.data, Region=='AVG'), generation, check.plots=F)
average <- findAverageLoads(loadCurve, c(0,1))
demand <- ceDemand(e=elasticity, pd, ini.supply=average)
segment <- makeLoadSegment(pd$ParentPAC, pd, demand, 
                    name='One Segment', ramp=ramp)
sector <- assembleSector(list(segment), t.weights=1, name='single sector')
industry <- assembleIndustry(list(sector), name='Entire US')

ind1 <- assembleIndustry(list(assembleSector(list(makeLoadSegment(pd$ParentPAC, pd, ceDemand(e=-0.9, pd, ini.supply=average))), t.weights=1)))
dem1 <- makeSupplyDemandPlot(ind1, data=T)$demand
dem1 <- subset(dem1, demand<8e9)

# 2 demand curves, a range of supply curves for differnt pc values
dev.new(width=800, height=600)
makeSupplyDemandPlot(industry, supply.pc.list=c(0, 20, 50, 100, 200, 300), data=F) +
  ggtitle('') + guides(linetype=guide_legend(title="Carbon Tax"), color=FALSE) +
  geom_line(data=dem1, aes(x=demand, y=price), size=1) + theme(text = element_text(size=14)) +
  annotate("text", x = 3.1e9, y = 250, label = "e=-0.2") +
  annotate("text", x = 1.1e9, y = 250, label = "e=-0.9")
# ggsave(file=".\\plots\\SupplyAndDemand.png") 



# Single demand curve and supply curve
dev.new(width=800, height=600)
ind1 <- assembleIndustry(list(assembleSector(list(makeLoadSegment(pd$ParentPAC, pd, ceDemand(e=-0.2, pd, util=0.4))), t.weights=1)))
makeSupplyDemandPlot(ind1, supply.pc.list=c(0), data=F) +
  ggtitle('') + guides(linetype=guide_legend(title="Carbon Tax"), color=FALSE) +
  geom_line(data=dem1, aes(x=demand, y=price), size=1) + theme(text = element_text(size=14))
# ggsave(file=".\\plots\\SupplyAndDemand-single_e_02,1.png") 


dev.new(width=800, height=600)
makeSupplyDemandPlot(industry, supply.pc.list=c(0, 20, 50, 100, 200, 300), data=F) +
  ggtitle('') + guides(linetype=guide_legend(title="Carbon Tax"), color=FALSE) +
  theme(text = element_text(size=14))
# ggsave(file=".\\plots\\SupplyAndDemand1.png") 


}

{ ## Market clearing price as a function of pc and elasticity


## First, single market, single load segment
elasticities <- c(0, -0.2, -0.5, -0.9) 
pc.seq <- seq(0, 200, length=200)
d <- data.frame()
for (elasticity in elasticities) {
  industry <- makeSingleIndustry(pd, load.data, elasticity, ramp=0.01, load.groups=c(0,1))
  p.clear <- unlist(lapply(pc.seq, industry[['clearingPrices']]))
  d <- rbind(d, data.frame(pc=pc.seq, p=p.clear, Elasticity=elasticity))
}
d$Elasticity <- factor(d$Elasticity, ordered=TRUE,
                levels=elasticities)
dev.new(width=800, height=600)
ggplot(d, aes(x=pc,y=p)) + geom_line(aes(linetype=Elasticity), size=1) +
  ggtitle('') + ylab('Clearing\nPrice\n($/MWh)') + xlab('Carbon Tax ($/tCO2)') +
  scale_linetype_manual(values = 1:length(elasticities)) +
  theme(text = element_text(size=14))
# ggsave(file=".\\plots\\PriceVsPcAndElasticityNew.png") 

## Next, all NERC segments, many load segments
dd <- data.frame()
elasticities <- c(0, -0.2, -0.5, -0.9) 
for (elasticity in elasticities) {
  ramp <- 0.01
  load.groups <- c(0, 0.01, seq(0.05, 0.95, length=10), 1)
  industry <- makeNercIndustry(pd, load.data, elasticity, ramp, load.groups)
  
  p.clear <- lapply(pc.seq, industry$clearingPrices)
  
  p.clear.avg <- sapply(1:length(pc.seq), function(t) industry$clearingPrice(p.clear[[t]],pc.seq[t]))
  dd <- rbind(dd, data.frame(pc=pc.seq, p=p.clear.avg, Elasticity=elasticity))
}

dd$Elasticity <- factor(dd$Elasticity, ordered=TRUE,
                levels=elasticities)
                
dev.new(width=800, height=600)
ggplot(dd, aes(x=pc,y=p)) + geom_line(aes(linetype=Elasticity), size=1) +
  ggtitle('') + ylab('Clearing\nPrice\n($/MWh)') + xlab('Carbon Tax ($/tCO2)') +
  theme(text = element_text(size=14)) +
  scale_linetype_manual(values = c(1:length(elasticities)))
# ggsave(file=".\\plots\\PriceVsPcAndElasticityNERC.png") 

dd.1 <- dd
dd.1$Set <- 'NERC'
d.1 <- d
d.1$Set <- 'Single'
ddd <- rbind( dd.1, d.1)
ddd$group <- paste(ddd$Set, ddd$Elasticity)
ddd$group <- factor(ddd$group, ordered=TRUE,
                levels=c('NERC 0','NERC -0.2','NERC -0.5','NERC -0.9', 'Single 0','Single -0.2','Single -0.5','Single -0.9'))
dev.new(width=800, height=600)
ggplot(ddd, aes(x=pc,y=p)) + geom_line(aes(linetype=group, color=Set, size=Set)) +
  ggtitle('') + ylab('Clearing\nPrice\n($/MWh)') + xlab('Carbon Tax ($/tCO2)') +
  scale_colour_manual(values = c("black","lightgrey")) +
  theme(text = element_text(size=14)) +
  scale_linetype_manual(values = c(1:4,1:4)) +
  scale_size_manual(values = c(1,1))
# ggsave(file=".\\plots\\PriceVsPcAndElasticityNERCandSingleMarket.png") 

}

{ ## Industry Profit

# Illustrate a industry profit can increase in inelastic markets, or even slightly elastic markets

## Next, all NERC segments, many load segments
d <- data.frame()
pc.seq <- seq(0, 100, length=100)
elasticities <- c(-0.2, -0.5, -0.9, -1.5) 
for (elasticity in elasticities) {
  print(elasticity)
  load.groups <- c(0, 0.01, seq(0.05, 0.95, length=10), 1)
  #load.groups <- c(0, 1)
  industry <- makeNercIndustry(pd, load.data, elasticity, ramp=0.01, load.groups)
  
  p.clear <- lapply(pc.seq, industry$clearingPrices)
  dd <- makeStandardPlot(industry, type='profit', pc.seq=pc.seq, p.clear=p.clear, plot=F)
  dd$Elasticity <- elasticity
  d <- rbind(d, dd)
}
names(d) <- c('pc','profit','Elasticity')

# Get labels and positions
labs <- d[d$pc==100,]

d$Elasticity <- factor(d$Elasticity, ordered=TRUE,
                levels=elasticities)
d$profit <- d$profit/10^9
dev.new(width=800, height=600)
ggplot(d, aes(x=pc,y=profit)) + geom_line(aes(linetype=Elasticity), size=1) +
  ggtitle('') + ylab('Profit\n($B)') + xlab(expression(paste('Carbon Tax ($/')~tCO[2]~')')) +
  scale_linetype_manual(values = 1:length(elasticities)) +
  theme(text = element_text(size=14))  #+
  #annotate("text", x = 100, y = labs$profit, label = paste("e= ",labs$Elasticity,sep=''), vjust =0, hjust=1)
# ggsave(file=".\\plots\\TotalProfitVsPcAndElasticityNERC.png") 

}

{ ## Profit by Fuel type
nice.names <- c('Coal','Gas','Oil','Nuclear','Hydro','Wind','Other\nRenewable')
elasticities <- c(-0.2,-0.9) 
d <- data.frame()
for (elasticity in elasticities) {
  print(elasticity)
  load.groups <- c(0, 0.01, seq(0.05, 0.95, length=10), 1)
  #load.groups <- c(0, 1)
  industry <- makeNercIndustry(pd, load.data, elasticity=elasticity, ramp=0.01, load.groups, owner.column='FuelAgg')
  dd <- profitByOwnerPlot(industry, unique(pd$FuelAgg), pc.seq=seq(0, 50, length=200), p.clear=NULL, plot=F)
  dd$Elasticity <- elasticity
  d <- rbind(d,dd)
}
d$Elasticity <- factor(d$Elasticity, ordered=TRUE,
                levels=elasticities)
d$profit <- d$profit/10^9
d$owner <- factor(d$owner, ordered=TRUE,
                 levels=c('COAL','GAS','OIL','NUCLEAR','HYDRO','WIND','OTHER RENEWABLE'))

nd <- data.frame(pc=c(0,0),
                profit=c(35, 35),
                Elasticity= elasticities,                     
                Text=paste("Elasticity =", elasticities))  

dev.new(width=800, height=600)
ggplot(d, aes(pc, profit)) + geom_line(size=1, aes(color=owner,linetype=owner)) + 
  ylab('Profit\n($B)') + scale_colour_manual(values = fuel.cols, labels=nice.names) +
  xlab(expression(paste('Carbon Tax ($/')~tCO[2]~')')) +
  scale_linetype_manual(values = 1:length(fuel.cols), labels=nice.names) +
  theme(legend.title=element_blank()) +
  theme(text = element_text(size=14)) +
  facet_wrap(~Elasticity, ncol=3) +
  theme(strip.text.x = element_blank(), strip.background=element_blank()) +
  geom_text(aes(label=Text), data=nd, hjust=0, show_guide=FALSE)
  
# ggsave(file=".\\plots\\FuelProfitVsPcAnd2Elasticities2NERC.png", width=10, height=6)   

  
}

{ ## Profit by ParentPAC

#pd1 <- pd
#pd1[pd1$Fuel=='NUCLEAR','ParentPAC'] <- 'None'

elasticities <- c(-0.2,-0.9) 
d <- data.frame()
pac.names <- exclude(unique(pd$ParentPAC), c('None','American Public Power Assn','Edison Electric Institute'))
for (elasticity in elasticities) {
  print(elasticity)
  load.groups <- c(0, 0.01, seq(0.05, 0.95, length=10), 1)
  #load.groups <- c(0, 1)
  industry <- makeNercIndustry(pd, load.data, elasticity=elasticity, ramp=0.01, load.groups, owner.column='ParentPAC')
  dd <- profitByOwnerPlot(industry, pac.names, pc.seq=seq(0, 50, length=100), p.clear=NULL, plot=F)
  dd$Elasticity <- elasticity
  d <- rbind(d,dd)
}
d$Elasticity <- factor(d$Elasticity, ordered=TRUE,
                levels=elasticities)
d$profit <- d$profit/10^9

dd <- subset(d, Elasticity==-0.9)

# Get the biggest winners and losers, abs(profit at pc=0 - profit at pc=50)
delta.profit <- aggregate(dd$profit, list(PAC=dd$owner), function(x) max(x)-min(x))
#delta.profit <- aggregate(dd$profit, list(PAC=dd$owner), function(x) (max(x)-min(x))/mean(x))
delta.profit <- delta.profit[order(-delta.profit$x),]
keep.pacs <- delta.profit$PAC[1:10]
d1 <- subset(dd, owner %in% keep.pacs)

#dev.new(width=800, height=600)
label.data <- subset(d1, pc==50)
label.data$pos <- label.data$profit
label.data$pc <- 51
label.data[label.data$owner=='Public Service Electric & Gas', 'pos'] <- 1.95
label.data[label.data$owner=='Firstenergy Corp', 'pos'] <- 1.23
label.data[label.data$owner=='Constellation Energy', 'pos'] <- 1.37

ggplot(d1, aes(pc, profit)) + geom_line(size=1, aes(color=owner)) + 
  ylab('Profit\n($B)') + xlab(expression(paste('Carbon Tax ($/')~tCO[2]~')')) +
  geom_text(data=label.data, aes(label=owner, color=owner, y=pos), hjust=0) +
  scale_x_continuous(limits=c(0,70), breaks=c(0,25,50)) +
  theme(legend.position="none") +
  theme(text = element_text(size=16)) 
ggsave(file=".\\plots\\PacProfitWithNucsVsPcNerc.png", width=10, height=8)   

### Same thing but exclude profits from nuclear

pd1 <- pd
pd1[pd1$Fuel=='NUCLEAR','ParentPAC'] <- 'None'

dnn <- data.frame()
pac.names <- exclude(unique(pd1$ParentPAC), c('None','American Public Power Assn','Edison Electric Institute'))
for (elasticity in elasticities) {
  print(elasticity)
  load.groups <- c(0, 0.01, seq(0.05, 0.95, length=10), 1)
  industry <- makeNercIndustry(pd1, load.data, elasticity=elasticity, ramp=0.01, load.groups, owner.column='ParentPAC')
  ddnn <- profitByOwnerPlot(industry, pac.names, pc.seq=seq(0, 50, length=100), p.clear=NULL, plot=F)
  ddnn$Elasticity <- elasticity
  dnn <- rbind(dnn,ddnn)
}
dnn$Elasticity <- factor(dnn$Elasticity, ordered=TRUE,
                       levels=elasticities)
dnn$profit <- dnn$profit/10^9

d1.no.nucs <- subset(dnn, Elasticity==-0.9)
d1.no.nucs <- subset(d1.no.nucs, owner %in% label.data$owner)

nn.label.data <- subset(d1.no.nucs, pc==50)
nn.label.data$pos <- nn.label.data$profit
nn.label.data$pc <- 51
#nn.label.data[label.data$owner=='Public Service Electric & Gas', 'pos'] <- 1.95
nn.label.data[nn.label.data$owner=='Exelon Corp', 'pos'] <- 0.10
nn.label.data[nn.label.data$owner=='Firstenergy Corp', 'pos'] <- -0.01
nn.label.data[nn.label.data$owner=='Constellation Energy', 'pos'] <- 0.01

ggplot(d1.no.nucs, aes(pc, profit)) + geom_line(size=1, aes(color=owner)) + 
  ylab('Profit\n($B)') + xlab(expression(paste('Carbon Tax ($/')~tCO[2]~')')) +
  geom_text(data=nn.label.data, aes(label=owner, color=owner, y=pos), hjust=0) +
  scale_x_continuous(limits=c(0,72), breaks=c(0,25,50)) +
  theme(legend.position="none") +
  theme(text = element_text(size=16)) 
ggsave(file=".\\plots\\PacProfitWithNoNucsVsPcNerc.png", width=10, height=8)   

}

{ ## Manual 'hand drawn' plot showing how profit can increase with pc
k <- 30
e <- -2

p0 <- 1.9
b1 <- 9
a1 <- 0.15
a2 <- 20*a1
b2 <- b1
q.t <- seq(0, b1-0.001, length=10000)

pd.func <- function(Q) (Q/k)^(1/e)
p1.func <- function(Q) -a1/(Q-b1) + p0-(a1/b1)
pp1.func <- function(Q) -a1/(Q-b1) + p0-(a1/b1) + 0.75

p2.func <- function(Q) -a2/(Q-b2) + p0-(a2/b2)


# Find intersections:
int.q1 <- uniroot(function(x) pd.func(x)-p1.func(x), c(0.01,8.9))$root
int.q2 <- uniroot(function(x) pd.func(x)-p2.func(x), c(0.01,8.9))$root
int.qq1 <- uniroot(function(x) pd.func(x)-pp1.func(x), c(0.1,8.9))$root

q.d <- seq(0, 10, length=1000)
p.d <- pd.func(q.d)
p.1 <- p1.func(q.t)
p.2 <- p2.func(q.t)

pp.1 <- pp1.func(q.t)

int.p1 <- p1.func(int.q1)
int.p2 <- p2.func(int.q2)
int.pp1 <- pp1.func(int.qq1)

# Make the polygons
poly.q1 <- c(int.q1, 0, q.t[p.1<int.p1])
poly.p1 <- c(int.p1, int.p1, p1.func(poly.q1[3:length(poly.q1)]))

poly.qq1 <- c(int.qq1, 0, q.t[pp.1<int.pp1])
poly.pp1 <- c(int.pp1, int.pp1, pp1.func(poly.qq1[3:length(poly.qq1)]))

poly.q2 <- c(int.q2, 0, q.t[p.2<int.p2])
poly.p2 <- c(int.p2, int.p2, p2.func(poly.q2[3:length(poly.q2)]))


ggplot(data.frame(p=p.d,q.d=q.d), aes(x=q.d, y=p.d)) + geom_line(size=1) +
  geom_line(data=data.frame(q.t=q.t, p.1=p.1), mapping=aes(x=q.t,y=p.1), size=1) +
  geom_line(data=data.frame(q.t=q.t, p.2=p.2), mapping=aes(x=q.t,y=p.2), size=1) +
  geom_line(data=data.frame(q.t=q.t, pp.1=pp.1), mapping=aes(x=q.t,y=pp.1), size=1) +
  geom_polygon(data = data.frame(x=poly.q1,y=poly.p1), aes(x = x,y = y), fill = "blue",alpha = 0.5) +
  geom_polygon(data = data.frame(x=poly.q2,y=poly.p2), aes(x = x,y = y),fill = "red",alpha = 0.5) +
  geom_polygon(data = data.frame(x=poly.qq1,y=poly.pp1), aes(x = x,y = y),fill = "blue",alpha = 0.5) +
  ylab('P') + xlab('Q') +
  scale_x_continuous(limits=c(0,10), expand=c(0,0)) +
  scale_y_continuous(limits=c(1.5,4), expand=c(0,0)) +
  annotate('text', x=c(9.2, 8.34, 7, 8.5), y=c(3.75, 3.5, 3.5, 1.65), 
              label=c('S1', "S1'", 'S2', 'Demand\ne=-2')) +
  theme(text = element_text(size=14)) +
  theme(axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank())

# ggsave(file=".\\plots\\StylizedProfitExplanation.png")  
}

{ ## Load Duration Curves

dev.new(width=800, height=600)
ggplot(subset(load.data, Region!='AVG'), aes(x=TR, y=Load)) + geom_line(aes(linetype=Region), size=1) +
  scale_y_continuous(limits=c(0, 125000), labels = comma) +
  geom_line(data=subset(load.data,Region=='AVG'), aes(x=TR, y=Load), size=2) +
  xlab('Fraction of Hours per Year') + ylab('Load\n(MWh)')

  #ggsave(file=".\\plots\\LoadCurves.png") 

  generation <- sum(pd$NetGeneration)
  loadCurve <- generateLoadDurationCurve(subset(load.data, Region=='AVG'), generation, check.plots=T)
  load.groups <- c(0, 0.01, seq(0.05, 0.95, length=5), 1)
  averages <- findAverageLoads(loadCurve, load.groups)
  
  dev.new(); 
  plotLoadDurationCurve(loadCurve, load.groups, averages) + 
    ggtitle('') + xlab('Fraction of Hours per Year') + ylab('Demand\n(MWh)') +
    theme(text = element_text(size=14)) + scale_x_continuous(labels = comma)
  
  #ggsave(file=".\\plots\\LoadCurvesSegments.png") 
}

{ ## Profit and Government Welfare Plots

gov.weights <- c(0.2, 0.2)
load.groups <- c(0, 1)
elasticity <- -0.9
scc <- 20
pc.seq <- seq(0, 100, length=100)
# All plants and usual lobbies
#load.groups <- c(0, 0.01, seq(0.05, 0.95, length=10), 1)

lobbies <- exclude(unique(pd$ParentPAC), c('None','American Public Power Assn','Edison Electric Institute'))
industry <- makeNercIndustry(pd, load.data, elasticity=elasticity, ramp=0.01, load.groups, owner.column='ParentPAC')
gov <- componentGov(scc=scc, weight=gov.weights )

d <- makeWelfarePlot(industry, gov, lobbies, pc.seq=pc.seq, data=T)
d <- subset(d, entity != 'Owners')
d$welfare <- d$welfare/10^9

dev.new(width=800, height=600)
ggplot(d, aes(pc, welfare)) + geom_line(size=1, aes(color=entity,linetype=entity)) + 
  ylab('Welfare\n($B)') + xlab(expression(paste('Carbon Tax ($/')~tCO[2]~')')) +
  scale_linetype_manual(values = c(1,2,4)) +
  theme(legend.title=element_blank()) +
  theme(text = element_text(size=14))  
# ggsave(file=".\\plots\\WelfareScc40.png")  


## Removing all nuclear power plants from the lobbying
#load.groups <- c(0, 0.01, seq(0.05, 0.95, length=10), 1)
#elasticity <- -0.9
#load.groups <- c(0, 1)
pd1 <- pd
pd1[pd1$Fuel=='NUCLEAR','ParentPAC'] <- 'None'
pd1[pd1$Fuel=='HYDRO','ParentPAC'] <- 'None'
lobbies <- exclude(unique(pd1$ParentPAC), c('None','American Public Power Assn','Edison Electric Institute'))
industry <- makeNercIndustry(pd1, load.data, elasticity=elasticity, ramp=0.01, load.groups, owner.column='ParentPAC')
gov <- componentGov(scc=scc, weight=gov.weights)

d1 <- makeWelfarePlot(industry, gov, lobbies, pc.seq=pc.seq, data=T)
d1 <- subset(d1, entity != 'Owners')
d1$welfare <- d1$welfare/10^9

dev.new(width=800, height=600)
ggplot(d1, aes(pc, welfare)) + geom_line(size=1, aes(color=entity,linetype=entity)) + 
  ylab('Welfare\n($B)') + xlab(expression(paste('Carbon Tax ($/')~tCO[2]~')')) +
  scale_linetype_manual(values = c(1,2,4)) +
  theme(legend.title=element_blank()) +
  theme(text = element_text(size=14))  
# ggsave(file=".\\plots\\WelfareScc40NoNucNoHydro.png")  


}


##### OLD ######

{ ## Profit by elasticity plots

elasticities <- c(-0.2, -1, -2, -4, -6)
plots <- list()
for (e in elasticities) {
  #groups <- c(0, seq(0.05, 0.95, length=6), 1)
  groups <- c(0, 1)
  weights <- groups[-1]-groups[-length(groups)]
  generation <- sum(pd$NetGeneration)
  loadCurve <- generateLoadDurationCurve(subset(load.data, Region=='AVG'), generation, check.plots=F)
  average.loads <- findAverageLoads(loadCurve, groups)
  #dev.new(); print(plotLoadDurationCurve(loadCurve, groups, average.loads) + ggtitle(paste("Load Duration Curve")))

  load.segments <- lapply(1:length(weights), function(i) {
    sub.demand <- ceDemand(e=e, pd, ini.supply=average.loads[i])
    makeLoadSegment(pd$ParentPAC, pd, sub.demand, 
                    name=paste(groups[i],groups[i+1],sep='-'), ramp=5 )
  })
  sector <- assembleSector(load.segments, weights, name='Sector')

  agg.industry <- assembleIndustry(list(sector), name=paste('1 Sector,',length(weights),'LGs'))
  gov <- componentGov(scc=scc, weight=c(1,1))

  plots[[length(plots)+1]] <- makeStandardPlot(agg.industry, pc.seq=seq(0,100,length=100))
}
names(plots) <- paste('e=',elasticities,sep='')
plot.data <- combinePlotData(plots)
ggplot(plot.data, aes(pc, result)) + geom_line(size=1, aes(color=Plot))



}


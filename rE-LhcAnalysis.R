rm(list=ls(all=TRUE))
require(ggplot2)
require(ggthemes)
require(reshape)
require(parallel)
require(tgp)

theme_set(theme_gdocs())
theme_update(plot.background = element_blank()) #, text = element_text(size=14))
#theme_update(legend.key.width=unit(3,"lines"))

setwd("C:\\Users\\sisley\\Documents\\RAND\\Thesis\\Empirical\\rE-market\\")
#source('rE-support.R')
#source('rE-plots.R')
options(digits=10)
source('rE-support.R')
#### Assemble Data ####

#s3.bucket <- 's3://isleyremarketresults/'
#for (i in 1:1) {
#  system(paste('aws s3 cp ', s3.bucket, 'lhcResults', i, '.RData', ' ',getwd(), sep=''))
#}

# Now load all the data in and combine it
results <- list()
for (i in 1:20) {
  load(paste('lhcData\\lhcResults',i,'.RData',sep=''))
  results <- c(results, ans)
}

#### Diagnostic Histograms ####

# Histogram of final negotiated price
pcs <- sapply(results, function(x) x$cont$pc.eqbm)
weights <- sapply(results, function(x) c( x$alpha.scc, x$alpha.spike, 
                                          x$alpha.revenue, x$alpha.production, x$elasticity) )
weights <- as.data.frame(t(weights))
names(weights) <- c('scc','spike','revenue','production','e')

hist(pcs)
sum(pcs > 0.1 & pcs < 49.9)

# Histogram of total contributions
sum.contributions <- sapply(results, function(x) sum(unlist(x$cont$contributions)))
hist(sum.contributions[sum.contributions < 1e9 & sum.contributions > 10])
hist(sum.contributions[sum.contributions > 10])
sum(sum.contributions > 5e8)
sum(sum.contributions < 5e8 & sum.contributions > 10)

##### Load up the empirical data ####

pd <- getPlantData("New_Plant_Data.csv")
pd$Renewable <- 0
ff.indx <- pd$Fuel == 'OIL' | pd$Fuel=='GAS' | pd$Fuel == 'COAL'
pd$Renewable[!ff.indx] <- pd$NetGeneration[!ff.indx] 

net.generation <- aggregate(pd$NetGeneration, by=list(pd$ParentPAC), sum)
names(net.generation) <- c('ParentPAC','NetGeneration')
net.renewable <- aggregate(pd$Renewable, by=list(pd$ParentPAC), sum)
names(net.renewable) <- c('ParentPAC','CarbonFree')

maplight1 <- read.csv('MaplightYearlyAvg_2008-2010.csv', header=TRUE, na.strings = c("N/A","#N/A","?"), stringsAsFactors=FALSE)
maplight <- merge(maplight1, net.generation)
maplight <- merge(maplight, net.renewable)
maplight$PercentCarbonFree <- maplight$CarbonFree/maplight$NetGeneration
row.names(maplight) <- maplight$ParentPAC

raw.contributions <- sapply(results, function(x) unlist(x$cont$contributions))
raw.contributions <- merge(maplight, raw.contributions, by="row.names")

contributions <- subset(raw.contributions, !ParentPAC %in% c('Exelon Corp','Entergy Corp') )

# Use R^2 as the goodness of fit metric
fit.r2 <- sapply(1:4000, function(x) {
    df <- data.frame(MapLight=contributions$Contribution, Theoretical=unlist(contributions[, paste('V',x,sep='')]))
    summary( lm(MapLight ~ Theoretical, df) )$r.squared
  })

# Use r squared with weights on net generation
fit.r2w <- sapply(1:4000, function(x) {
  df <- data.frame(MapLight=contributions$Contribution, Theoretical=unlist(contributions[, paste('V',x,sep='')]))
  summary( lm(MapLight ~ Theoretical, df, weights=contributions$NetGeneration) )$r.squared
})

# Use sum squared error as goodness of fit metric (DOES NOT WORK)
#fit.sse <- sapply(1:4000, function(x) {
#    t <- unlist(contributions[, paste('V',x,sep='')])
#    m <- contributions$Contribution
#    sum( (t-m)^2 )
#})

fit.sse <- sapply(1:4000, function(x) {
  t <- unlist(contributions[, paste('V',x,sep='')])
  t <- (t-min(t))/(max(t)-min(t))
  m <- contributions$Contribution
  m <- (m-min(m))/(max(m)-min(m))
  sum( (t-m)^2 ) 
})

all <- as.data.frame(cbind(pcs, sum.contributions, weights, fit.r2, fit.r2w, fit.sse))
View( all[ order(all$fit.sse),] )

#best.i <- which.max(fit.r2)
best.i <- 1515 #3838 #2525 # 1308 #
d <- cbind(contributions[,1:7], unlist(contributions[,paste('V',best.i,sep='')]))
names(d) <- c(names(d)[1:7], 'Theoretical')
#ggplot(d, aes(x=Contribution, y=Theoretical)) + geom_point()


ggplot(d, aes(x=Contribution, y=Theoretical, size=NetGeneration)) + 
  annotate('line', x=c(0,1), y=c(0,1), color='grey') +
  geom_point(aes(fill=PercentCarbonFree), color='black', pch=21) +
  scale_fill_gradient(low="black", high='white') +
  geom_text(aes(label=ParentPAC, y=Theoretical, x=Contribution), 
            size=3, hjust=0.5, vjust=0, color='black') +
  xlab('MapLight') + ylab('Theoretical') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  theme(text = element_text(size=14)) +
  guides(fill=FALSE, size=guide_legend(title="Net Generation\n(Million MWh)"))

all.nz <- all[pcs>0 & pcs<49.99,]
save(all=all, all.nz=all.nz, file='all.RData')

# Show how often Exelon and Entergy are the top two
rc <- raw.contributions

exelon.rank <- sapply(1:4000, function(x) {
  col <- paste('V',x,sep='')
  df <- subset(rc, select=c('ParentPAC',col))
  df <- df[ order(-df[col]),]
  #return('Exelon Corp' %in% df[1:3,1]) 
  return(which('Exelon Corp' == df))
})
#sum(exelon.first)/length(exelon.first)
hist(exelon.rank)
sum(exelon.rank==1)/length(exelon.rank)

free.loaders <- sapply(1:4000, function(x) {
  col <- paste('V',x,sep='')
  df <- subset(rc, select=c('ParentPAC',col))
  return( sum(df[col] <= 0) )
})
hist(free.loaders)

# Look at distribution of contribution levels across *all* runs
all.c <- unlist(subset(rc, select=V1:V4000))
hist(all.c)

##### ANALYSIS TODO #####

# Find 'best fit' via excluding 'Exelon Corp' & 'Entergy Corp'
#    Still not very good fits...
# Make plot showing how often 'Exelon Corp' & 'Entergy Corp' are the top two
# Show how theory predicts more free-riding
# Need some simple plots with only a few variables changing
# Look up max possible contribution and compare to actual/theoretical values
# Show how spike and revenue must be close for any sort of realistic result.
# Look at adding theoretical contribution and netgeneration into regression. 
#   Are results better than netgeneration by iteslf?
# Look at contributions as a percent of revenue and/or profit
# Need to show how the 'ramp' parameter affects the welfare plots


#### Redo Notes

# Make weights go from 0-1.2
# make e go from -0.1 to -1.0 (same)
# allow pc solution range to go from 0-150 rather than 0-50
# Maybe I should do a sparse run but with big ranges, weights from 0-5
# Look into using ramp=3. Make sure load.groups are being used...



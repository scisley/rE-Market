rm(list=ls(all=TRUE))
require(ggplot2)
require(ggthemes)
require(reshape)
require(parallel)
require(tgp)

theme_set(theme_gdocs())
theme_update(plot.background = element_blank()) #, text = element_text(size=14))
#theme_update(legend.key.width=unit(3,"lines"))

setwd("C:\\Users\\sisley\\Documents\\RAND\\Thesis\\Empirical\\rE-market\\lhcData")
#source('rE-support.R')
#source('rE-plots.R')
options(digits=10)

#### Assemble Data ####

s3.bucket <- 's3://isleyremarketresults/'
for (i in 1:1) {
  system(paste('aws s3 cp ', s3.bucket, 'lhcResults', i, '.RData', ' ',getwd(), sep=''))
}

# Now load all the data in and combine it
results <- list()
for (i in 1:9) {
  load(paste('lhcResults',i,'.RData',sep=''))
  results <- c(results, ans)
}

#### Diagnostic Histograms ####

# Histogram of final negotiated price
pcs <- sapply(1:1800, function(x) results[[x]]$cont$pc.eqbm)























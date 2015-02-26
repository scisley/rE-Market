library(lineprof)
library(shiny)

setwd("~/RAND/Thesis/Empirical/rE-market/profiler")
source('rE-support.R')
source('rE-lobbying.R')
source("rE-market-profiler-source.R")

l <- lineprof(go())
l
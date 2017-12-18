rm(list=ls())
sink()
source("R/sequentialnma.R")
source("R/main.R")
source("R/pairwisenowarn.R")
library(devtools)
data(Dong)
options(warn=-1)
Dongseq =suppressWarnings(
  sequentialnma(data=Dong, perarm=TRUE, type="binary", sm="OR", tau.preset = 0.1183216, 
                         comb.fixed=F, comb.random=T,
                     studlab="id",sortvar="year")
  
)
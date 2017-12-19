install.packages("meta")
install.packages("netmeta")
install.packages("plyr")
install.packages("devtools")
install.packages("ggplot2")
install.packages("grid")

install_github("esm-ispm-unibe-ch/sequentialnma")
library(sequentialnma)

data(Dong)

dongseq <- sequentialnma(data=Dong, perarm=TRUE, type="binary", sm="OR", tau.preset = sqrt(0.014), 
                         comb.fixed=F, comb.random=T,
                         studlab="id",sortvar="year")







rm(list=ls())
source('C:/Users/nikolakopoulou/Desktop/sequentialnma/R/sequentialnma.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma/R/fordelta.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma/R/alpha.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma/R/formatdata.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma/R/main.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma/R/repeatedCI.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma/R/plot.sequentialnma.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma/R/summary.sequentialnma.R')

library(readr)
test <- read_delim("C:/Users/nikolakopoulou/Desktop/conclusive revision/test.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

testseq <- sequentialnma(data=test, perarm=TRUE, type="binary", sm="OR", tau.preset = sqrt(0.014), 
                           comb.fixed=F, comb.random=T,
                           studlab="id",sortvar="year")

Leuchtsmall <- read_delim("C:/Users/nikolakopoulou/Desktop/conclusive revision/Leuchtsmall.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

leuchtseq <- sequentialnma(data=Leuchtsmall, perarm=FALSE, type="continuous", sm="SMD", tau.preset = sqrt(0.049), 
                             comb.fixed=F, comb.random=T,
                             studlab="id",sortvar="year", TE="effect", seTE="se",
                             t1="treat1", t2="treat2")

summary(testseq,comparison="A:B")
summary(leuchtseq,comparison="CLO:CPZ")




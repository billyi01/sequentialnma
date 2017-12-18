rm(list=ls())
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/sequentialnma.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/fordelta.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/alpha.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/formatdata.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/main.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/repeatedCI.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/plot.sequentialnma.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/summary.sequentialnma.R')

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


testseq$studies[unique(testseq$studies$id)==1,]

for(i in 1:10){
  print(unique(testseq$studies$year[testseq$studies$id==unique(testseq$studies$id)[i]]))
}

m2=mapply(function(i){((unique(testseq$studies$year[testseq$studies$id==unique(testseq$studies$id)[i]])))},
       1:length(testseq$result))



testseq$studies$year[unique(testseq$studies$id)[4]]

unique(testseq$studies$id)[10]

unique(testseq$studies$year[testseq$studies$id==unique(testseq$studies$id)[10]])




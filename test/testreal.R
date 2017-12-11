######################################
###########test Leucht################
######################################

#load old and new functions
#these are the 7 functions in sequentialnma2
#I load them with source because the old one is named sequentialnma as well
rm(list=ls())
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/sequentialnma.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/fordelta.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/alpha.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/formatdata.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/main.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/rci.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/plot.sequentialnma.R')

library(readr)
LeuchtID <- read_delim("C:/Users/nikolakopoulou/Desktop/LeuchtID.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
#View(LeuchtID)
leuchtseq1 <- sequentialnma(data=LeuchtID, perarm=FALSE, type="continuous", sm="SMD", tau.preset = 0.2213594, 
                           comb.fixed=F, comb.random=T,
                           studlab="id",sortvar="year", TE="effect", seTE="se",
                           t1="treat1", t2="treat2")

install_github("esm-ispm-unibe-ch/sequentialnma")
library(sequentialnma)
y<-livenma(data=LeuchtID, level="study",type="continuous",effsize="SMD",tau.sq=0.049,delta=NA)

#check last step of leucht
y$output[y$output$ComparisonNetw=="HAL vs OLA",]
leuchtseq1$laststep$output["HAL:OLA",]

#this is figure 1 of main manuscript
plot(seqnmaobject=leuchtseq1,comparison="HAL:OLA",evidence="both",small.values=NA) 

#results with old and new function
DirectZscore1 = as.vector(y$Prosp[68, 8, 1:(max(y$D$idyear) -1)], mode = "numeric")
NetworkZscore1 = as.vector(y$Prosp[68, 10, 1:(max(y$D$idyear) - 1)], mode = "numeric")
DirectT1 = as.vector(y$Prosp[68, 12, 1:(max(y$D$idyear) - 1)], mode = "numeric")
NetworkT1 = as.vector(y$Prosp[68, 13, 1:(max(y$D$idyear) - 1)], mode = "numeric")
DirectEfficacyB1 = as.vector(y$Prosp[68, 15, 1:(max(y$D$idyear) - 1)], mode = "numeric")
NetworkEfficacyB1 = as.vector(y$Prosp[68, 17, 1:(max(y$D$idyear) - 1)], mode = "numeric")

DirectZscore2 = unlist(mapply(function(i){(leuchtseq1$result[[i]]$output["HAL:OLA","DirectZscore"])},
                              1:length(leuchtseq1$result)))
NetworkZscore2 = unlist(mapply(function(i){(leuchtseq1$result[[i]]$output["HAL:OLA","NetworkZscore"])},
                               1:length(leuchtseq1$result)))
DirectT2 = unlist(mapply(function(i){(leuchtseq1$result[[i]]$output["HAL:OLA","DirectTaccum"])},
                         1:length(leuchtseq1$result)))
NetworkT2 = unlist(mapply(function(i){(leuchtseq1$result[[i]]$output["HAL:OLA","NetworkTaccum"])},
                          1:length(leuchtseq1$result)))
DirectEfficacyB2 = unlist(mapply(function(i){(leuchtseq1$result[[i]]$output["HAL:OLA","DirectBoundary"])},
                                 1:length(leuchtseq1$result)))
NetworkEfficacyB2 = unlist(mapply(function(i){(leuchtseq1$result[[i]]$output["HAL:OLA","NetworkBoundary"])},
                                  1:length(leuchtseq1$result)))



##test Dong
#data(Dong)
library(readr)
Dong <- read_delim("C:/Users/nikolakopoulou/Desktop/Dong.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
#View(Dong)


Dongseq <- sequentialnma(data=Dong, perarm=TRUE, type="binary", sm="OR", tau.preset = 0.1183216, 
                         comb.fixed=F, comb.random=T,
                         studlab="id",sortvar="year")

install_github("esm-ispm-unibe-ch/sequentialnma")
library(sequentialnma)
x<-livenma(data=Dong, level="arm",type="binary",effsize="OR",tau.sq=0.014,delta=NA)

#check last step of dong
x$output[x$output$ComparisonNetw=="ICS vs LABA-ICS",]
Dongseq$laststep$output["ICS:LABA-ICS",]

###

DirectZscore1 = as.vector(x$Prosp[2, 8, 1:(max(x$D$idyear) -1)], mode = "numeric")
NetworkZscore1 = as.vector(x$Prosp[2, 10, 1:(max(x$D$idyear) - 1)], mode = "numeric")
DirectT1 = as.vector(x$Prosp[2, 12, 1:(max(x$D$idyear) - 1)], mode = "numeric")
NetworkT1 = as.vector(x$Prosp[2, 13, 1:(max(x$D$idyear) - 1)], mode = "numeric")
DirectEfficacyB1 = as.vector(x$Prosp[2, 15, 1:(max(x$D$idyear) - 1)], mode = "numeric")
NetworkEfficacyB1 = as.vector(x$Prosp[2, 17, 1:(max(x$D$idyear) - 1)], mode = "numeric")

DirectZscore2 = unlist(mapply(function(i){(Dongseq$result[[i]]$output["ICS:LABA-ICS","DirectZscore"])},
                              1:length(Dongseq$result)))
NetworkZscore2 = unlist(mapply(function(i){(Dongseq$result[[i]]$output["ICS:LABA-ICS","NetworkZscore"])},
                               1:length(Dongseq$result)))
DirectT2 = unlist(mapply(function(i){(Dongseq$result[[i]]$output["ICS:LABA-ICS","DirectTaccum"])},
                         1:length(Dongseq$result)))
NetworkT2 = unlist(mapply(function(i){(Dongseq$result[[i]]$output["ICS:LABA-ICS","NetworkTaccum"])},
                          1:length(Dongseq$result)))
DirectEfficacyB2 = unlist(mapply(function(i){(Dongseq$result[[i]]$output["ICS:LABA-ICS","DirectBoundary"])},
                                 1:length(Dongseq$result)))
NetworkEfficacyB2 = unlist(mapply(function(i){(Dongseq$result[[i]]$output["ICS:LABA-ICS","NetworkBoundary"])},
                                  1:length(Dongseq$result)))


DiffNetwZscores=abs(unique(round(NetworkZscore2[!is.na(NetworkZscore2)],3)))-
  abs(unique(round(NetworkZscore1[!is.na(NetworkZscore1)],3)))
DiffDirZscores=abs(unique(round(DirectZscore2[!is.na(DirectZscore2)],3)))-
  abs(unique(round(DirectZscore1[!is.na(DirectZscore1)],3)))

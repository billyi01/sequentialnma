#test data input
rm(list=ls())
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/sequentialnma.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/fordelta.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/alpha.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/formatdata.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/main.R')
source('C:/Users/nikolakopoulou/Desktop/sequentialnma2/R/rci.R')

####test arm level binary
library(readr)
test <- read_delim("C:/Users/nikolakopoulou/Desktop/test.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
test=as.data.frame(test)
install_github("esm-ispm-unibe-ch/sequentialnma")
library(sequentialnma)

testarmbinary=function(data){
  testseq <- sequentialnma(data=test, perarm=TRUE, type="binary", sm="OR", tau.preset = sqrt(0.014), 
                           comb.fixed=F, comb.random=T,
                           studlab="id",sortvar="year")
  
  x<-livenma(data=test, level="arm",type="binary",effsize="OR",tau.sq=0.014,delta=NA)
  
  #check last step 
  x$output[x$output$ComparisonNetw=="A vs B",]
  testseq$laststep$output["A:B",]
  
  ###
  DirectZscore1 = as.vector(x$Prosp[1, 8, 1:(max(x$D$idyear) -1)], mode = "numeric")
  NetworkZscore1 = as.vector(x$Prosp[1, 10, 1:(max(x$D$idyear) - 1)], mode = "numeric")
  DirectT1 = as.vector(x$Prosp[1, 12, 1:(max(x$D$idyear) - 1)], mode = "numeric")
  NetworkT1 = as.vector(x$Prosp[1, 13, 1:(max(x$D$idyear) - 1)], mode = "numeric")
  DirectEfficacyB1 = as.vector(x$Prosp[1, 15, 1:(max(x$D$idyear) - 1)], mode = "numeric")
  NetworkEfficacyB1 = as.vector(x$Prosp[1, 17, 1:(max(x$D$idyear) - 1)], mode = "numeric")
  
  DirectZscore2 = unlist(mapply(function(i){(testseq$result[[i]]$output["A:B","DirectZscore"])},
                                1:length(testseq$result)))
  NetworkZscore2 = unlist(mapply(function(i){(testseq$result[[i]]$output["A:B","NetworkZscore"])},
                                 1:length(testseq$result)))
  DirectT2 = unlist(mapply(function(i){(testseq$result[[i]]$output["A:B","DirectTaccum"])},
                           1:length(testseq$result)))
  NetworkT2 = unlist(mapply(function(i){(testseq$result[[i]]$output["A:B","NetworkTaccum"])},
                            1:length(testseq$result)))
  DirectEfficacyB2 = unlist(mapply(function(i){(testseq$result[[i]]$output["A:B","DirectBoundary"])},
                                   1:length(testseq$result)))
  NetworkEfficacyB2 = unlist(mapply(function(i){(testseq$result[[i]]$output["A:B","NetworkBoundary"])},
                                    1:length(testseq$result)))
  
  DiffNetwZscores=abs(unique(round(NetworkZscore2[!is.na(NetworkZscore2)],3)))-
    abs(unique(round(NetworkZscore1[!is.na(NetworkZscore1)],3)))
  DiffDirZscores=abs(unique(round(DirectZscore2[!is.na(DirectZscore2)],3)))-
    abs(unique(round(DirectZscore1[!is.na(DirectZscore1)],3)))
  DiffDirectEfficacyB=abs(unique(round(DirectEfficacyB2[!is.na(DirectEfficacyB2)],3)))-
    abs(unique(round(DirectEfficacyB1[!is.na(DirectEfficacyB1)],3)))
  DiffNetworkEfficacyB=abs(unique(round(NetworkEfficacyB2[!is.na(NetworkEfficacyB2)],3)))-
    abs(unique(round(NetworkEfficacyB1[!is.na(NetworkEfficacyB1)],3)))
  
  differences=list(DiffNetwZscores=DiffNetwZscores,DiffDirZscores=DiffDirZscores,
                   DiffDirectEfficacyB=DiffDirectEfficacyB,DiffNetworkEfficacyB=DiffNetworkEfficacyB)
  print(differences)
}

testarmbinary(data = test)

#test subset of Leucht
library(readr)
Leuchtsmall <- read_delim("C:/Users/nikolakopoulou/Desktop/Leuchtsmall.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)
install_github("esm-ispm-unibe-ch/sequentialnma")
library(sequentialnma)

testivcontinuous=function(data){
  leuchtseq <- sequentialnma(data=Leuchtsmall, perarm=FALSE, type="continuous", sm="SMD", tau.preset = sqrt(0.049), 
                             comb.fixed=F, comb.random=T,
                             studlab="id",sortvar="year", TE="effect", seTE="se",
                             t1="treat1", t2="treat2")
  

  y<-livenma(data=Leuchtsmall, level="study",type="continuous",effsize="SMD",tau.sq=0.049,delta=NA)
  
  #check last step of leuchtsmall
  y$output[y$output$ComparisonNetw=="CLO vs CPZ",]
  leuchtseq$laststep$output["CLO:CPZ",]
  
  DirectZscore1 = as.vector(y$Prosp[1, 8, 1:(max(y$D$idyear) -1)], mode = "numeric")
  NetworkZscore1 = as.vector(y$Prosp[1, 10, 1:(max(y$D$idyear) - 1)], mode = "numeric")
  DirectT1 = as.vector(y$Prosp[1, 12, 1:(max(y$D$idyear) - 1)], mode = "numeric")
  NetworkT1 = as.vector(y$Prosp[1, 13, 1:(max(y$D$idyear) - 1)], mode = "numeric")
  DirectEfficacyB1 = as.vector(y$Prosp[1, 15, 1:(max(y$D$idyear) - 1)], mode = "numeric")
  NetworkEfficacyB1 = as.vector(y$Prosp[1, 17, 1:(max(y$D$idyear) - 1)], mode = "numeric")
  
  DirectZscore2 = unlist(mapply(function(i){(leuchtseq$result[[i]]$output["CLO:CPZ","DirectZscore"])},
                                1:length(leuchtseq$result)))
  NetworkZscore2 = unlist(mapply(function(i){(leuchtseq$result[[i]]$output["CLO:CPZ","NetworkZscore"])},
                                 1:length(leuchtseq$result)))
  DirectT2 = unlist(mapply(function(i){(leuchtseq$result[[i]]$output["CLO:CPZ","DirectTaccum"])},
                           1:length(leuchtseq$result)))
  NetworkT2 = unlist(mapply(function(i){(leuchtseq$result[[i]]$output["CLO:CPZ","NetworkTaccum"])},
                            1:length(leuchtseq$result)))
  DirectEfficacyB2 = unlist(mapply(function(i){(leuchtseq$result[[i]]$output["CLO:CPZ","DirectBoundary"])},
                                   1:length(leuchtseq$result)))
  NetworkEfficacyB2 = unlist(mapply(function(i){(leuchtseq$result[[i]]$output["CLO:CPZ","NetworkBoundary"])},
                                    1:length(leuchtseq$result)))
  
  DiffNetwZscores=abs(unique(round(NetworkZscore2[!is.na(NetworkZscore2)],3)))-
    abs(unique(round(NetworkZscore1[!is.na(NetworkZscore1)],3)))
  DiffDirZscores=abs(unique(round(DirectZscore2[!is.na(DirectZscore2)],3)))-
    abs(unique(round(DirectZscore1[!is.na(DirectZscore1)],3)))
  DiffDirectEfficacyB=abs(unique(round(DirectEfficacyB2[!is.na(DirectEfficacyB2)],3)))-
    abs(unique(round(DirectEfficacyB1[!is.na(DirectEfficacyB1)],3)))
  DiffNetworkEfficacyB=abs(unique(round(NetworkEfficacyB2[!is.na(NetworkEfficacyB2)],3)))-
    abs(unique(round(NetworkEfficacyB1[!is.na(NetworkEfficacyB1)],3)))
  
  differences=list(DiffNetwZscores=DiffNetwZscores,DiffDirZscores=DiffDirZscores,
                   DiffDirectEfficacyB=DiffDirectEfficacyB,DiffNetworkEfficacyB=DiffNetworkEfficacyB)
  print(differences)
}

testivcontinuous(Leuchtsmall)




















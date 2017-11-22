#perform a sequential pairwise and network meta-analysis
install.packages("meta")
install.packages("netmeta")
install.packages("caTools")
install.packages("devtools")

library(meta)
library(netmeta)
library(caTools)
library(devtools)

install.packages("RCurl")
require(RCurl)
install_github("esm-ispm-unibe-ch/sequentialnma")
library(sequentialnma)

#an example with binary data type and arm level data
#input dataset from github
Dong <- read.csv(text=getURL("https://raw.githubusercontent.com/esm-ispm-unibe-ch/sequentialnma/master/data/Dong.csv"),sep=";",header=T)

#perform the sequential analysis and name it x
x<-livenma(data=Dong, level="arm",type="binary",effsize="OR",tau.sq=0.014,delta=NA)

#see the available comparisons from which to choose the one of interest
x$comparison[] #here we will choose "LABA-ICS vs ICS" corresponds to 2

#draw the sequential panel

  #for pairwise meta-analysis
  panel(x,comparison=x$comparison[2],evidence="pairwise",asp=0.1,outcome="harmful")

  #for network meta-analysis
  panel(x,comparison=x$comparison[2],evidence="network",asp=0.1,outcome="harmful")

  #for both sources of evidence
  panel(x,comparison=x$comparison[2],evidence="both",asp=0.1,outcome="harmful")

#draw the repeated confidence intervals plot

  #for pairwise meta-analysis
  rci(x,comparison=x$comparison[2],evidence="pairwise",asp=0.1,outcome="harmful")

  #for network meta-analysis
  rci(x,comparison=x$comparison[2],evidence="network",asp=0.1,outcome="harmful")

  #for both sources of evidence
  rci(x,comparison=x$comparison[2],evidence="both.together",asp=0.1,outcome="harmful")

  #for both sources of evidence
  rci(x,comparison=x$comparison[2],evidence="both.separate",asp=0.1,outcome="harmful")


#an example with continuous data and study level data 
#input dataset from github
  Leucht <- read.csv(text=getURL("https://raw.githubusercontent.com/esm-ispm-unibe-ch/sequentialnma/master/data/Leucht.csv"),sep=";",header=T)
  
#perform the sequential analysis and name it y (note that it takes around 25-30 minutes to run)
  y<-livenma(data=Leucht, level="study",type="continuous",effsize="SMD",tau.sq=0.049,delta=NA)
  
  #see the available comparisons from which to choose the one of interest
  y$comparison[] #here we will choose "HAL vs OLA" corresponds to 68
  
  #draw the sequential panel
  
  #for pairwise meta-analysis
  panel(y,comparison=y$comparison[68],evidence="pairwise",asp=0.1,outcome="harmful")
  
  #for network meta-analysis
  panel(y,comparison=y$comparison[68],evidence="network",asp=0.1,outcome="harmful")
  
  #for both sources of evidence
  panel(y,comparison=y$comparison[68],evidence="both",asp=0.1,outcome="harmful")
  
  #draw the repeated confidence intervals plot
  
  #for pairwise meta-analysis
  rci(y,comparison=y$comparison[68],evidence="pairwise",asp=0.05,outcome="harmful")
  
  #for network meta-analysis
  rci(y,comparison=y$comparison[68],evidence="network",asp=0.05,outcome="harmful")
  
  #for both sources of evidence
  rci(y,comparison=y$comparison[68],evidence="both.together",asp=0.05,outcome="harmful")
  
  #for both sources of evidence
  rci(y,comparison=y$comparison[68],evidence="both.separate",asp=0.05,outcome="harmful")
  
  
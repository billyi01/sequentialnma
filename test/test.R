###########################################################################
############### example of sequential network meta-analysis ###############
###########################################################################

#install packages
install.packages("meta")
install.packages("netmeta")
install.packages("plyr")
install.packages("devtools")
install.packages("ggplot2")
install.packages("grid")

install_github("esm-ispm-unibe-ch/sequentialnma")
library(sequentialnma)

#load data
data(Dong)

#run sequential network meta-analysis
dongseq <- sequentialnma(data=Dong, perarm=TRUE, type="binary", sm="OR", tau.preset = sqrt(0.014), 
                         comb.fixed=F, comb.random=T,
                         studlab="id",sortvar="year")

#summary of monitoring for the comparison ICS:LABA-ICS
summary(dongseq,comparison="ICS:LABA-ICS")

#plots sequential framework for the comparison ICS:LABA-ICS
plot(seqnmaobject=dongseq,comparison="ICS:LABA-ICS",evidence="pairwise",small.values=NA) 
plot(seqnmaobject=dongseq,comparison="ICS:LABA-ICS",evidence="network",small.values=NA) 
plot(seqnmaobject=dongseq,comparison="ICS:LABA-ICS",evidence="both",small.values=NA) 

#plot cumulative effects repeated confidence intervals for ICS:LABA-ICS
repeatedCI(seqnmaobject=dongseq,comparison="ICS:LABA-ICS",evidence="pairwise",small.values=NA) 
repeatedCI(seqnmaobject=dongseq,comparison="ICS:LABA-ICS",evidence="network",small.values=NA) 
repeatedCI(seqnmaobject=dongseq,comparison="ICS:LABA-ICS",evidence="both.separate",small.values=NA) 
repeatedCI(seqnmaobject=dongseq,comparison="ICS:LABA-ICS",evidence="both.together",small.values=NA) 

#Leucth example
data(Leucht)
leuchtseq <- sequentialnma(data=LeuchtID, perarm=FALSE, type="continuous", sm="SMD", tau.preset = 0.2213594, comb.fixed=F, comb.random=T, studlab="id",sortvar="year", TE="effect", seTE="se", t1="treat1", t2="treat2")
plot(seqnmaobject=leuchtseq,comparison="CLO:CPZ",evidence="pairwise",small.values=NA)
repeatedCI(seqnmaobject=leuchtseq,comparison="CLO:CPZ",evidence="both.together",small.values=NA)








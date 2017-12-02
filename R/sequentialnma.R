sequentialnma=function (data, perarm=T, type, sm, tau.preset = NULL, comb.fixed=F, comb.random=T 
                        , studlab="id",sortvar="year", t="t", r="r", n="n", y="y", sd="sd", TE="TE", seTE="seTE"
                        , t1="t1", t2="t2")
{
  library(netmeta)
  library(meta)
  library(plyr)
  library(caTools)
  args =  unlist(as.list(match.call()));
  studies=formatdata (data,args)
  
  #runmain = function (studlab,stids) {
   #     m1=main(data, perarm, type, sm, tau.preset, comb.fixed, comb.random
    #        , t, r, n, y, sd, TE, seTE, t1, t2, studlab=stds,subset=stids)
    #return(m1)
  #}
 
#  uniqueids = unique(studylab)
  
  #accIds = Reduce(function(ac,id){append(ac,c(tail(ac,1),c(id)))},uniqueids,list(c()))
  
  #accIds = mapply(function(i){rev(tail(rev(uniqueids),i))},1:length(uniqueids))
  #result = mapply(function(stds){
   # runmain(studlab=studylab,stids=unlist(stds))}, accIds)
  
  #outopen=(data$Blinding_type==0)|is.na(Schizo$Blinding_type)
  
  #SchizoOBJ=Schizo[outopen==0,]
  
  #SchizoEFF=SchizoEFF[order(SchizoEFF$Study_No),]
  

#  runmain = function (x) {
 #   data=data[data$studlab %in% x,]
#    m1=main(studies, perarm, type, sm, tau.preset, comb.fixed, comb.random)
 #   return(m1)
# }
  
 # uniqueids = unique(studylab)
  
  #accIds = Reduce(function(ac,id){append(ac,c(tail(ac,1),c(id)))},uniqueids,list(c()))
  
#  accIds = mapply(function(i){rev(tail(rev(uniqueids),i))},1:length(uniqueids))
  
  #r1=runmain(accIds[[4]])
  
  #result = mapply(function(stds){
  #runmain(studlab=studylab,stids=unlist(stds))}, accIds)
  
  #data=data[data$studlab %in% accIds[[4]],]
 # m1=main(data, perarm, type, sm, tau.preset, comb.fixed, comb.random
 #                , t=data$t, r=data$r, n=data$n,  studlab=data$id)
  
return(studies)
  
  
  
}

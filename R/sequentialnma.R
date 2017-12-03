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
  
  uniqueids = unique(studies$id)
  accIds = mapply(function(i){rev(tail(rev(uniqueids),i))},4:length(uniqueids))
  
  studies1=studies[studies$id %in% accIds[[4]],]
  m1=main(data=studies1, perarm=perarm, type=type, sm=sm, tau.preset = tau.preset, comb.fixed, comb.random)
  
  runmain=function(x){main(data=studies[studies$id %in% x,], perarm=perarm, type=type, sm=sm, 
                           tau.preset=tau.preset, comb.fixed, comb.random)}
  
  result=mapply(runmain,accIds) 
   
   
  
  m2=main(data=studies, perarm=perarm, type=type, sm=sm, tau.preset = tau.preset, comb.fixed, comb.random)
  
  
  #runmain = function (studlab,stids) {
   #     m1=main(data, perarm, type, sm, tau.preset, comb.fixed, comb.random
    #        , t, r, n, y, sd, TE, seTE, t1, t2, studlab=stds,subset=stids)
    #return(m1)
  #}
 
  
  #accIds = Reduce(function(ac,id){append(ac,c(tail(ac,1),c(id)))},uniqueids,list(c()))
  
  
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
  

return(list(studies=studies, result=result, accIds=accIds))
  
  
  
}

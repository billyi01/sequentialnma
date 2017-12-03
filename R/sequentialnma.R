sequentialnma=function (data, perarm=T, type, sm, tau.preset = NULL, comb.fixed=F, comb.random=T 
                        , studlab="id",sortvar="year", t="t", r="r", n="n", y="y", sd="sd", TE="TE", seTE="seTE"
                        , t1="t1", t2="t2")
{
  library(netmeta)
  library(meta)
  library(plyr)
  library(caTools)
  
  #define arguments, correspond them to default names and sort them using formatdata function
  args =  unlist(as.list(match.call()));
  studies=formatdata (data,args)
  
  #define unique ids and create list with sequentially added study ids
  uniqueids = unique(studies$id)
  accIds = mapply(function(i){rev(tail(rev(uniqueids),i))},1:length(uniqueids))
  
  #define anticipated treatment effect
  delta=fordelta(data=studies, perarm=perarm, type=type, sm=sm, tau.preset = tau.preset, comb.fixed, comb.random)
  
  #run main function which performs sequential nma on the list of sequentially added ids
  runmain=function(x){main(data=studies[studies$id %in% x,], perarm=perarm, type=type, sm=sm, 
                           tau.preset=tau.preset, comb.fixed, comb.random, delta=delta)}
  result=mapply(runmain,accIds,SIMPLIFY = FALSE) 

  #run again the last step of sequential nma including all studies
  laststep=main(data=studies, perarm=perarm, type=type, sm=sm, 
                tau.preset = tau.preset, comb.fixed, comb.random, delta=delta)

  res=list(result=result,studies=studies,laststep=laststep)
  class(res)<-"sequentialnma"
  invisible(res)
}

###################################################################################################
#                   A function to perform sequential network meta-analysis
###################################################################################################
#       Arguments:
#data: a dataset in which the following arguments can be found: sortvar, studyid, t (or t1 and t2),
#n and r for binary outcomes, y, sd and n for continuous outcomes, TE and seTE for inverse variance data.
#perarm: a logical value indicating whether data are given as one treatment arm per row. 
#If TRUE the pairwise command is used to produce a dataset with one comparison per row.
#type: a character value indicating the type of the measured outcome, e.g. "binary", "continuous".
#sm: a character string indicating underlying summary measure, e.g. "OR", "RR", "RD", "MD", "SMD".
#tau.preset: an optional value for the square-root of the between-study variance τ^2. 
#If not specified heterogeneity is re-estimated at each step from the data.
#comb.fixed: A logical indicating whether a fixed effect meta-analysis should be conducted.
#comb.random: A logical indicating whether a random effects meta-analysis should be conducted.
#typeIerror: the type I error to be used in the calculations of the sequential boundaries
#power: the power to be used in the calculations of the sequential boundaries
#method: the method to be approximated in the alpha spending function to construct the sequential boundaries, 
#e.g. BF (O'Brien Flemming),"POC" (Pocock), "LIN" (Linear), "PFUN" (power function)
###################################################################################################

sequentialnma = function (data, perarm=T, type, sm, tau.preset = NULL, comb.fixed=F, comb.random=T 
                        , studlab="id",sortvar="year", t="t", r="r", n="n", y="y", sd="sd", TE="TE", seTE="seTE"
                        , t1="t1", t2="t2",typeIerror=0.05, power=0.9, method="BF")
{
  library(netmeta)
  library(meta)
  library(plyr)
  library(caTools)
  library(devtools) 
  
  #define arguments, correspond them to default names and sort them using formatdata function
  args =  unlist(as.list(match.call())); 
  studies=formatdata(data,args)
  
  #define unique ids and create list with sequentially added study ids
  uniqueids = unique(studies$id)
  accIds = mapply(function(i){rev(tail(rev(uniqueids),i))},1:length(uniqueids))
  
  #define anticipated treatment effect as the final NMA effect using fordelta function
  delta=fordelta(data=studies, perarm=perarm, type=type, sm=sm, tau.preset = tau.preset, comb.fixed, comb.random)
  
  #run main function which performs sequential nma on the list of sequentially added ids
  runmain = function(x){main(data=studies[studies$id %in% x,], perarm=perarm, type=type, sm=sm, 
                           tau.preset=tau.preset, comb.fixed, comb.random, delta=delta,
                           typeIerror=typeIerror, power=power, method=method)}
  
  result=mapply(runmain,accIds,SIMPLIFY = FALSE) 

  #run again the last step of sequential nma including all studies
  laststep=main(data=studies, perarm=perarm, type=type, sm=sm, 
                tau.preset = tau.preset, comb.fixed, comb.random, delta=delta,
                typeIerror=typeIerror, power=power, method=method)
  
  #vector with comparisons
  comparisons=rownames(result[[length(unique(studies$id))]])
  
  suppressWarnings({
    res=list(result=result,studies=studies,laststep=laststep, comparisons=comparisons);
    class(res)<-"sequentialnma"
  })
  return(res)
}

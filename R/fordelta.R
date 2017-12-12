#A function that performs NMA to the final step of the sequential process (entire dataset)
#and sets the anticipated treatment effect equal to final NMA estimates
#the input is a subset of the sequentialnma arguments
fordelta <- function(data, perarm=T, type, sm=sm, tau.preset = tau.preset, comb.fixed=F, comb.random=T){
  
  #perform network meta-analysis
  if (perarm & type=="binary"){
    Dpairs=pairwise(treat=t,event=r,n=n, data=data, studlab = id, sm= sm)
      metaNetw<-netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,
                        comb.fixed =F,comb.random = T,tol.multiarm=T,tau.preset = tau.preset)
  } 
  
  if (perarm & type=="continuous"){
    Dpairs=pairwise(treat=t,mean=y,sd=sd,n=n,data=data, studlab =id, sm=sm)
      metaNetw<-netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,
                        comb.fixed =F,comb.random = T,tol.multiarm=T,tau.preset = tau.preset)
  }
  
  if (!perarm){
      metaNetw=netmeta(TE,seTE,t1,t2,studlab=id,data=data,sm=sm,
                       comb.fixed =F,comb.random = T,tol.multiarm=T,tau.preset = tau.preset)
  }
  
    #store pairwise and network meta-analysis results
    if (comb.fixed){TE.nma=metaNetw$TE.fixed[lower.tri(metaNetw$TE.fixed)]}
    if (comb.random) {TE.nma=metaNetw$TE.random[lower.tri(metaNetw$TE.random)]}
    
    #set anticipated effect size equal to final nma
  sideSplit=netsplit(metaNetw)####!!!!!!!!!!!!!!!! auto giati xreizetai??? mono gia ta onomata ton comparisons? den einai ligo krima... einai argi function i netsplit
  delta=as.data.frame(TE.nma)
  rownames(delta) <- c(sideSplit$comparison)  
  
    return(delta)
}
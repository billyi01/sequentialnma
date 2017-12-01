sequentialnma=function (data, perarm=T, type, sm, tau.preset = NULL, comb.fixed=F, comb.random=T, 
                        sortvar, t=NA, r=NA, n=NA, y=NA, sd=NA, TE=NA, seTE=NA, t1=NA, t2=NA, studlab)
{
  library(netmeta)
  library(meta)
  library(plyr)
  library(caTools)
  
  #correspond variables 
  sortvar=eval(substitute(sortvar),data) #sortvar=data$year
  studlab=eval(substitute(studlab),data) #studlab=data$id
  if (perarm){
    t=eval(substitute(t),data)
    n=eval(substitute(n),data)
  }
  if (perarm & type=="binary"){
    r=eval(substitute(r),data)
  }
  if (perarm & type=="continuous"){
    y=eval(substitute(y),data)
    sd=eval(substitute(sd),data)
  }
  if (!perarm){
    TE=eval(substitute(TE),data)
    seTE=eval(substitute(seTE),data)
    t1=eval(substitute(t1),data)
    t2=eval(substitute(t2),data)
  }
  
  #sort everything according to sorting value
  sort<-!is.null(sortvar)
  if(!sort) sortvar<- 1:length(unique(studlab))
  o<-order(sortvar)
  t<-t[o]
  r<-r[o]
  n<-n[o]
  y<-y[o]
  sd<-sd[o]
  TE<-TE[o]
  seTE<-seTE[o]
  t1<-t1[o]
  t2<-t2[o]
  studylab<-studlab[o]

  m1=main(data, perarm, type, sm, tau.preset, comb.fixed, comb.random
           , t, r, n, y, sd, TE, seTE, t1, t2, studlab)
  
  return(m1)
  #return(list(sortvar=sortvar,studlab=studlab))
  
  
}

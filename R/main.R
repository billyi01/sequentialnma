############################################################################################################
##### A function that calculates the sequential quantities and boundaries at each step of the analysis #####
############################################################################################################
#the input is a subset of the sequentialnma arguments apart from the delta argument which is inputed by fordelta function
main <- function(data, perarm=T, type, sm=sm, tau.preset = tau.preset, comb.fixed=F, comb.random=T, delta=NA,
                 typeIerror=typeIerror, power=power, method=method){
  #perform network meta-analysis
  if (perarm & type=="binary"){
    Dpairs = suppressWarnings(
        pairwise(treat=t,event=r,n=n, data=data, studlab = id, sm = sm, warn=F)
        )
      checkconn = netconnection(treat1,treat2,studlab,data=Dpairs)
    if (checkconn$n.subnets==1){
      metaNetw = suppressWarnings(netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,
                         comb.fixed =F,comb.random = T,tol.multiarm=T,tau.preset = tau.preset
                        , warn = F)
      )
    }
  } 
  
  if (perarm & type=="continuous"){
    Dpairs=suppressWarnings(
      pairwise(treat=t,mean=y,sd=sd,n=n,data=data, studlab =id, sm=sm)
    )
    checkconn=netconnection(treat1,treat2,studlab,data=Dpairs)
    if (checkconn$n.subnets==1){
    metaNetw= suppressWarnings(netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,
                      comb.fixed =F,comb.random = T,tol.multiarm=T,tau.preset = tau.preset)
    )
    }
  }
  
  if (!perarm){
    checkconn=netconnection(t1,t2,studlab=id,data=data)
    if (checkconn$n.subnets==1){
    metaNetw=suppressWarnings(netmeta(TE,seTE,t1,t2,studlab=id,data=data,sm=sm,
                     comb.fixed =F,comb.random = T,tol.multiarm=T,tau.preset = tau.preset)
    )
    }
  }
  
  if (checkconn$n.subnets==1){
  #store pairwise and network meta-analysis results
  sideSplit=netsplit(metaNetw)
  if (comb.fixed){
    Direct=sideSplit$direct.fixed$TE
    DirectSE=sideSplit$direct.fixed$seTE
    DirectL=sideSplit$direct.fixed$lower
    DirectU=sideSplit$direct.fixed$upper
    TE.nma=metaNetw$TE.fixed[lower.tri(metaNetw$TE.fixed)]
    seTE.nma=metaNetw$seTE.fixed[lower.tri(metaNetw$seTE.fixed)]
    LCI.nma=metaNetw$lower.fixed[lower.tri(metaNetw$lower.fixed)]
    UCI.nma=metaNetw$upper.fixed[lower.tri(metaNetw$upper.fixed)]
  }
  
  if (comb.random){
    Direct=sideSplit$direct.random$TE
    DirectSE=sideSplit$direct.random$seTE
    DirectL=sideSplit$direct.random$lower
    DirectU=sideSplit$direct.random$upper
    TE.nma=metaNetw$TE.random[lower.tri(metaNetw$TE.random)]
    seTE.nma=metaNetw$seTE.random[lower.tri(metaNetw$seTE.random)]
    LCI.nma=metaNetw$lower.random[lower.tri(metaNetw$lower.random)]
    UCI.nma=metaNetw$upper.random[lower.tri(metaNetw$upper.random)]
  }
  
  input=cbind(c(Direct),c(DirectSE),c(DirectL),c(DirectU),c(TE.nma),c(seTE.nma),c(LCI.nma),c(UCI.nma))
  rownames(input) <- c(sideSplit$comparison)
  colnames(input) <- c("DirectTE","DirectSE","DirectL","DirectU",
                       "NetworkTE","NetworkSE","NetworkL","NetworkU")
  input=as.data.frame(input)
  #set anticipated effect size equal to final nma as estimated from fordelta function
  delta=as.data.frame(delta)
  delta = delta[rownames(input),]
  #calculate z scores and accumulated information
  Zpairw = input$DirectTE/input$DirectSE
  Ipairw = 1/input$DirectSE
  Zntw <- input$NetworkTE/input$NetworkSE
  Intw <- 1/input$NetworkSE
  ImaxPairw = ImaxNMA = abs((-qnorm(typeIerror/2)+ qnorm(power))/(delta))
  #fraction of accumulated information
  tallPairw = Ipairw/ImaxPairw
  tallNMA = Intw/ImaxNMA
  #calculation of sequential boundaries using alpha function
  SeqEffPairw = alpha(method = method, t = tallPairw, typeIerror=typeIerror)
  SeqEffNMA = alpha(method = method, t = tallNMA, typeIerror=typeIerror)
  #repeated confidence intervals
  LrciPairw = input$DirectTE - SeqEffPairw$E * input$DirectSE
  UrciPairw = input$DirectTE + SeqEffPairw$E * input$DirectSE
  LrciNMA = input$NetworkTE - SeqEffNMA$E * input$NetworkSE
  UrciNMA = input$NetworkTE + SeqEffNMA$E * input$NetworkSE
  #store sequential nma calculations together with nma results
  output=cbind.data.frame(input,delta=delta,DirectZscore=Zpairw,DirectI= Ipairw, NetworkZscore=Zntw, NetworkI=Intw,
                          DirectTaccum=tallPairw, NetworkTaccum=tallNMA, DirectAlpha=SeqEffPairw$at, 
                          DirectBoundary=SeqEffPairw$E, NetworkAlpha=SeqEffNMA$at, NetworkBoundary=SeqEffNMA$E, 
                          DirectLowerRCI=LrciPairw, DirectUpperRCI=UrciPairw, 
                          NetworkLowerRCI=LrciNMA, NetworkUpperRCI=UrciNMA)
  return(output)
  }
}



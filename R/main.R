main <- function(data, perarm=T, type, sm, tau.preset = NULL, comb.fixed=F, comb.random=T
                , t=NA, r=NA, n=NA, y=NA, sd=NA, TE=NA, seTE=NA, t1=NA, t2=NA, studlab){
  
  #network meta-analysis
  if (perarm & type=="binary"){
    Dpairs=pairwise(treat=t,event=r,n=n, data=data, studlab = studlab, sm= sm)
    metaNetw<-netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,
                      comb.fixed =F,comb.random = T,tol.multiarm=T,tau.preset = tau.preset)
  } 
  
  if (perarm & type=="continuous"){
    Dpairs=pairwise(treat=t,mean=y,sd=sd,n=n,data=data, studlab =studlab, sm=sm)
    metaNetw<-netmeta(TE,seTE,treat1,treat2,studlab,data=Dpairs,sm=sm,
                      comb.fixed =F,comb.random = T,tol.multiarm=T,tau.preset = tau.preset)
  }
  
  if (!perarm){
    metaNetw=netmeta(TE,seTE,t1,t2,studlab,data=data,sm=sm,
                     comb.fixed =F,comb.random = T,tol.multiarm=T,tau.preset = tau.preset)
  }
  
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
  
  #sequential
  delta=input$NetworkTE
  Zpairw = input$DirectTE/input$DirectSE
  Ipairw = 1/input$DirectSE
  Zntw <- input$NetworkTE/input$NetworkSE
  Intw <- 1/input$NetworkSE
  ImaxPairw = ImaxNMA = abs((-qnorm(0.05/(2), 0, 1, lower.tail = TRUE, 
                                    log.p = FALSE) + qnorm(1 - 0.1, 0, 1, lower.tail = TRUE, 
                                                           log.p = FALSE))/(delta))
  tallPairw = Ipairw/ImaxPairw
  tallNMA = Intw/ImaxNMA
  SeqEffPairw = alpha(method = "BF", t = tallPairw)
  SeqEffNMA = alpha(method = "BF", t = tallNMA)
  AtPairw = SeqEffPairw[, 2]
  EbPairw = SeqEffPairw[, 3]
  AtNMA = SeqEffNMA[, 2]
  EbNMA = SeqEffNMA[, 3]
  LrciPairw = input$DirectTE - EbPairw * input$DirectSE
  UrciPairw = input$DirectTE + EbPairw * input$DirectSE
  LrciNMA = input$NetworkTE - EbNMA * input$NetworkSE
  UrciNMA = input$NetworkTE + EbNMA * input$NetworkSE
  output = data.frame(input,delta, Zpairw, Ipairw, Zntw, Intw, 
                      tallPairw, tallNMA, AtPairw, EbPairw, AtNMA, EbNMA, 
                      LrciPairw, UrciPairw, LrciNMA, UrciNMA)
  colnames(output) <- c("DirectTE","DirectSE","DirectL","DirectU",
                        "NetworkTE","NetworkSE","NetworkL","NetworkU",
                        "delta","DirectZscore", "DirectI", "NetworkZscore", 
                        "NetworkI", "DirectTaccum", "NetworkTaccum", "DirectAlpha", 
                        "DirectBoundary", "NetworkAlpha", "NetworkBoundary", 
                        "DirectLowerRCI", "DirectUpperRCI", "NetworkLowerRCI", 
                        "NetworkUpperRCI")
  
  return(list(output=output))
}



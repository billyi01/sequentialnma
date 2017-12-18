##################################################################################################
### Presents the accumulated information through the sequential NMA for a specified comparison ###
##################################################################################################
# seqnmaobject: An object of class sequentialnma
# comparison: A character string defining the comparison for which the stopping framework is to be drawn.

summary.sequentialnma=function(seqnmaobject,comparison){
  
  if (!inherits(seqnmaobject, "sequentialnma"))
    stop("Argument 'seqnmaobject' must be an object of class \"sequentialnma\"")

  cat(paste("Cumulative network meta-analysis of",length(unique(seqnmaobject$studies$id)),
            "studies published between", min(seqnmaobject$studies$year), "and", max(seqnmaobject$studies$year),
            sep=" "))
  
  cat(sep="\n")
  
  NetwTE=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkTE"])},
                       1:length(seqnmaobject$result)))
  NetwseTE=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkSE"])},
                       1:length(seqnmaobject$result)))
  NetworkLCI=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkL"])},
                           1:length(seqnmaobject$result)))
  NetworkUCI=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkU"])},
                           1:length(seqnmaobject$result)))
  NetworkLRCI=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkLowerRCI"])},
                            1:length(seqnmaobject$result)))
  NetworkURCI=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkUpperRCI"])},
                            1:length(seqnmaobject$result)))
  NetworkZscore = unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkZscore"])},
                                1:length(seqnmaobject$result)))
  NetworkT = unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkTaccum"])},
                           1:length(seqnmaobject$result)))
  NetworkEfficacyB = unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkBoundary"])},
                                   1:length(seqnmaobject$result)))
  alphaNMA = unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkAlpha"])},
                                   1:length(seqnmaobject$result)))
  NetworkI = unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkI"])},
                           1:length(seqnmaobject$result)))
  NumberStudies=1:length(seqnmaobject$result)
  Year=mapply(function(i){(unique(seqnmaobject$studies$year[seqnmaobject$studies$id==unique(seqnmaobject$studies$id)[i]]))},
            1:length(seqnmaobject$result))
 
  resultcum=cbind.data.frame(Year=Year,NumberStudies=NumberStudies,
                             NMAEffect=NetwTE, NMAEffectSE=NetwseTE,
                             CILowNMA=NetworkLCI, CIUpNMA=NetworkUCI, 
                             RCILowNMA=NetworkLRCI, RCIUpNMA=NetworkURCI,
                             ZscoreNMA=NetworkEfficacyB, alphaNMA=alphaNMA,
                             InformationNMA=NetworkI, FractionInformationNMA=NetworkT)
  
  return(resultcum)
}
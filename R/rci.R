# Forest plot with repeated confidence intervals for pairwise and network meta-analysis
#
# This function draws a forest plot with repeated confidence intervals
# in the active graphics window for pairwise and network meta-analysis.

repeatedCI<-function(seqnmaobject,comparison,evidence,small.values="good"){

  library("ggplot2", lib.loc="~/R/win-library/3.2")

  if (!inherits(seqnmaobject, "sequentialnma"))
    stop("Argument 'seqnmaobject' must be an object of class \"sequentialnma\"")

  #####################set values#############################
  DirTE = unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"DirectTE"])},
                        1:length(seqnmaobject$result)))
  NetwTE=unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"NetworkTE"])},
                       1:length(seqnmaobject$result)))
  DirLCI=unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"DirectL"])},
                       1:length(seqnmaobject$result)))
  NetworkLCI=unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"NetworkL"])},
                           1:length(seqnmaobject$result)))
  DirectUCI=unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"DirectU"])},
                          1:length(seqnmaobject$result)))
  NetworkUCI=unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"NetworkU"])},
                           1:length(seqnmaobject$result)))
  DirectLRCI=unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"DirectLowerRCI"])},
                           1:length(seqnmaobject$result)))
  NetworkLRCI=unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"NetworkLowerRCI"])},
                            1:length(seqnmaobject$result)))
  DirectURCI=unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"DirectUpperRCI"])},
                           1:length(seqnmaobject$result)))
  NetworkURCI=unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"NetworkUpperRCI"])},
                            1:length(seqnmaobject$result)))
  steps=length(NetwTE):1
  Effects=data.frame(DirTE,NetwTE,steps)
  
  DirCI=c(DirLCI,DirectUCI)
  NetwCI=c(NetworkLCI,NetworkUCI)
  DirRCI=c(DirectLRCI,DirectURCI)
  NetwRCI=c(NetworkLRCI,NetworkURCI)
  DirEffect=c(DirTE,DirTE)
  NetwEffect=c(NetwTE,NetwTE)
  StepsForPlot=c(steps,steps)
  ForReapPlot=data.frame(DirCI,NetwCI,DirRCI,NetwRCI,DirEffect,NetwEffect,StepsForPlot)
  
  ############################forest plot with repeated confidence intervals################################

  if(evidence=="pairwise"){
    p1=ggplot(Effects)+
      geom_point(aes(Effects$DirTE,Effects$steps))
    p1=p1+geom_line(data=ForReapPlot,aes(DirRCI,StepsForPlot,group=StepsForPlot),colour="red")
    p1=p1+geom_line(data=ForReapPlot,aes(DirCI,StepsForPlot,group=StepsForPlot))
    p1=p1+geom_vline(xintercept = 0)
    p1

  }
  if(evidence=="network"){
    plot(c(-3,3), c(-length(NetworkTE),0), type = "n", xlab = " ", ylab = " ", asp =asp, xlim=c(-3,3), ylim=c(-length(NetworkTE),0))
    par(new=TRUE)

    for (i in 1:length(NetworkTE)){
      points(c(NetworkLRCI[i],NetworkURCI[i]),c(-i,-i),type="l", lty=2, lwd=0.1, col="black")
      points(c(NetworkLCI[i],NetworkUCI[i]),c(-i,-i),type="l", lwd=0.1, col="red")
      points(NetworkTE[i],(-i), col="black", cex=0.2, type="p", lwd=0.1)
    }
    abline(v=0)
    axis(1, at = seq(-1, 1, by = 1))
    if (is.na(outcome)){
      title(comparison)
    }
    if (!is.na(outcome)){
      if(outcome=="beneficial"){
        title(comparison, sub="Favors second              Favors first")

      }
      if(outcome=="harmful"){
        title(comparison, sub="Favors first               Favors second")
      }
    }
    }
  if(evidence=="both.separate"){
    par(mfrow=c(1,2))
    plot(c(-3,3), c(-length(DirectTE),0), type = "n", xlab = " ", ylab = " ", asp =asp, xlim=c(-3,3), ylim=c(-length(DirectTE),0))
    par(new=TRUE)
    for (i in 1:length(DirectTE)){
      points(c(DirectLRCI[i],DirectURCI[i]),c(-i,-i),type="l", lty=2, lwd=0.1, col="black")
      points(c(DirectLCI[i],DirectUCI[i]),c(-i,-i),type="l", lwd=0.1, col="blue")
      points(DirectTE[i],(-i), col="black", cex=0.2, lwd=0.1, type="p")
    }
    abline(v=0)
    axis(1, at = seq(-1, 1, by = 1))
    if (is.na(outcome)){
      title(comparison)
    }
    if (!is.na(outcome)){
      if(outcome=="beneficial"){
        title(comparison, sub="Favors second              Favors first")

      }
      if(outcome=="harmful"){
        title(comparison, sub="Favors first               Favors second")
      }
    }

    plot(c(-3,3), c(-length(NetworkTE),0), type = "n", xlab = " ", ylab = " ", asp =asp, xlim=c(-3,3), ylim=c(-length(NetworkTE),0))
    par(new=TRUE)
    for (i in 1:length(NetworkTE)){
      points(c(NetworkLRCI[i],NetworkURCI[i]),c(-i,-i),type="l", lty=2, lwd=0.1, col="black")
      points(c(NetworkLCI[i],NetworkUCI[i]),c(-i,-i),type="l", lwd=0.1, col="red")
      points(NetworkTE[i],(-i), col="black", cex=0.2, type="p", lwd=0.1)
    }
    abline(v=0)
    axis(1, at = seq(-1, 1, by = 1))
    if (is.na(outcome)){
      title(comparison)
    }
    if (!is.na(outcome)){
      if(outcome=="beneficial"){
        title(comparison, sub="Favors second              Favors first")

      }
      if(outcome=="harmful"){
        title(comparison, sub="Favors first               Favors second")
      }
    }
    }
  if(evidence=="both.together"){
    plot(c(-3,3), c(-length(DirectTE),0), type = "n", xlab = " ", ylab = " ", asp = asp, xlim=c(-3,3), ylim=c(-length(DirectTE),0))
    par(new=TRUE)

    for (i in 1:length(DirectTE)){
      points(c(DirectLRCI[i],DirectURCI[i]),c(-i,-i),type="l", lty=2, lwd=0.1, col="black")
      points(c(DirectLCI[i],DirectUCI[i]),c(-i,-i),type="l", lwd=0.1, col="blue")
      points(DirectTE[i],(-i), col="black", cex=0.2, lwd=0.1, type="p")
      points(c(NetworkLRCI[i],NetworkURCI[i]),c(-i-0.2,-i-0.2),type="l", lty=2, lwd=0.1, col="black")
      points(c(NetworkLCI[i],NetworkUCI[i]),c(-i-0.2,-i-0.2),type="l", lwd=0.1, col="red")
      points(NetworkTE[i],(-i-0.2), col="black", cex=0.2, type="p", lwd=0.1)
    }
    abline(v=0)
    axis(1, at = seq(-1, 1, by = 1))
    if (is.na(outcome)){
      title(comparison)
    }
    if (!is.na(outcome)){
      if(outcome=="beneficial"){
        title(comparison, sub="Favors second              Favors first")

      }
      if(outcome=="harmful"){
        title(comparison, sub="Favors first               Favors second")
      }
    }
  }
  p1
}

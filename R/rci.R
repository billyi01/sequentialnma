#' Forest plot with repeated confidence intervals for pairwise and network meta-analysis
#'
#' This function draws a forest plot with repeated confidence intervals
#' in the active graphics window for pairwise and network meta-analysis.
#' @usage S3 method for class livenma
#' @param x An object of class livenma
#' @param comparison The comparison for which the stopping framework to be drawn. It can be
#' specified as x$comparison[] whith the particular number withing the squared brackets or as
#' a string variable.
#' @param evidence A character string to indicate whether the stopping framework should be drawn
#' based on "pairwise", "network", "both.separate" or "both.together" evidence. "both.separate"
#' will draw two forest plots side by side where "both.together" will draw both pairwise and
#' network meta-anlysis results on the same forest plot. The later option may not be convenient
#' for large datasets.
#' @param asp An optional value to indicate the respective option for the plot command.
#' @param outcome An optional value to indicate whether the outcome is beneficial or harmful. If
#' specified, the respective direction is indicated in the plot.
#' @return A forest plot with repeated confidence intervals
#' @export

rci<-function(x,comparison,evidence,asp=0.01,outcome=NA){

  if (!inherits(x, "livenma"))
    stop("Argument 'x' must be an object of class \"livenma\"")

  #####################set values#############################

  TargetComp=comparison
  tc=min(which((x$comparison==TargetComp)== TRUE))
  D=x$D

  DirectTE=as.vector(x$Prosp[tc,2,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  NetworkTE=as.vector(x$Prosp[tc,5,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  DirectLCI=as.vector(x$Prosp[tc,2,1:(max(D$idyear)-(x$frn-2))],mode="numeric")-1.96*as.vector(x$Prosp[tc,3,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  NetworkLCI=as.vector(x$Prosp[tc,5,1:(max(D$idyear)-(x$frn-2))],mode="numeric")-1.96*as.vector(x$Prosp[tc,6,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  DirectUCI=as.vector(x$Prosp[tc,2,1:(max(D$idyear)-(x$frn-2))],mode="numeric")+1.96*as.vector(x$Prosp[tc,3,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  NetworkUCI=as.vector(x$Prosp[tc,5,1:(max(D$idyear)-(x$frn-2))],mode="numeric")+1.96*as.vector(x$Prosp[tc,6,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  DirectLRCI=as.vector(x$Prosp[tc,18,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  NetworkLRCI=as.vector(x$Prosp[tc,20,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  DirectURCI=as.vector(x$Prosp[tc,19,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  NetworkURCI=as.vector(x$Prosp[tc,21,1:(max(D$idyear)-(x$frn-2))],mode="numeric")

  DirectTE=DirectTE[!is.na(DirectTE)]
  NetworkTE=NetworkTE[!is.na(NetworkTE)]
  DirectLCI=DirectLCI[!is.na(DirectLCI)]
  NetworkLCI=NetworkLCI[!is.na(NetworkLCI)]
  DirectUCI=DirectUCI[!is.na(DirectUCI)]
  NetworkUCI=NetworkUCI[!is.na(NetworkUCI)]
  DirectLRCI=DirectLRCI[!is.na(DirectLRCI)]
  NetworkLRCI=NetworkLRCI[!is.na(NetworkLRCI)]
  DirectURCI=DirectURCI[!is.na(DirectURCI)]
  NetworkURCI=NetworkURCI[!is.na(NetworkURCI)]

  ############################forest plot with repeated confidence intervals################################

  if(evidence=="pairwise"){
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
}

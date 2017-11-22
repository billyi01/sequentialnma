#' Stopping framework for pairwise and network meta-analysis
#'
#' This function draws a stopping framework panel in the active graphics window for
#' pairwise and network meta-analysis.
#' @usage S3 method for class livenma
#' @param x An object of class livenma
#' @param comparison The comparison for which the stopping framework to be drawn. It can be
#' specified as x$comparison[] whith the particular number within the squared brackets or as
#' a string variable.
#' @param evidence A character string to indicate whether the stopping framework should be drawn
#' based on "pairwise", "network" or "both" evidence.
#' @param asp An optional value to indicate the respective option for the plot command.
#' @param outcome An optional value to indicate whether the outcome is beneficial or harmful. If
#' specified, the respective direction is indicated in the plot.
#' @return A stopping framework graph
#' @export

panel<-function(x,comparison,evidence,asp=1,outcome=NA){

  if (!inherits(x, "livenma"))
    stop("Argument 'x' must be an object of class \"livenma\"")

  #####################set values#############################

  TargetComp=comparison
  tc=min(which((x$comparison==TargetComp)== TRUE))
  D=x$D
  DirectZscore=as.vector(x$Prosp[tc,8,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  NetworkZscore=as.vector(x$Prosp[tc,10,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  DirectT=as.vector(x$Prosp[tc,12,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  NetworkT=as.vector(x$Prosp[tc,13,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  DirectEfficacyB=as.vector(x$Prosp[tc,15,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  NetworkEfficacyB=as.vector(x$Prosp[tc,17,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  DirectFutile=as.vector(x$Prosp[tc,22,1:(max(D$idyear)-(x$frn-2))],mode="numeric")
  NetworkFutile=as.vector(x$Prosp[tc,23,1:(max(D$idyear)-(x$frn-2))],mode="numeric")

  #####################stopping panel##########################################

  if(evidence=="pairwise"){
    #stopping framework for the target comparison (pairwise MA)
    plot(c(0,1), c(-5,5), type = "n", xlab = "t", ylab = " ", asp = asp, xlim=c(0,1), ylim=c(-5,5))
    par(new=TRUE)
    points(DirectT[!is.na(DirectT)],
           DirectZscore[!is.na(DirectT)], col="black",
           type="p", pch=19, cex=0.8)
    points(DirectT[!is.na(DirectT)],DirectEfficacyB[!is.na(DirectT)], col="black", type="l")
    points(DirectT[!is.na(DirectT)],-DirectEfficacyB[!is.na(DirectT)], col="black", type="l")
    #points(DirectT[!is.na(DirectT)],DirectFutile[!is.na(DirectT)],col="black",type="l", lty=2)
    #points(DirectT[!is.na(DirectT)],-DirectFutile[!is.na(DirectT)],col="black",type="l", lty=2)
    if (is.na(outcome)){
      title(comparison)
    }
    if (!is.na(outcome)){
      if(outcome=="beneficial"){
        title(comparison, ylab="Favors second     Favors first")

      }
      if(outcome=="harmful"){
        title(comparison, ylab="Favors first      Favors second")
      }
    }
  }
  if(evidence=="network"){
    #stopping framework for the target comparison (NMA)
    plot(c(0,1), c(-5,5), type = "n", xlab = "t", ylab = " ", asp = asp, xlim=c(0,1), ylim=c(-5,5))
    par(new=TRUE)
    points(NetworkT[!is.na(NetworkT)],
           NetworkZscore[!is.na(NetworkT)], col="black",
           type="p", pch=19, cex=0.8)
    points(NetworkT[!is.na(NetworkT)],NetworkEfficacyB[!is.na(NetworkT)], col="black", type="l")
    points(NetworkT[!is.na(NetworkT)],-NetworkEfficacyB[!is.na(NetworkT)], col="black", type="l")
    #points(NetworkT[!is.na(NetworkT)],NetworkFutile[!is.na(NetworkT)],col="black",type="l", lty=2)
    #points(NetworkT[!is.na(NetworkT)],-NetworkFutile[!is.na(NetworkT)],col="black",type="l", lty=2)
    axis(1, at = seq(0, 1, by = 0.1))
    if (is.na(outcome)){
      title(comparison)
    }
    if (!is.na(outcome)){
      if(outcome=="beneficial"){
        title(comparison, ylab="Favors second     Favors first")

      }
      if(outcome=="harmful"){
        title(comparison, ylab="Favors first      Favors second")
      }
    }
  }
  if(evidence=="both"){
    par(mfrow=c(1,2))
    plot(c(0,1), c(-5,5), type = "n", xlab = "t", ylab = " ", asp = asp, xlim=c(0,1), ylim=c(-5,5))
    par(new=TRUE)
    points(DirectT[!is.na(DirectT)],
           DirectZscore[!is.na(DirectT)], col="black",
           type="p", pch=19, cex=0.8)
    points(DirectT[!is.na(DirectT)],DirectEfficacyB[!is.na(DirectT)], col="black", type="l")
    points(DirectT[!is.na(DirectT)],-DirectEfficacyB[!is.na(DirectT)], col="black", type="l")
    if (is.na(outcome)){
      title(comparison)
    }
    if (!is.na(outcome)){
      if(outcome=="beneficial"){
        title(comparison, ylab="Favors second     Favors first")

      }
      if(outcome=="harmful"){
        title(comparison, ylab="Favors first      Favors second")
      }
    }
    plot(c(0,1), c(-5,5), type = "n", xlab = "t", ylab = " ", asp = asp, xlim=c(0,1), ylim=c(-5,5))
    par(new=TRUE)
    points(NetworkT[!is.na(NetworkT)],
           NetworkZscore[!is.na(NetworkT)], col="black",
           type="p", pch=19, cex=0.8)
    points(NetworkT[!is.na(NetworkT)],NetworkEfficacyB[!is.na(NetworkT)], col="black", type="l")
    points(NetworkT[!is.na(NetworkT)],-NetworkEfficacyB[!is.na(NetworkT)], col="black", type="l")
    #points(NetworkT[!is.na(NetworkT)],NetworkFutile[!is.na(NetworkT)],col="black",type="l", lty=2)
    #points(NetworkT[!is.na(NetworkT)],-NetworkFutile[!is.na(NetworkT)],col="black",type="l", lty=2)
    axis(1, at = seq(0, 1, by = 0.1))
    if (is.na(outcome)){
      title(comparison)
    }
    if (!is.na(outcome)){
      if(outcome=="beneficial"){
        title(comparison, ylab="Favors second     Favors first")

      }
      if(outcome=="harmful"){
        title(comparison, ylab="Favors first      Favors second")
      }
    }
  }

}

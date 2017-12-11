# Stopping framework for pairwise and network meta-analysis
#
# This function draws the panel with z-scores and stopping boundaries
# in the active graphics window for pairwise and network meta-analysis.
# seqnmaobject: An object of class sequentialnma
# comparison: A character string defining the comparison for which the stopping framework is to be drawn. 
# evidence: A character string to indicate whether the stopping framework 
# should be drawn based on "pairwise", "network" or "both" evidence.
# small.values: A character string specifying whether small treatment effects indicate a "good" or "bad" effect

plot.sequentialnma=function (seqnmaobject,comparison,evidence,small.values=NA) 
{
  library(ggplot2)
  library(grid)
  
  if (!inherits(seqnmaobject, "sequentialnma")) 
    stop("Argument 'seqnmaobject' must be an object of class \"sequentialnma\"")
  
  DirectZscore = unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"DirectZscore"])},
                               1:length(seqnmaobject$result)))
  NetworkZscore = unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"NetworkZscore"])},
                                1:length(seqnmaobject$result)))
  DirectT = unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"DirectTaccum"])},
                          1:length(seqnmaobject$result)))
  NetworkT = unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"NetworkTaccum"])},
                           1:length(seqnmaobject$result)))
  DirectEfficacyB = unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"DirectBoundary"])},
                                  1:length(seqnmaobject$result)))
  NetworkEfficacyB = unlist(mapply(function(i){(seqnmaobject$result[[i]]$output[comparison,"NetworkBoundary"])},
                                   1:length(seqnmaobject$result)))

  ForStopFramPlot=data.frame(DirectZscore,NetworkZscore,DirectT,NetworkT,DirectEfficacyB,NetworkEfficacyB)
  
  ############################stopping framework################################
  
  if (evidence == "pairwise"){
    p <- ggplot(ForStopFramPlot)+
      geom_point(aes(ForStopFramPlot$DirectT,ForStopFramPlot$DirectZscore))
    p=p+geom_line(aes(ForStopFramPlot$DirectT,ForStopFramPlot$DirectEfficacyB))
    p=p+geom_line(aes(ForStopFramPlot$DirectT,-ForStopFramPlot$DirectEfficacyB))
    p=p + geom_vline(xintercept = 1)
    p=p +labs(title=comparison,
              x ="Fraction of maximum information")
    
    if(!is.na(small.values) && small.values=="good"){
      p=p +ylab(expression(atop("Z score", paste("Favors first         Favors second"))))
    }
    else if(!is.na(small.values) && small.values=="bad"){
      p=p +ylab(expression(atop("Z score", paste("Favors second         Favors first"))))
    }
    else if(is.na(small.values)){
      p=p +ylab("Z score")
    }
    
  }
  if (evidence == "network"){
    p <- ggplot(ForStopFramPlot)+
      geom_point(aes(ForStopFramPlot$NetworkT,ForStopFramPlot$NetworkZscore))
    p=p+geom_line(aes(ForStopFramPlot$NetworkT,ForStopFramPlot$NetworkEfficacyB))
    p=p+geom_line(aes(ForStopFramPlot$NetworkT,-ForStopFramPlot$NetworkEfficacyB))
    p=p + geom_vline(xintercept = 1)
    p=p +labs(title=comparison,
              x ="Fraction of maximum information")
    
    if(!is.na(small.values) && small.values=="good"){
      p=p +ylab(expression(atop("Z score", paste("Favors first         Favors second"))))
    }
    else if(!is.na(small.values) && small.values=="bad"){
      p=p +ylab(expression(atop("Z score", paste("Favors second         Favors first"))))
    }
    else if(is.na(small.values)){
      p=p +ylab("Z score")
    }
    
  }
  if (evidence == "both"){
    vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
    arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
      dots <- list(...)
      n <- length(dots)
      if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
      if(is.null(nrow)) { nrow = ceiling(n/ncol)}
      if(is.null(ncol)) { ncol = ceiling(n/nrow)}
      ## NOTE see n2mfrow in grDevices for possible alternative
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
      ii.p <- 1
      for(ii.row in seq(1, nrow)){
        ii.table.row <- ii.row	
        if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
        for(ii.col in seq(1, ncol)){
          ii.table <- ii.p
          if(ii.p > n) break
          print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
          ii.p <- ii.p + 1
        }
      }
    }
    
    p1 <- ggplot(ForStopFramPlot)+
      geom_point(aes(ForStopFramPlot$DirectT,ForStopFramPlot$DirectZscore))
    p1=p1+geom_line(aes(ForStopFramPlot$DirectT,ForStopFramPlot$DirectEfficacyB))
    p1=p1+geom_line(aes(ForStopFramPlot$DirectT,-ForStopFramPlot$DirectEfficacyB))
    p1=p1 + geom_vline(xintercept = 1)
    p1=p1 +labs(title=comparison,
              x ="Fraction of maximum information")
    
    if(!is.na(small.values) && small.values=="good"){
      p1=p1 +ylab(expression(atop("Z score", paste("Favors first         Favors second"))))
    }
    else if(!is.na(small.values) && small.values=="bad"){
      p1=p1 +ylab(expression(atop("Z score", paste("Favors second         Favors first"))))
    }
    else if(is.na(small.values)){
      p1=p1 +ylab("Z score")
    }
    
    p2 <- ggplot(ForStopFramPlot)+
      geom_point(aes(ForStopFramPlot$NetworkT,ForStopFramPlot$NetworkZscore))
    p2=p2+geom_line(aes(ForStopFramPlot$NetworkT,ForStopFramPlot$NetworkEfficacyB))
    p2=p2+geom_line(aes(ForStopFramPlot$NetworkT,-ForStopFramPlot$NetworkEfficacyB))
    p2=p2 + geom_vline(xintercept = 1)
    p2=p2 +labs(title=comparison,
              x ="Fraction of maximum information")
    
    if(!is.na(small.values) && small.values=="good"){
      p2=p2 +ylab(expression(atop("Z score", paste("Favors first         Favors second"))))
    }
    else if(!is.na(small.values) && small.values=="bad"){
      p2=p2 +ylab(expression(atop("Z score", paste("Favors second         Favors first"))))
    }
    else if(is.na(small.values)){
      p2=p2 +ylab("Z score")
    }
    p=(arrange_ggplot2(p1,p2,nrow=1))
  }
  
  p
}

# Forest plot with repeated confidence intervals for pairwise and network meta-analysis
#
# This function draws a forest plot with repeated confidence intervals
# in the active graphics window for pairwise and network meta-analysis.
# seqnmaobject: An object of class sequentialnma
# comparison: A character string defining the comparison for which the stopping framework is to be drawn. 
# evidence: A character string to indicate whether the stopping framework 
# should be drawn based on "pairwise", "network", "both.separate" or "both.together" evidence.
# small.values: A character string specifying whether small treatment effects indicate a "good" or "bad" effect

repeatedCI<-function(seqnmaobject,comparison,evidence,small.values=NA){

  library(ggplot2)
  library(grid)
  
  if (!inherits(seqnmaobject, "sequentialnma"))
    stop("Argument 'seqnmaobject' must be an object of class \"sequentialnma\"")

  #####################set values#############################
  DirTE = unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"DirectTE"])},
                        1:length(seqnmaobject$result)))
  NetwTE=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkTE"])},
                       1:length(seqnmaobject$result)))
  DirLCI=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"DirectL"])},
                       1:length(seqnmaobject$result)))
  NetworkLCI=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkL"])},
                           1:length(seqnmaobject$result)))
  DirectUCI=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"DirectU"])},
                          1:length(seqnmaobject$result)))
  NetworkUCI=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkU"])},
                           1:length(seqnmaobject$result)))
  DirectLRCI=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"DirectLowerRCI"])},
                           1:length(seqnmaobject$result)))
  NetworkLRCI=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkLowerRCI"])},
                            1:length(seqnmaobject$result)))
  DirectURCI=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"DirectUpperRCI"])},
                           1:length(seqnmaobject$result)))
  NetworkURCI=unlist(mapply(function(i){(seqnmaobject$result[[i]][comparison,"NetworkUpperRCI"])},
                            1:length(seqnmaobject$result)))
  steps=length(NetwTE):1
  steps0=steps-0.2
  Effects=data.frame(DirTE,NetwTE,steps,steps0)
  
  DirCI=c(DirLCI,DirectUCI)
  NetwCI=c(NetworkLCI,NetworkUCI)
  DirRCI=c(DirectLRCI,DirectURCI)
  NetwRCI=c(NetworkLRCI,NetworkURCI)
  DirEffect=c(DirTE,DirTE)
  NetwEffect=c(NetwTE,NetwTE)
  StepsForPlot=c(steps,steps)
  StepsForPlot0=c(steps0,steps0)
  ForReapPlot=data.frame(DirCI,NetwCI,DirRCI,NetwRCI,DirEffect,NetwEffect,StepsForPlot,StepsForPlot0)
  
  ############################forest plot with repeated confidence intervals################################

  if(evidence=="pairwise"){
    p=ggplot(Effects)+
      geom_point(aes(Effects$DirTE,Effects$steps))
    p=p+geom_line(data=ForReapPlot,aes(DirRCI,StepsForPlot,group=StepsForPlot),colour="blue")
    p=p+geom_line(data=ForReapPlot,aes(DirCI,StepsForPlot,group=StepsForPlot))
    p=p+geom_vline(xintercept = 0)
    p=p +labs(title=comparison,
              x =" ", y = " ")
    p=p + theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())

  }
  if(evidence=="network"){
    p=ggplot(Effects)+
      geom_point(aes(Effects$NetwTE,Effects$steps))
    p=p+geom_line(data=ForReapPlot,aes(NetwRCI,StepsForPlot,group=StepsForPlot),colour="red")
    p=p+geom_line(data=ForReapPlot,aes(NetwCI,StepsForPlot,group=StepsForPlot))
    p=p+geom_vline(xintercept = 0)
    p=p +labs(title=comparison,
              x =" ", y = " ")
    p=p + theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())

    }
  if(evidence=="both.separate"){
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
    
    p1=ggplot(Effects)+
      geom_point(aes(Effects$DirTE,Effects$steps))
    p1=p1+geom_line(data=ForReapPlot,aes(DirRCI,StepsForPlot,group=StepsForPlot),colour="blue")
    p1=p1+geom_line(data=ForReapPlot,aes(DirCI,StepsForPlot,group=StepsForPlot))
    p1=p1+geom_vline(xintercept = 0)
    p1=p1 +labs(title=comparison,x =" ", y = " ")
    p1=p1 + theme(axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
    
    p2=ggplot(Effects)+
      geom_point(aes(Effects$NetwTE,Effects$steps))
    p2=p2+geom_line(data=ForReapPlot,aes(NetwRCI,StepsForPlot,group=StepsForPlot),colour="red")
    p2=p2+geom_line(data=ForReapPlot,aes(NetwCI,StepsForPlot,group=StepsForPlot))
    p2=p2+geom_vline(xintercept = 0)
    p2=p2 +labs(title=comparison,x =" ", y = " ")
    p2=p2 + theme(axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
    p=(arrange_ggplot2(p1,p2,nrow=1))
    }
  if(evidence=="both.together"){
    p=ggplot(Effects)+
      geom_point(aes(Effects$DirTE,Effects$steps))
    p=p+geom_line(data=ForReapPlot,aes(DirRCI,StepsForPlot,group=StepsForPlot),colour="blue")
    p=p+geom_line(data=ForReapPlot,aes(DirCI,StepsForPlot,group=StepsForPlot))
    p=p+geom_point(aes(Effects$NetwTE,Effects$steps0))
    p=p+geom_line(data=ForReapPlot,aes(NetwRCI,StepsForPlot0,group=StepsForPlot0),colour="red")
    p=p+geom_line(data=ForReapPlot,aes(NetwCI,StepsForPlot0,group=StepsForPlot0))
    p=p+geom_vline(xintercept = 0)
    p=p +labs(title=comparison,x =" ", y = " ")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  }
  if(!is.na(small.values) && small.values=="good" && evidence!="both.separate"){
    p=p +labs(caption  = (paste("Favors",strsplit(comparison,split=":")[[1]][2],sep = " ")))
    
    # Generate a ggplot2 plot grob
    pfin <- ggplotGrob(p)
    # Find the grob tree containing the right caption (as child)
    k <- which(pfin$layout$name=="caption")
    # Copy the "right caption" text grob in grbTxt
    grbTxt <- pfin$grobs[[k]]$children[[1]]
    
    # Modify content and position of the text grob  
    grbTxt$label <- paste("Favors",strsplit(comparison,split=":")[[1]][1],sep = " ")
    grbTxt$name <- "GRID.text.left"
    grbTxt$x <- unit(0,"npc")
    grbTxt$hjust <- 0
    grbTxt$gp$col <- "black"
    
    # Add grbTxt (left caption) to the title grob containing the right caption
    pfin$grobs[[k]] <- addGrob(pfin$grobs[[k]],grbTxt)
    grid.draw(pfin)
  }
  else if(!is.na(small.values) && small.values=="bad" && evidence!="both.separate"){
    p=p +labs(caption  = (paste("Favors",strsplit(comparison,split=":")[[1]][1],sep = " ")))
    
    # Generate a ggplot2 plot grob
    pfin <- ggplotGrob(p)
    # Find the grob tree containing the right caption (as child)
    k <- which(pfin$layout$name=="caption")
    # Copy the "right caption" text grob in grbTxt
    grbTxt <- pfin$grobs[[k]]$children[[1]]
    
    # Modify content and position of the text grob  
    grbTxt$label <- paste("Favors",strsplit(comparison,split=":")[[1]][2],sep = " ")
    grbTxt$name <- "GRID.text.left"
    grbTxt$x <- unit(0,"npc")
    grbTxt$hjust <- 0
    grbTxt$gp$col <- "black"
    
    # Add grbTxt (left caption) to the title grob containing the right caption
    pfin$grobs[[k]] <- addGrob(pfin$grobs[[k]],grbTxt)
    grid.draw(pfin)
  }
  else if(is.na(small.values) | evidence=="both.separate"){pfin=p}
  pfin
}





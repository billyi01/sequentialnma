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
      geom_point(aes(Effects$DirTE,Effects$steps),na.rm = T)
    p=p+geom_line(data=ForReapPlot,aes(DirRCI,StepsForPlot,group=StepsForPlot),colour="blue",na.rm = T)
    p=p+geom_line(data=ForReapPlot,aes(DirCI,StepsForPlot,group=StepsForPlot),na.rm = T)
    p=p+geom_vline(xintercept = 0)

    p=p + theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
    
    p=p +labs(title=paste("Forest plot with repeated confidence intervals for",comparison,
                          "based on pairwise meta-analysis", sep=" "))
    
    if(seqnmaobject$sm=="OR" | seqnmaobject$sm=="RR" | seqnmaobject$sm=="HR"){
      if(!is.na(small.values) && small.values=="good"){
        p=p +labs(x =paste("log", seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][1]), y = " ")
      }
      else if(!is.na(small.values) && small.values=="bad"){
        p=p +labs(x =paste("log",seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][2]), y = " ")
      }
      else if(is.na(small.values)){
        p=p +labs(x=paste("log",seqnmaobject$sm))
      }
    }
    if(seqnmaobject$sm=="MD" | seqnmaobject$sm=="SMD"){
      if(!is.na(small.values) && small.values=="good"){
        p=p +labs(x =paste(seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][1]), y = " ")
      }
      else if(!is.na(small.values) && small.values=="bad"){
        p=p +labs(x =paste(seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][2]), y = " ")
      }
      else if(is.na(small.values)){
        p=p +labs(x=seqnmaobject$sm)
      }
    }
    
  }
  if(evidence=="network"){
    p=ggplot(Effects)+
      geom_point(aes(Effects$NetwTE,Effects$steps),na.rm = T)
    p=p+geom_line(data=ForReapPlot,aes(NetwRCI,StepsForPlot,group=StepsForPlot),colour="red",na.rm = T)
    p=p+geom_line(data=ForReapPlot,aes(NetwCI,StepsForPlot,group=StepsForPlot),na.rm = T)
    p=p+geom_vline(xintercept = 0)

    p=p + theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
    p=p +labs(title=paste("Forest plot with repeated confidence intervals for",comparison,
                          "based on network meta-analysis", sep=" "))
    if(seqnmaobject$sm=="OR" | seqnmaobject$sm=="RR" | seqnmaobject$sm=="HR"){
      if(!is.na(small.values) && small.values=="good"){
        p=p +labs(x =paste("log", seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][1]), y = " ")
      }
      else if(!is.na(small.values) && small.values=="bad"){
        p=p +labs(x =paste("log",seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][2]), y = " ")
      }
      else if(is.na(small.values)){
        p=p +labs(x=paste("log",seqnmaobject$sm))
      }
    }
    if(seqnmaobject$sm=="MD" | seqnmaobject$sm=="SMD"){
      if(!is.na(small.values) && small.values=="good"){
        p=p +labs(x =paste(seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][1]), y = " ")
      }
      else if(!is.na(small.values) && small.values=="bad"){
        p=p +labs(x =paste(seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][2]), y = " ")
      }
      else if(is.na(small.values)){
        p=p +labs(x=seqnmaobject$sm)
      }
    }

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
      geom_point(aes(Effects$DirTE,Effects$steps),na.rm = T)
    p1=p1+geom_line(data=ForReapPlot,aes(DirRCI,StepsForPlot,group=StepsForPlot),colour="blue",na.rm = T)
    p1=p1+geom_line(data=ForReapPlot,aes(DirCI,StepsForPlot,group=StepsForPlot),na.rm = T)
    p1=p1+geom_vline(xintercept = 0)
    p1=p1 + theme(axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
    p1=p1 +labs(title=paste("Pairwise meta-analysis",comparison, sep=" "))
    if(seqnmaobject$sm=="OR" | seqnmaobject$sm=="RR" | seqnmaobject$sm=="HR"){
      if(!is.na(small.values) && small.values=="good"){
        p1=p1 +labs(x =paste("log", seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][1]), y = " ")
      }
      else if(!is.na(small.values) && small.values=="bad"){
        p1=p1 +labs(x =paste("log",seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][2]), y = " ")
      }
      else if(is.na(small.values)){
        p1=p1 +labs(x=paste("log",seqnmaobject$sm))
      }
    }
    if(seqnmaobject$sm=="MD" | seqnmaobject$sm=="SMD"){
      if(!is.na(small.values) && small.values=="good"){
        p1=p1 +labs(x =paste(seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][1]), y = " ")
      }
      else if(!is.na(small.values) && small.values=="bad"){
        p1=p1 +labs(x =paste(seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][2]), y = " ")
      }
      else if(is.na(small.values)){
        p1=p1 +labs(x=seqnmaobject$sm)
      }
    }
    
    p2=ggplot(Effects)+
      geom_point(aes(Effects$NetwTE,Effects$steps),na.rm = T)
    p2=p2+geom_line(data=ForReapPlot,aes(NetwRCI,StepsForPlot,group=StepsForPlot),colour="red",na.rm = T)
    p2=p2+geom_line(data=ForReapPlot,aes(NetwCI,StepsForPlot,group=StepsForPlot),na.rm = T)
    p2=p2+geom_vline(xintercept = 0)
    p2=p2 + theme(axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
    p2=p2 +labs(title=paste("Network meta-analysis",comparison, sep=" "))
    if(seqnmaobject$sm=="OR" | seqnmaobject$sm=="RR" | seqnmaobject$sm=="HR"){
      if(!is.na(small.values) && small.values=="good"){
        p2=p2 +labs(x =paste("log", seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][1]), y = " ")
      }
      else if(!is.na(small.values) && small.values=="bad"){
        p2=p2 +labs(x =paste("log",seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][2]), y = " ")
      }
      else if(is.na(small.values)){
        p2=p2 +labs(x=paste("log",seqnmaobject$sm))
      }
    }
    if(seqnmaobject$sm=="MD" | seqnmaobject$sm=="SMD"){
      if(!is.na(small.values) && small.values=="good"){
        p2=p2 +labs(x =paste(seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][1]), y = " ")
      }
      else if(!is.na(small.values) && small.values=="bad"){
        p2=p2 +labs(x =paste(seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][2]), y = " ")
      }
      else if(is.na(small.values)){
        p2=p2 +labs(x=seqnmaobject$sm)
      }
    }
    p=(arrange_ggplot2(p1,p2,nrow=1))
    }
  if(evidence=="both.together"){
    p=ggplot(Effects)+
      geom_point(aes(Effects$DirTE,Effects$steps),na.rm = T)
    p=p+geom_line(data=ForReapPlot,aes(DirRCI,StepsForPlot,group=StepsForPlot),colour="blue",na.rm = T)
    p=p+geom_line(data=ForReapPlot,aes(DirCI,StepsForPlot,group=StepsForPlot),na.rm = T)
    p=p+geom_point(aes(Effects$NetwTE,Effects$steps0),na.rm = T)
    p=p+geom_line(data=ForReapPlot,aes(NetwRCI,StepsForPlot0,group=StepsForPlot0),colour="red",na.rm = T)
    p=p+geom_line(data=ForReapPlot,aes(NetwCI,StepsForPlot0,group=StepsForPlot0),na.rm = T)
    p=p+geom_vline(xintercept = 0)
    p=p+theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    p=p+theme(plot.title = element_text(hjust = 0.5))
    p=p +labs(title= paste("Forest plot with repeated confidence intervals for",
                           comparison,"based on pairwise and network meta-analysis", sep="\n"))

    if(seqnmaobject$sm=="OR" | seqnmaobject$sm=="RR" | seqnmaobject$sm=="HR"){
        if(!is.na(small.values) && small.values=="good"){
          p=p +labs(x =paste("log", seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][1]), y = " ")
        }
        else if(!is.na(small.values) && small.values=="bad"){
          p=p +labs(x =paste("log",seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][2]), y = " ")
        }
        else if(is.na(small.values)){
          p=p +labs(x=paste("log",seqnmaobject$sm))
        }
      }
    if(seqnmaobject$sm=="MD" | seqnmaobject$sm=="SMD"){
        if(!is.na(small.values) && small.values=="good"){
          p=p +labs(x =paste(seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][1]), y = " ")
        }
        else if(!is.na(small.values) && small.values=="bad"){
          p=p +labs(x =paste(seqnmaobject$sm, "smaller than 0 favors",strsplit(comparison,split=":")[[1]][2]), y = " ")
        }
        else if(is.na(small.values)){
          p=p +labs(x=seqnmaobject$sm)
        }
      }
  }
  
  p
}





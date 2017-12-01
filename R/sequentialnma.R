sequentialnma=function (data, level, type, effsize, tau.sq = NA,delta=NA)
{
  d <- DataInput(data, level, type, effsize)
  mafinal <- metaanalysis(D = d$D, tau.sq, level = d$level, 
                          type = d$type, effsize = d$effsize)
  if (is.na(delta)) {
    delta = mafinal$Mntw
  }
  ma <- metaanalysis(D = d$D, tau.sq, level = d$level, type = d$type, 
                     effsize = d$effsize)
  Tr = ma$T
  MaToInput1 <- MaToInput(ma, mafinal)
  input = MaToInput1$input
  pr1 <- prospective(Tr, input)
  frn = 3
  D = d$D
  Prosp = array(data = NA, dim = c(length(mafinal$CompNtw), 
                                   23, ((max(D$idyear) + 1 - frn + 1))))
  for (i in frn:(max(D$idyear) + 1)) {
    ma <- metaanalysis(subset(D, idyear < i), tau.sq, type, 
                       level, effsize)
    invisible(ma)
    MaToInput1 <- MaToInput(ma, mafinal)
    input <- MaToInput1$input
    pr <- prospective(T = ma$T, input = input)
    Prosp[, , i - (frn - 1)] = as.matrix(pr$output, mode = "numeric")
  }
  output = data.frame(pr1$output)
  colnames(output) <- c("ComparisonPairw", "DirectTE", "DirectSE", 
                        "ComparisonNetw", "NetworkTE", "NetworkSE", "delta", 
                        "DirectZscore", "DirectI", "NetworkZscore", "NetworkI", 
                        "DirectT", "NetworkT", "DirectAlpha", "DirectEfficacyB", 
                        "NetworkAlpha", "NetworkEfficacyB", "DirectLowerRCI", 
                        "DirectUpperRCI", "NetworkLowerRCI", "NetworkUpperRCI", 
                        "DirectFutile", "NetworkFutile")
  output2 = data.frame(pr1$output2)
  colnames(output2) <- c("TreatID", "Treatment(viaComparison)", 
                         "SUCRA")
  result = invisible(list(output = output, comparison = mafinal$CompNtwA, 
                          D = D, output2 = output2, Prosp = Prosp, frn = frn))
  class(result) <- "sequentialnma"
  print(result)
}

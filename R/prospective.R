prospective <- function(Tr, input) {
  Zpairw = input$DirectTE/input$DirectSE
  Ipairw = 1/input$DirectSE
  Zntw <- input$NetworkTE/input$NetworkSE
  Intw <- 1/input$NetworkSE
  ImaxPairw = ImaxNMA = abs((-qnorm(0.05/(2), 0, 1, lower.tail = TRUE, 
                                    log.p = FALSE) + qnorm(1 - 0.1, 0, 1, lower.tail = TRUE, 
                                                           log.p = FALSE))/(input$delta))
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
  FuPairw = delta * Ipairw - 1.959964
  FuNMA = delta * Intw - 1.959964
  draws = 10000
  adjSE = (EbNMA * input$NetworkSE)/(1.959964)
  adjVarMntw = adjSE^2
  x <- mapply(rnorm, mean = input$NetworkTE[1:(Tr - 1)], 
              sd = adjVarMntw[1:(Tr - 1)], n = draws)
  x1 = matrix(cbind(rep(0, draws), x), draws, Tr)
  rankings <- Tr - t(apply(x1, 1, rank)) + 1
  MeanRank <- apply(rankings, 2, mean)
  SUCRAmr <- (Tr - MeanRank)/(Tr - 1)
  output = data.frame(input, Zpairw, Ipairw, Zntw, Intw, 
                      tallPairw, tallNMA, AtPairw, EbPairw, AtNMA, EbNMA, 
                      LrciPairw, UrciPairw, LrciNMA, UrciNMA, FuPairw, 
                      FuNMA)
  colnames(output) <- c("ComparisonPairw", "DirectTE", 
                        "DirectSE", "ComparisonNetw", "NetworkTE", "NetworkSE", 
                        "delta", "DirectZscore", "DirectI", "NetworkZscore", 
                        "NetworkI", "DirectT", "NetworkT", "DirectAlpha", 
                        "DirectEfficacyB", "NetworkAlpha", "NetworkEfficacyB", 
                        "DirectLowerRCI", "DirectUpperRCI", "NetworkLowerRCI", 
                        "NetworkUpperRCI", "DirectFutile", "NetworkFutile")
  output2 = data.frame(seq(1, Tr), c("Reference", as.character(output$ComparisonNetw[1:Tr - 
                                                                                       1])), SUCRAmr)
  colnames(output2) <- c("TreatID", "Treatment(viaComparison)", 
                         "SUCRA")
  invisible(list(output = output, output2 = output2))
}

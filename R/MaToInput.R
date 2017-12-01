MaToInput <- function(ma, mafinal) {
  CompPairw1 = Mpairw1 = seMpairw1 = CompNetw1 = Mnetw1 = seMnetw1 = rep(0, 
                                                                         length(mafinal$CompNtw))
  for (i in 1:length(ma$Mpairw)) {
    for (j in 1:length(mafinal$CompNtw)) {
      if (ma$CompPairw[i] == mafinal$CompNtw[j]) {
        CompPairw1[j] = mafinal$CompNtwA[j]
        Mpairw1[j] = -ma$Mpairw[i]
        seMpairw1[j] = ma$seMpairw[i]
      }
    }
  }
  for (i in 1:length(ma$Mpairw)) {
    for (j in 1:length(mafinal$CompNtw)) {
      if (ma$CompPairw[i] == mafinal$CompNtwA[j]) {
        CompPairw1[j] = mafinal$CompNtwA[j]
        Mpairw1[j] = ma$Mpairw[i]
        seMpairw1[j] = ma$seMpairw[i]
      }
    }
  }
  for (i in 1:length(ma$Mntw)) {
    for (j in 1:length(mafinal$CompNtw)) {
      if (ma$CompNtw[i] == mafinal$CompNtw[j]) {
        CompNetw1[j] = mafinal$CompNtwA[j]
        Mnetw1[j] = ma$Mntw[i]
        seMnetw1[j] = ma$seMntw[i]
      }
    }
  }
  is.na(CompPairw1)[CompPairw1 == 0] <- TRUE
  is.na(Mpairw1)[is.na(CompPairw1)] <- TRUE
  is.na(seMpairw1)[is.na(CompPairw1)] <- TRUE
  is.na(CompNetw1)[CompNetw1 == 0] <- TRUE
  is.na(Mnetw1)[is.na(CompNetw1)] <- TRUE
  is.na(seMnetw1)[is.na(CompNetw1)] <- TRUE
  input = data.frame(CompPairw1, Mpairw1, seMpairw1, CompNetw1, 
                     Mnetw1, seMnetw1, delta)
  colnames(input) <- c("Comparison Pairw", "DirectTE", 
                       "DirectSE", "Comparison Netw", "NetworkTE", "NetworkSE", 
                       "delta")
  invisible(list(input = input))
}

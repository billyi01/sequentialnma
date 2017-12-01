metaanalysis <- function(D, tau.sq, type, level, effsize) {
  comp <- paste(D$treat1, rep("vs", length(D$id)), D$treat2)
  metaNetw = NULL
  if (!is.na(tau.sq)) {
    if (level == "arm" & type == "binary" & effsize == 
        "OR") {
      metaD <- metabin(c(D$r1), c(D$n1), c(D$r2), c(D$n2), 
                       sm = "OR", method = "I", allstudies = T)
      metaPairw <- metagen(metaD$TE, metaD$seTE, sm = "OR", 
                           tau.common = TRUE, tau.preset = sqrt(tau.sq), 
                           byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaD$TE, metaD$seTE, D$treat1, 
                          D$treat2, sm = "OR", comb.random = TRUE, studlab = D$idyear, 
                          tau.preset = sqrt(tau.sq))
    }
    if (level == "arm" & type == "binary" & effsize == 
        "RR") {
      metaD <- metabin(c(D$r1), c(D$n1), c(D$r2), c(D$n2), 
                       sm = "RR", method = "I", allstudies = T)
      metaPairw <- metagen(metaD$TE, metaD$seTE, sm = "RR", 
                           tau.common = TRUE, tau.preset = sqrt(tau.sq), 
                           byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaD$TE, metaD$seTE, D$treat1, 
                          D$treat2, sm = "RR", comb.random = TRUE, studlab = D$idyear, 
                          tau.preset = sqrt(tau.sq))
    }
    if (level == "arm" & type == "continuous" & effsize == 
        "MD") {
      metaD <- metacont(c(D$n1), c(D$y1), c(D$sd1), 
                        c(D$n2), c(D$y2), c(D$sd2), sm = "MD")
      metaPairw <- metagen(metaD$TE, metaD$seTE, sm = "MD", 
                           tau.common = TRUE, tau.preset = sqrt(tau.sq), 
                           byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaD$TE, metaD$seTE, D$treat1, 
                          D$treat2, sm = "MD", comb.random = TRUE, studlab = D$idyear, 
                          tau.preset = sqrt(tau.sq), tol.multiarm = 1)
    }
    if (level == "arm" & type == "continuous" & effsize == 
        "SMD") {
      metaD <- metacont(c(D$n1), c(D$y1), c(D$sd1), 
                        c(D$n2), c(D$y2), c(D$sd2), sm = "SMD")
      metaPairw <- metagen(metaD$TE, metaD$seTE, sm = "SMD", 
                           tau.common = TRUE, tau.preset = sqrt(tau.sq), 
                           byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaD$TE, metaD$seTE, D$treat1, 
                          D$treat2, sm = "SMD", comb.random = TRUE, studlab = D$idyear, 
                          tau.preset = sqrt(tau.sq), tol.multiarm = 1)
    }
    if (level == "study" & type == "binary" & effsize == 
        "OR") {
      metaPairw <- metagen(D$effect, D$se, sm = "OR", 
                           tau.common = TRUE, tau.preset = sqrt(tau.sq), 
                           byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaPairw$TE, metaPairw$seTE, 
                          D$treat1, D$treat2, sm = "OR", comb.random = TRUE, 
                          studlab = D$idyear, tau.preset = sqrt(tau.sq))
    }
    if (level == "study" & type == "binary" & effsize == 
        "RR") {
      metaPairw <- metagen(D$effect, D$se, sm = "RR", 
                           tau.common = TRUE, tau.preset = sqrt(tau.sq), 
                           byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaPairw$TE, metaPairw$seTE, 
                          D$treat1, D$treat2, sm = "RR", comb.random = TRUE, 
                          studlab = D$idyear, tau.preset = sqrt(tau.sq))
    }
    if (level == "study" & type == "continuous" & effsize == 
        "MD") {
      metaPairw <- metagen(D$effect, D$se, sm = "MD", 
                           tau.common = TRUE, tau.preset = sqrt(tau.sq), 
                           byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaPairw$TE, metaPairw$seTE, 
                          D$treat1, D$treat2, sm = "MD", comb.random = TRUE, 
                          studlab = D$idyear, tau.preset = sqrt(tau.sq), 
                          tol.multiarm = T)
    }
    if (level == "study" & type == "continuous" & effsize == 
        "SMD") {
      metaPairw <- metagen(D$effect, D$se, sm = "SMD", 
                           tau.common = TRUE, tau.preset = sqrt(tau.sq), 
                           byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaPairw$TE, metaPairw$seTE, 
                          D$treat1, D$treat2, sm = "SMD", comb.random = TRUE, 
                          studlab = D$idyear, tau.preset = sqrt(tau.sq), 
                          tol.multiarm = T)
    }
    if (level == "study" & type == "timetoevent" & effsize == 
        "HR") {
      metaPairw <- metagen(D$effect, D$se, sm = "HR", 
                           tau.common = TRUE, tau.preset = sqrt(tau.sq), 
                           byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaPairw$TE, metaPairw$seTE, 
                          D$treat1, D$treat2, sm = "HR", comb.random = TRUE, 
                          studlab = D$idyear, tau.preset = sqrt(tau.sq))
    }
  }
  if (is.na(tau.sq)) {
    if (level == "arm" & type == "binary" & effsize == 
        "OR") {
      metaD <- metabin(c(D$r1), c(D$n1), c(D$r2), c(D$n2), 
                       sm = "OR", method = "I", allstudies = T)
      metaPairw <- metagen(metaD$TE, metaD$seTE, sm = "OR", 
                           tau.common = TRUE, byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaD$TE, metaD$seTE, D$treat1, 
                          D$treat2, sm = "OR", comb.random = TRUE, studlab = D$idyear)
    }
    if (level == "arm" & type == "binary" & effsize == 
        "RR") {
      metaD <- metabin(c(D$r1), c(D$n1), c(D$r2), c(D$n2), 
                       sm = "RR", method = "I", allstudies = T)
      metaPairw <- metagen(metaD$TE, metaD$seTE, sm = "RR", 
                           tau.common = TRUE, byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaD$TE, metaD$seTE, D$treat1, 
                          D$treat2, sm = "RR", comb.random = TRUE, studlab = D$idyear)
    }
    if (level == "arm" & type == "continuous" & effsize == 
        "MD") {
      metaD <- metacont(c(D$n1), c(D$y1), c(D$sd1), 
                        c(D$n2), c(D$y2), c(D$sd2), sm = "MD")
      metaPairw <- metagen(metaD$TE, metaD$seTE, sm = "MD", 
                           tau.common = TRUE, byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaD$TE, metaD$seTE, D$treat1, 
                          D$treat2, sm = "MD", comb.random = TRUE, studlab = D$idyear)
    }
    if (level == "arm" & type == "continuous" & effsize == 
        "SMD") {
      metaD <- metacont(c(D$n1), c(D$y1), c(D$sd1), 
                        c(D$n2), c(D$y2), c(D$sd2), sm = "SMD")
      metaPairw <- metagen(metaD$TE, metaD$seTE, sm = "SMD", 
                           tau.common = TRUE, byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaD$TE, metaD$seTE, D$treat1, 
                          D$treat2, sm = "SMD", comb.random = TRUE, studlab = D$idyear)
    }
    if (level == "study" & type == "binary" & effsize == 
        "OR") {
      metaPairw <- metagen(D$effect, D$se, sm = "OR", 
                           tau.common = TRUE, byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaPairw$TE, metaPairw$seTE, 
                          D$treat1, D$treat2, sm = "OR", comb.random = TRUE, 
                          studlab = D$idyear)
    }
    if (level == "study" & type == "binary" & effsize == 
        "RR") {
      metaPairw <- metagen(D$effect, D$se, sm = "RR", 
                           tau.common = TRUE, byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaPairw$TE, metaPairw$seTE, 
                          D$treat1, D$treat2, sm = "RR", comb.random = TRUE, 
                          studlab = D$idyear)
    }
    if (level == "study" & type == "continuous" & effsize == 
        "MD") {
      metaPairw <- metagen(D$effect, D$se, sm = "MD", 
                           tau.common = TRUE, byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaPairw$TE, metaPairw$seTE, 
                          D$treat1, D$treat2, sm = "MD", comb.random = TRUE, 
                          studlab = D$idyear)
    }
    if (level == "study" & type == "continuous" & effsize == 
        "SMD") {
      metaPairw <- metagen(D$effect, D$se, sm = "SMD", 
                           tau.common = TRUE, byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaPairw$TE, metaPairw$seTE, 
                          D$treat1, D$treat2, sm = "SMD", comb.random = TRUE, 
                          studlab = D$idyear)
    }
    if (level == "study" & type == "timetoevent" & effsize == 
        "HR") {
      metaPairw <- metagen(D$effect, D$se, sm = "HR", 
                           tau.common = TRUE, byvar = comp)
      CompPairw = metaPairw$bylevs
      metaNetw <- netmeta(metaPairw$TE, metaPairw$seTE, 
                          D$treat1, D$treat2, sm = "HR", comb.random = TRUE, 
                          studlab = D$idyear)
    }
  }
  PotComB <- combs(c(rownames(metaNetw$TE.random)), 2)
  CompNtw <- paste(PotComB[, 2], rep("vs", length(PotComB)/2), 
                   PotComB[, 1])
  CompNtwA <- paste(PotComB[, 1], rep("vs", length(PotComB)/2), 
                    PotComB[, 2])
  noc = factorial(metaNetw$n)/(factorial(2) * factorial(metaNetw$n - 
                                                          2))
  noc == length(CompNtw)
  Mpairw = metaPairw$TE.random.w
  seMpairw = metaPairw$seTE.random.w
  Mntw = seMntw = Zntw = Intw = rep(0, noc)
  Tr = metaNetw$n
  if (Tr > 2) {
    u1 = u2 = rep(0, Tr - 1)
    u2[1] = Tr - 1
    for (i in 2:(Tr - 1)) {
      u2[i] = u2[i - 1] + (Tr - i)
    }
    u1[1] = 1
    for (i in 2:(Tr - 1)) {
      u1[i] = u1[i - 1] + (Tr - (i - 1))
    }
    for (i in 1:(Tr - 1)) {
      Mntw[u1[i]:u2[i]] = (metaNetw$TE.random[i, ][(i + 
                                                      1):Tr])
      seMntw[u1[i]:u2[i]] = (metaNetw$seTE.random[i, 
                                                  ][(i + 1):Tr])
    }
  }
  if (Tr == 2) {
    Mntw = Mpairw
    seMntw = seMntw
  }
  invisible(list(Tr = Tr, CompPairw = CompPairw, Mpairw = round(Mpairw, 
                                                                3), seMpairw = round(seMpairw, 3), CompNtw = CompNtw, 
                 CompNtwA = CompNtwA, Mntw = round(Mntw, 3), seMntw = round(seMntw, 
                                                                            3)))
}

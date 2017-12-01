DataInput <- function(data, level, type, effsize) {
  if (level == "arm" & type == "binary") {
    data <- data[order(data$id), ]
    data$arm <- sequence(tabulate(data$id))
    dataw = reshape(data, idvar = c("year", "study", 
                                    "id"), v.names = c("t", "n", "r"), timevar = c("arm"), 
                    direction = "wide")
    dataw <- dataw[order(dataw$year), ]
    dataw$idyear <- seq(1:max(dataw$id))
    x = c(dataw$idyear[dataw$t.3 != "NA"])
    y = x[!is.na(x)]
    D = NULL
    if (length(dataw$t.3) == 0 & length(dataw$t.4) == 
        0 & length(dataw$t.5) == 0) {
      D = data.frame(as.vector(c(dataw$year), mode = "numeric"), 
                     c(c(as.character(dataw$study))), as.vector(c(dataw$id), 
                                                                mode = "numeric"), c(c(as.character(dataw$t.1))), 
                     as.vector(c(dataw$n.1), mode = "numeric"), 
                     as.vector(c(dataw$r.1), mode = "numeric"), 
                     c(c(as.character(dataw$t.2))), as.vector(c(dataw$n.2), 
                                                              mode = "numeric"), as.vector(c(dataw$r.2), 
                                                                                           mode = "numeric"), as.vector(c(dataw$idyear), 
                                                                                                                        mode = "numeric"))
    }
    if (length(dataw$t.3) > 0 & length(dataw$t.4) == 
        0 & length(dataw$t.5) == 0) {
      aa = array(0, c(2, 10, length(y)))
      a = matrix(0, length(y) * 2, 10)
      for (i in 1:length(y)) {
        aa[1, , i] = as.matrix(subset(dataw, idyear == 
                                        y[i], select = c(year, study, id, t.1, n.1, 
                                                         r.1, t.3, n.3, r.3, idyear)))
        aa[2, , i] = as.matrix(subset(dataw, idyear == 
                                        y[i], select = c(year, study, id, t.2, n.2, 
                                                         r.2, t.3, n.3, r.3, idyear)))
      }
      for (i in 1:length(y)) {
        a[i, ] = aa[1, , i]
        a[i + length(y), ] = aa[2, , i]
      }
      x1 = c(dataw$idyear[dataw$t.4 != "NA"])
      y1 = x1[!is.na(x1)]
      D = data.frame(as.vector(c(dataw$year, a[, 1]), 
                               mode = "numeric"), c(c(as.character(dataw$study)), 
                                                    c(as.character(a[, 2]))), as.vector(c(dataw$id, 
                                                                                          a[, 3]), mode = "numeric"), c(c(as.character(dataw$t.1)), 
                                                                                                                        c(as.character(a[, 4]))), as.vector(c(dataw$n.1, 
                                                                                                                                                              a[, 5]), mode = "numeric"), as.vector(c(dataw$r.1, 
                                                                                                                                                                                                      a[, 6]), mode = "numeric"), c(c(as.character(dataw$t.2)), 
                                                                                                                                                                                                                                    c(as.character(a[, 7]))), as.vector(c(dataw$n.2, 
                                                                                                                                                                                                                                                                          a[, 8]), mode = "numeric"), as.vector(c(dataw$r.2, 
                                                                                                                                                                                                                                                                                                                  a[, 9]), mode = "numeric"), as.vector(c(dataw$idyear, 
                                                                                                                                                                                                                                                                                                                                                          a[, 10]), mode = "numeric"))
    }
    if (length(dataw$t.3) > 0 & length(dataw$t.4) > 0 & 
        length(dataw$t.5) == 0) {
      x = c(dataw$idyear[dataw$t.3 != "NA"])
      y = x[!is.na(x)]
      D = NULL
      aa = array(0, c(2, 10, length(y)))
      a = matrix(0, length(y) * 2, 10)
      for (i in 1:length(y)) {
        aa[1, , i] = as.matrix(subset(dataw, idyear == 
                                        y[i], select = c(year, study, id, t.1, n.1, 
                                                         r.1, t.3, n.3, r.3, idyear)))
        aa[2, , i] = as.matrix(subset(dataw, idyear == 
                                        y[i], select = c(year, study, id, t.2, n.2, 
                                                         r.2, t.3, n.3, r.3, idyear)))
      }
      for (i in 1:length(y)) {
        a[i, ] = aa[1, , i]
        a[i + length(y), ] = aa[2, , i]
      }
      x1 = c(dataw$idyear[dataw$t.4 != "NA"])
      y1 = x1[!is.na(x1)]
      D = NULL
      aa1 = array(0, c(3, 10, length(y1)))
      a1 = matrix(0, length(y1) * 3, 10)
      for (i in 1:length(y1)) {
        aa1[1, , i] = as.matrix(subset(dataw, idyear == 
                                         y1[i], select = c(year, study, id, t.1, n.1, 
                                                           r.1, t.4, n.4, r.4, idyear)))
        aa1[2, , i] = as.matrix(subset(dataw, idyear == 
                                         y1[i], select = c(year, study, id, t.2, n.2, 
                                                           r.2, t.4, n.4, r.4, idyear)))
        aa1[3, , i] = as.matrix(subset(dataw, idyear == 
                                         y1[i], select = c(year, study, id, t.3, n.3, 
                                                           r.3, t.4, n.4, r.4, idyear)))
      }
      for (i in 1:length(y1)) {
        a1[i, ] = aa1[1, , i]
        a1[i + length(y1), ] = aa1[2, , i]
        a1[i + length(y1) + length(y1), ] = aa1[3, 
                                                , i]
      }
      x5 = c(dataw$idyear[dataw$t.5 != "NA"])
      y5 = x1[!is.na(x5)]
      D = data.frame(as.vector(c(dataw$year, a[, 1], 
                                 a1[, 1]), mode = "numeric"), c(c(as.character(dataw$study)), 
                                                                c(as.character(a[, 2])), c(as.character(a1[, 
                                                                                                           2]))), as.vector(c(dataw$id, a[, 3], a1[, 
                                                                                                                                                   3]), mode = "numeric"), c(c(as.character(dataw$t.1)), 
                                                                                                                                                                             c(as.character(a[, 4])), c(as.character(a1[, 
                                                                                                                                                                                                                        4]))), as.vector(c(dataw$n.1, a[, 5], a1[, 
                                                                                                                                                                                                                                                                 5]), mode = "numeric"), as.vector(c(dataw$r.1, 
                                                                                                                                                                                                                                                                                                     a[, 6], a1[, 6]), mode = "numeric"), c(c(as.character(dataw$t.2)), 
                                                                                                                                                                                                                                                                                                                                            c(as.character(a[, 7])), c(as.character(a1[, 
                                                                                                                                                                                                                                                                                                                                                                                       7]))), as.vector(c(dataw$n.2, a[, 8], a1[, 
                                                                                                                                                                                                                                                                                                                                                                                                                                8]), mode = "numeric"), as.vector(c(dataw$r.2, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    a[, 9], a1[, 9]), mode = "numeric"), as.vector(c(dataw$idyear, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     a[, 10], a1[, 10]), mode = "numeric"))
    }
    if (length(dataw$t.3) > 0 & length(dataw$t.4) > 0 & 
        length(dataw$t.5) > 0) {
      x = c(dataw$idyear[dataw$t.3 != "NA"])
      y = x[!is.na(x)]
      D = NULL
      aa = array(0, c(2, 10, length(y)))
      a = matrix(0, length(y) * 2, 10)
      for (i in 1:length(y)) {
        aa[1, , i] = as.matrix(subset(dataw, idyear == 
                                        y[i], select = c(year, study, id, t.1, n.1, 
                                                         r.1, t.3, n.3, r.3, idyear)))
        aa[2, , i] = as.matrix(subset(dataw, idyear == 
                                        y[i], select = c(year, study, id, t.2, n.2, 
                                                         r.2, t.3, n.3, r.3, idyear)))
      }
      for (i in 1:length(y)) {
        a[i, ] = aa[1, , i]
        a[i + length(y), ] = aa[2, , i]
      }
      x1 = c(dataw$idyear[dataw$t.4 != "NA"])
      y1 = x1[!is.na(x1)]
      aa1 = array(0, c(3, 10, length(y1)))
      a1 = matrix(0, length(y1) * 3, 10)
      for (i in 1:length(y1)) {
        aa1[1, , i] = as.matrix(subset(dataw, idyear == 
                                         y1[i], select = c(year, study, id, t.1, n.1, 
                                                           r.1, t.4, n.4, r.4, idyear)))
        aa1[2, , i] = as.matrix(subset(dataw, idyear == 
                                         y1[i], select = c(year, study, id, t.2, n.2, 
                                                           r.2, t.4, n.4, r.4, idyear)))
        aa1[3, , i] = as.matrix(subset(dataw, idyear == 
                                         y1[i], select = c(year, study, id, t.3, n.3, 
                                                           r.3, t.4, n.4, r.4, idyear)))
      }
      for (i in 1:length(y1)) {
        a1[i, ] = aa1[1, , i]
        a1[i + length(y1), ] = aa1[2, , i]
        a1[i + length(y1) + length(y1), ] = aa1[3, 
                                                , i]
      }
      x5 = c(dataw$idyear[dataw$t.5 != "NA"])
      y5 = x1[!is.na(x5)]
      aa5 = array(0, c(4, 10, length(y5)))
      a5 = matrix(0, length(y5) * 4, 10)
      for (i in 1:length(y5)) {
        aa5[1, , i] = as.matrix(subset(dataw, idyear == 
                                         y5[i], select = c(year, study, id, t.1, n.1, 
                                                           r.1, t.5, n.5, r.5, idyear)))
        aa5[2, , i] = as.matrix(subset(dataw, idyear == 
                                         y5[i], select = c(year, study, id, t.2, n.2, 
                                                           r.2, t.5, n.5, r.5, idyear)))
        aa5[3, , i] = as.matrix(subset(dataw, idyear == 
                                         y5[i], select = c(year, study, id, t.3, n.3, 
                                                           r.3, t.5, n.5, r.5, idyear)))
        aa5[4, , i] = as.matrix(subset(dataw, idyear == 
                                         y5[i], select = c(year, study, id, t.4, n.4, 
                                                           r.4, t.5, n.5, r.5, idyear)))
      }
      for (i in 1:length(y5)) {
        a5[i, ] = aa5[1, , i]
        a5[i + length(y5), ] = aa5[2, , i]
        a5[i + length(y5) + length(y5), ] = aa5[3, 
                                                , i]
        a5[i + length(y5) + length(y5) + length(y5), 
           ] = aa5[4, , i]
      }
      D = data.frame(as.vector(c(dataw$year, a[, 1], 
                                 a1[, 1], a5[, 1]), mode = "numeric"), c(c(as.character(dataw$study)), 
                                                                         c(as.character(a[, 2])), c(as.character(a1[, 
                                                                                                                    2])), c(as.character(a5[, 2]))), as.vector(c(dataw$id, 
                                                                                                                                                                 a[, 3], a1[, 3], a5[, 3]), mode = "numeric"), 
                     c(c(as.character(dataw$t.1)), c(as.character(a[, 
                                                                    4])), c(as.character(a1[, 4])), c(as.character(a5[, 
                                                                                                                      4]))), as.vector(c(dataw$n.1, a[, 5], a1[, 
                                                                                                                                                               5], a5[, 5]), mode = "numeric"), as.vector(c(dataw$r.1, 
                                                                                                                                                                                                            a[, 6], a1[, 6], a5[, 6]), mode = "numeric"), 
                     c(c(as.character(dataw$t.2)), c(as.character(a[, 
                                                                    7])), c(as.character(a1[, 7])), c(as.character(a5[, 
                                                                                                                      7]))), as.vector(c(dataw$n.2, a[, 8], a1[, 
                                                                                                                                                               8], a5[, 8]), mode = "numeric"), as.vector(c(dataw$r.2, 
                                                                                                                                                                                                            a[, 9], a1[, 9], a5[, 9]), mode = "numeric"), 
                     as.vector(c(dataw$idyear, a[, 10], a1[, 10], 
                                 a5[, 10]), mode = "numeric"))
    }
    colnames(D) <- c("year", "study", "id", "treat1", 
                     "n1", "r1", "treat2", "n2", "r2", "idyear")
  }
  if (level == "arm" & type == "continuous") {
    data <- data[order(data$id), ]
    data$arm <- sequence(tabulate(data$id))
    dataw = reshape(data, idvar = c("year", "study", 
                                    "id"), v.names = c("t", "y", "sd", "n"), timevar = c("arm"), 
                    direction = "wide")
    dataw <- dataw[order(dataw$year), ]
    dataw$idyear <- seq(1:max(dataw$id))
    x = c(dataw$idyear[dataw$t.3 != "NA"])
    y = x[!is.na(x)]
    D = NULL
    if (length(dataw$t.3) == 0 & length(dataw$t.4) == 
        0 & length(dataw$t.5) == 0) {
      D = data.frame(as.vector(c(dataw$year), mode = "numeric"), 
                     c(c(as.character(dataw$study))), as.vector(c(dataw$id), 
                                                                mode = "numeric"), c(c(as.character(dataw$t.1))), 
                     as.vector(c(dataw$y.1), mode = "numeric"), 
                     as.vector(c(dataw$sd.1), mode = "numeric"), 
                     as.vector(c(dataw$n.1), mode = "numeric"), 
                     c(c(as.character(dataw$t.2))), as.vector(c(dataw$y.2), 
                                                              mode = "numeric"), as.vector(c(dataw$sd.2), 
                                                                                           mode = "numeric"), as.vector(c(dataw$n.2), 
                                                                                                                        mode = "numeric"), as.vector(c(dataw$idyear), 
                                                                                                                                                     mode = "numeric"))
    }
    if (length(dataw$t.3) > 0 & length(dataw$t.4) == 
        0 & length(dataw$t.5) == 0) {
      aa = array(0, c(2, 12, length(y)))
      a = matrix(0, length(y) * 2, 12)
      for (i in 1:length(y)) {
        aa[1, , i] = as.matrix(subset(dataw, idyear == 
                                        y[i], select = c(year, study, id, t.1, y.1, 
                                                         sd.1, n.1, t.3, y.3, sd.3, n.3, idyear)))
        aa[2, , i] = as.matrix(subset(dataw, idyear == 
                                        y[i], select = c(year, study, id, t.2, y.2, 
                                                         sd.2, n.2, t.3, y.3, sd.3, n.3, idyear)))
      }
      for (i in 1:length(y)) {
        a[i, ] = aa[1, , i]
        a[i + length(y), ] = aa[2, , i]
      }
      x1 = c(dataw$idyear[dataw$t.4 != "NA"])
      y1 = x1[!is.na(x1)]
      D = data.frame(as.vector(c(dataw$year, a[, 1]), 
                               mode = "numeric"), c(c(as.character(dataw$study)), 
                                                    c(as.character(a[, 2]))), as.vector(c(dataw$id, 
                                                                                          a[, 3]), mode = "numeric"), c(c(as.character(dataw$t.1)), 
                                                                                                                        c(as.character(a[, 4]))), as.vector(c(dataw$y.1, 
                                                                                                                                                              a[, 5]), mode = "numeric"), as.vector(c(dataw$sd.1, 
                                                                                                                                                                                                      a[, 6]), mode = "numeric"), as.vector(c(dataw$n.1, 
                                                                                                                                                                                                                                              a[, 7]), mode = "numeric"), c(c(as.character(dataw$t.2)), 
                                                                                                                                                                                                                                                                            c(as.character(a[, 8]))), as.vector(c(dataw$y.2, 
                                                                                                                                                                                                                                                                                                                  a[, 9]), mode = "numeric"), as.vector(c(dataw$sd.2, 
                                                                                                                                                                                                                                                                                                                                                          a[, 10]), mode = "numeric"), as.vector(c(dataw$n.2, 
                                                                                                                                                                                                                                                                                                                                                                                                   a[, 11]), mode = "numeric"), as.vector(c(dataw$idyear, 
                                                                                                                                                                                                                                                                                                                                                                                                                                            a[, 12]), mode = "numeric"))
    }
    if (length(dataw$t.3) > 0 & length(dataw$t.4) > 0 & 
        length(dataw$t.5) == 0) {
      aa = array(0, c(2, 12, length(y)))
      a = matrix(0, length(y) * 2, 12)
      for (i in 1:length(y)) {
        aa[1, , i] = as.matrix(subset(dataw, idyear == 
                                        y[i], select = c(year, study, id, t.1, y.1, 
                                                         sd.1, n.1, t.3, y.3, sd.3, n.3, idyear)))
        aa[2, , i] = as.matrix(subset(dataw, idyear == 
                                        y[i], select = c(year, study, id, t.2, y.2, 
                                                         sd.2, n.2, t.3, y.3, sd.3, n.3, idyear)))
      }
      for (i in 1:length(y)) {
        a[i, ] = aa[1, , i]
        a[i + length(y), ] = aa[2, , i]
      }
      x1 = c(dataw$idyear[dataw$t.4 != "NA"])
      y1 = x1[!is.na(x1)]
      aa1 = array(0, c(3, 12, length(y1)))
      a1 = matrix(0, length(y1) * 3, 12)
      for (i in 1:length(y1)) {
        aa1[1, , i] = as.matrix(subset(dataw, idyear == 
                                         y1[i], select = c(year, study, id, t.1, y.1, 
                                                           sd.1, n.1, t.4, y.4, sd.4, n.4, idyear)))
        aa1[2, , i] = as.matrix(subset(dataw, idyear == 
                                         y1[i], select = c(year, study, id, t.2, y.2, 
                                                           sd.2, n.2, t.4, y.4, sd.4, n.4, idyear)))
        aa1[3, , i] = as.matrix(subset(dataw, idyear == 
                                         y1[i], select = c(year, study, id, t.3, y.3, 
                                                           sd.3, n.3, t.4, y.4, sd.4, n.4, idyear)))
      }
      for (i in 1:length(y1)) {
        a1[i, ] = aa1[1, , i]
        a1[i + length(y1), ] = aa1[2, , i]
        a1[i + length(y1) + length(y1), ] = aa1[3, 
                                                , i]
      }
      x5 = c(dataw$idyear[dataw$t.5 != "NA"])
      y5 = x1[!is.na(x5)]
      D = data.frame(as.vector(c(dataw$year, a[, 1], 
                                 a1[, 1]), mode = "numeric"), c(c(as.character(dataw$study)), 
                                                                c(as.character(a[, 2])), c(as.character(a1[, 
                                                                                                           2]))), as.vector(c(dataw$id, a[, 3], a1[, 
                                                                                                                                                   3]), mode = "numeric"), c(c(as.character(dataw$t.1)), 
                                                                                                                                                                             c(as.character(a[, 4])), c(as.character(a1[, 
                                                                                                                                                                                                                        4]))), as.vector(c(dataw$y.1, a[, 5], a1[, 
                                                                                                                                                                                                                                                                 5]), mode = "numeric"), as.vector(c(dataw$sd.1, 
                                                                                                                                                                                                                                                                                                     a[, 6], a1[, 6]), mode = "numeric"), as.vector(c(dataw$n.1, 
                                                                                                                                                                                                                                                                                                                                                      a[, 7], a1[, 7]), mode = "numeric"), c(c(as.character(dataw$t.2)), 
                                                                                                                                                                                                                                                                                                                                                                                             c(as.character(a[, 8])), c(as.character(a1[, 
                                                                                                                                                                                                                                                                                                                                                                                                                                        8]))), as.vector(c(dataw$y.2, a[, 9], a1[, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 9]), mode = "numeric"), as.vector(c(dataw$sd.2, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     a[, 10], a1[, 10]), mode = "numeric"), as.vector(c(dataw$n.2, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        a[, 11], a1[, 11]), mode = "numeric"), as.vector(c(dataw$idyear, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           a[, 12], a1[, 12]), mode = "numeric"))
    }
    if (length(dataw$t.3) > 0 & length(dataw$t.4) > 0 & 
        length(dataw$t.5) > 0) {
      aa = array(0, c(2, 12, length(y)))
      a = matrix(0, length(y) * 2, 12)
      for (i in 1:length(y)) {
        aa[1, , i] = as.matrix(subset(dataw, idyear == 
                                        y[i], select = c(year, study, id, t.1, y.1, 
                                                         sd.1, n.1, t.3, y.3, sd.3, n.3, idyear)))
        aa[2, , i] = as.matrix(subset(dataw, idyear == 
                                        y[i], select = c(year, study, id, t.2, y.2, 
                                                         sd.2, n.2, t.3, y.3, sd.3, n.3, idyear)))
      }
      for (i in 1:length(y)) {
        a[i, ] = aa[1, , i]
        a[i + length(y), ] = aa[2, , i]
      }
      x1 = c(dataw$idyear[dataw$t.4 != "NA"])
      y1 = x1[!is.na(x1)]
      aa1 = array(0, c(3, 12, length(y1)))
      a1 = matrix(0, length(y1) * 3, 12)
      for (i in 1:length(y1)) {
        aa1[1, , i] = as.matrix(subset(dataw, idyear == 
                                         y1[i], select = c(year, study, id, t.1, y.1, 
                                                           sd.1, n.1, t.4, y.4, sd.4, n.4, idyear)))
        aa1[2, , i] = as.matrix(subset(dataw, idyear == 
                                         y1[i], select = c(year, study, id, t.2, y.2, 
                                                           sd.2, n.2, t.4, y.4, sd.4, n.4, idyear)))
        aa1[3, , i] = as.matrix(subset(dataw, idyear == 
                                         y1[i], select = c(year, study, id, t.3, y.3, 
                                                           sd.3, n.3, t.4, y.4, sd.4, n.4, idyear)))
      }
      for (i in 1:length(y1)) {
        a1[i, ] = aa1[1, , i]
        a1[i + length(y1), ] = aa1[2, , i]
        a1[i + length(y1) + length(y1), ] = aa1[3, 
                                                , i]
      }
      x5 = c(dataw$idyear[dataw$t.5 != "NA"])
      y5 = x1[!is.na(x5)]
      aa5 = array(0, c(4, 12, length(y5)))
      a5 = matrix(0, length(y5) * 4, 12)
      for (i in 1:length(y5)) {
        aa5[1, , i] = as.matrix(subset(dataw, idyear == 
                                         y5[i], select = c(year, study, id, t.1, y.1, 
                                                           sd.1, n.1, t.5, y.5, sd.5, n.5, idyear)))
        aa5[2, , i] = as.matrix(subset(dataw, idyear == 
                                         y5[i], select = c(year, study, id, t.2, y.2, 
                                                           sd.2, n.2, t.5, y.5, sd.5, n.5, idyear)))
        aa5[3, , i] = as.matrix(subset(dataw, idyear == 
                                         y5[i], select = c(year, study, id, t.3, y.3, 
                                                           sd.3, n.3, t.5, y.5, sd.5, n.5, idyear)))
        aa5[4, , i] = as.matrix(subset(dataw, idyear == 
                                         y5[i], select = c(year, study, id, t.4, y.4, 
                                                           sd.4, n.4, t.5, y.5, sd.5, n.5, idyear)))
      }
      for (i in 1:length(y5)) {
        a5[i, ] = aa5[1, , i]
        a5[i + length(y5), ] = aa5[2, , i]
        a5[i + length(y5) + length(y5), ] = aa5[3, 
                                                , i]
        a5[i + length(y5) + length(y5) + length(y5), 
           ] = aa5[4, , i]
      }
      D = data.frame(as.vector(c(dataw$year, a[, 1], 
                                 a1[, 1], a5[, 1]), mode = "numeric"), c(c(as.character(dataw$study)), 
                                                                         c(as.character(a[, 2])), c(as.character(a1[, 
                                                                                                                    2])), c(as.character(a5[, 2]))), as.vector(c(dataw$id, 
                                                                                                                                                                 a[, 3], a1[, 3], a5[, 3]), mode = "numeric"), 
                     c(c(as.character(dataw$t.1)), c(as.character(a[, 
                                                                    4])), c(as.character(a1[, 4])), c(as.character(a5[, 
                                                                                                                      4]))), as.vector(c(dataw$y.1, a[, 5], a1[, 
                                                                                                                                                               5], a5[, 5]), mode = "numeric"), as.vector(c(dataw$sd.1, 
                                                                                                                                                                                                            a[, 6], a1[, 6], a5[, 6]), mode = "numeric"), 
                     as.vector(c(dataw$n.1, a[, 7], a1[, 7], a5[, 
                                                                7]), mode = "numeric"), c(c(as.character(dataw$t.2)), 
                                                                                          c(as.character(a[, 8])), c(as.character(a1[, 
                                                                                                                                     8])), c(as.character(a5[, 8]))), as.vector(c(dataw$y.2, 
                                                                                                                                                                                  a[, 9], a1[, 9], a5[, 9]), mode = "numeric"), 
                     as.vector(c(dataw$sd.2, a[, 10], a1[, 10], 
                                 a5[, 10]), mode = "numeric"), as.vector(c(dataw$n.2, 
                                                                           a[, 11], a1[, 11], a5[, 11]), mode = "numeric"), 
                     as.vector(c(dataw$idyear, a[, 12], a1[, 12], 
                                 a5[, 12]), mode = "numeric"))
    }
    colnames(D) <- c("year", "study", "id", "treat1", 
                     "y1", "sd1", "n1", "treat2", "y2", "sd2", "n2", 
                     "idyear")
  }
  if (level == "study" & type == "binary") {
    data <- data[order(data$year), ]
    for (i in 1:length(data$year)) {
      data$idyear[i] = which((data$id == data$id[i]) == 
                               TRUE)
    }
    D = data
    colnames(D) <- c("year", "study", "id", "treat1", 
                     "treat2", "effect", "se", "idyear")
  }
  if (level == "study" & type == "continuous") {
    data <- data[order(data$year), ]
    for (i in 1:length(data$year)) {
      data$idyear[i] = which((data$id == data$id[i]) == 
                               TRUE)
    }
    D = data
    colnames(D) <- c("year", "study", "id", "treat1", 
                     "treat2", "effect", "se", "idyear")
  }
  if (level == "study" & type == "timetoevent") {
    data <- data[order(data$year), ]
    for (i in 1:length(data$year)) {
      data$idyear[i] = which((data$id == data$id[i]) == 
                               TRUE)
    }
    D = data
    colnames(D) <- c("year", "study", "id", "treat1", 
                     "treat2", "effect", "se", "idyear")
  }
  invisible(list(D = D, data = data, level = level, type = type, 
                 effsize = effsize))
}

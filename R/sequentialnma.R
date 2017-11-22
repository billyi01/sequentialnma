sequentialnma=function (data, level, type, effsize, tau.sq = NA,delta=NA)
{
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
  d <- DataInput(data, level, type, effsize)
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
  mafinal <- metaanalysis(D = d$D, tau.sq, level = d$level, 
                          type = d$type, effsize = d$effsize)
  if (is.na(delta)) {
    delta = mafinal$Mntw
  }
  ma <- metaanalysis(D = d$D, tau.sq, level = d$level, type = d$type, 
                     effsize = d$effsize)
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
  Tr = ma$T
  MaToInput1 <- MaToInput(ma, mafinal)
  input = MaToInput1$input
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
    alpha <- function(method, t) {
      a <- 0.05
      za <- qnorm(1 - a/2, 0, 1, lower.tail = TRUE, log.p = FALSE)
      if (method == "BF") {
        at <- 2 * (1 - pnorm(za/sqrt(t)))
      }
      if (method == "POC") {
        at <- a * log(1 + (exp(1) - 1) * t)
      }
      if (method == "LIN") {
        at <- a * t
      }
      if (method == "PFUN") {
        at <- a * (t^2)
      }
      E = qnorm(1 - at/2, mean = 0, sd = 1, lower.tail = TRUE, 
                log.p = FALSE)
      output <- data.frame(t = t, at = at, E = E)
      return(output)
      result <- array(0, 1, length(output))
      for (j in 1:length(output)) {
        result[j] <- output[j]
      }
      print(result)
    }
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

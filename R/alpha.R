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

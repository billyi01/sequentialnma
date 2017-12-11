#A function that calculates the adjusted nominal level and the associated sequential bounaries
#method: the method to construct the boundaries, it can take the values "BF", "POC", "LIN", "PFUN"
#t: the fraction of the accumulated information

alpha <- function(method, t) {
  a <- 0.05
  za <- qnorm(1 - a/2, 0, 1, lower.tail = TRUE, log.p = FALSE)
  #calculation of adjusted nominal level
  if (method == "BF") at <- 2 * (1 - pnorm(za/sqrt(t)))
  if (method == "POC") at <- a * log(1 + (exp(1) - 1) * t)
  if (method == "LIN") at <- a * t
  if (method == "PFUN") at <- a * (t^2)
  #calculation of associated boundary
  E = qnorm(1 - at/2, mean = 0, sd = 1, lower.tail = TRUE, 
            log.p = FALSE)
  #store fraction of accumulated information, nominal level and boundary
  output <- data.frame(t = t, at = at, E = E)
  #replace boundaries with abs(1.96) if t>1
  output$E[output$E<za & output$E>0]<-za
  output$E[output$E>-za & output$E<0]<--za
  return(output)
  result <- array(0, 1, length(output))
  for (j in 1:length(output)) {
    result[j] <- output[j]
  }
  print(result)
}

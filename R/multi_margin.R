multi.margin <- function(delta, varC, varT = NULL, alpha = 0.05, beta = 0.20){
  stopifnot(delta > 1, alpha  > 0, beta > 0, varC > 0)
  if(is.null(varT)){
    k <- 1 / (delta)
  }
  else{
    k <- sqrt(varC) / (delta * sqrt(varT))
  }
  nT <- (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * (delta^2 * varT + varC / k) / (1 - delta)^2
  nC <- k * nT
  eff <- 2 * (varC + delta^2 * varT) / (sqrt(varC) + delta * sqrt(varT))^2
  return(list(control = nC, treatment = nT, randomization = k, efficiency = eff))
}

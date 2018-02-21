add.margin <- function(delta, varC, varT = NULL, alpha = 0.05, beta = 0.20){
  stopifnot(delta > 0, alpha  > 0, beta > 0, varC > 0)
  if(is.null(varT)){
    k <- 1
  }
  else{
    k <- sqrt(varC) / sqrt(varT)
  }
  nT <- (qnorm(1 - alpha / 2) + qnorm(1 - beta))^2 * (varT + varC / k) / (delta)^2
  nC <- k * nT
  if(k == 1){
    eff <- 1
  }
  else{
    eff <- 2 * (varC + varT) / (sqrt(varC) + sqrt(varT))^2
  }
  return(list(control = nC, treatment = nT, randomization = k, efficiency = eff))
}

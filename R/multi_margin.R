#' Sample Size Calculation for Non-Inferiority Trial for Multiplicative Mean Hypothesis
#'
#' This function computes the required sample size for non-inferiority trial with multiplicative margin.
#' It also computes the sample size required for both treatment and control group.
#' It also computes the efficiency compared to equal treatment allocation.
#'
#' @param delta a numerical value of the additive value in the hypothesis ($\Delta > 1$)
#' @param varC a numerical value of the variance of the control group
#' @param varT a numerical value of the variance of the treatment group, it is null if it is same as varC
#' @param muC a numerical value of the mean of the contril group
#' @param alpha  one-sided Type-I-error
#' @param beta  Type-II-error
#' @return a list of sample sizes for control, treatment, and randomization ratio for treatment to control (k:1) and efficiency compared 1:1 randomization ratio
#'
#'
#' @example multi.margin(0.04, 20, 18)
#'
#'
#' @author Thevaa Chandereng, Rick Chappell
#'
#'



multi.margin <- function(delta, muC, varC, varT = NULL,  alpha = 0.025, beta = 0.20){
  stopifnot(delta > 1, alpha  > 0, beta > 0, varC > 0, is.numeric(muC))
  if(is.null(varT)){
    k <- 1 / (delta)
    varT <- varC
  }
  else{
    k <- sqrt(varC) / (delta * sqrt(varT))
  }
  nT <- (qnorm(1 - alpha) + qnorm(1 - beta))^2 * (delta^2 * varT + varC / k) / (muC * (1 - delta))^2
  nC <- k * nT
  eff <- 2 * (varC + delta^2 * varT) / (sqrt(varC) + delta * sqrt(varT))^2
  return(list(control = nC, treatment = nT, randomization = k, efficiency = eff))
}

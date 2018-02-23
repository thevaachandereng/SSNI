#' Sample Size Calculation for Non-Inferiority Trial for Additive Mean Hypothesis
#'
#' This function computes the required sample size for non-inferiority trial with additive margin.
#' It also computes the sample size required for both treatment and control group.
#' It also computes the efficiency compared to equal treatment allocation.
#'
#' @param delta a numerical value of the additive value in the hypothesis ($\Delta > 0$)
#' @param varC a numerical value of the variance of the control group
#' @param varT a numerical value of the variance of the treatment group, it is null if it is same as varC
#' @param alpha  Two-sided Type-I-error
#' @param beta  Type-II-error
#' @return a list of sample sizes for control, treatment, and randomization ratio for treatment to control and efficiency compared 1:1 randomization ratio
#'
#'
#'
#' @example add.margin(0.04, 20, 18)
#'
#'
#' @author Thevaa Chandereng, Rick Chappell
#'
#'



add.margin <- function(delta, varC, varT = NULL, alpha = 0.025, beta = 0.20){
  stopifnot(delta > 0, alpha  > 0, beta > 0, varC > 0)
  if(is.null(varT)){
    k <- 1
    varT <- varC
  }
  else{
    k <- sqrt(varC) / sqrt(varT)
  }
  nT <- (qnorm(1 - alpha) + qnorm(1 - beta))^2 * (varT + varC / k) / (delta)^2
  nC <- k * nT
  if(k == 1){
    eff <- 1
  }
  else{
    eff <- 2 * (varC + varT) / (sqrt(varC) + sqrt(varT))^2
  }
  return(list(control = nC, treatment = nT, randomization = k, efficiency = eff))
}

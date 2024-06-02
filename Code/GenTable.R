library("R6")
library("tidyverse")
gen_metrics_tbl <- function(expl){
  #Get the true ATE across elements in expl by taking the mean of expl[[i]] over all i
  true_ate <- mean(sapply(expl, function(x) x$TrueATE))
  true_var <- mean(sapply(expl, function(x) x$asvar))
  
  calc_pwr <- function(exp){
    if(true_ate>0){
      pwr <- mean(exp$confint_lwr > 0)
    } else {
      pwr <- mean(exp$confint_upr < 0)
    }
    return(pwr)
  }
  
  bias <- unlist(lapply(expl, function(x) mean(x$ATE-true_ate)))
  variance <- unlist(lapply(expl, function(x) var(x$ATE)))
  mse <- unlist(lapply(expl, function(x) mean((x$ATE-true_ate)^2)))
  ci_width <- unlist(lapply(expl, function(x) mean(x$confint_upr - x$confint_lwr)))
  cvrg <- unlist(lapply(expl, function(x) x$cvrg))
  pwr <- unlist(lapply(expl, function(x) calc_pwr(x)))
  K <- unlist(lapply(expl, function(x) x$est$cross_fit))
  est_name <- unlist(lapply(expl, function(x) x$est$name))
  
  out_frame <- tibble(
    est_name = est_name,
    bias = bias,
    variance = variance,
    mse = mse,
    ci_width = ci_width,
    cvrg = cvrg,
    pwr = pwr,
    K = K
  )
  return(list(res = out_frame, true_ate = true_ate))
}
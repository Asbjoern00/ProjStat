setwd("/home/asr/Desktop/ProjStat/Code/UPTMLE")
exps <- readRDS("parametric_exp_w10.rds")
exps[[2]] <- NULL
exps[[4]] <- NULL

asvar <- mean(c(exps[[1]]$asvar,exps[[2]]$asvar,exps[[3]]$asvar,exps[[4]]$asvar) )
exps[[1]]$asvar <- asvar
exps[[2]]$asvar <- asvar
exps[[3]]$asvar <- asvar
exps[[4]]$asvar <- asvar

plotdist <- function(exp, title, bins = 12){
  data = tibble(ATE = exp$ATE)
  ggplot(data) + geom_histogram(aes(x = sqrt(exp$sim$n)*(ATE-exp$TrueATE), after_stat(density)), fill = "black", color = "black", alpha = 0.6, bins = bins) + 
    geom_density(aes(x = sqrt(exp$sim$n)*(ATE-exp$TrueATE), after_stat(density)), color = "black") + geom_vline(xintercept = 0, color = "red") +
    stat_function(fun = dnorm, args = list(mean = 0, sd =sqrt(exp$asvar)), color = "blue") + 
    ylab("Density") + xlab(latex2exp::TeX(r"($\sqrt{n}\cdot(ATE-ATE_0)$)")) + ggtitle(title)+
    theme_bw() + theme(plot.title = element_text(size = 13,hjust = 0.5))
  
}

library(tidyverse)
p1 <- plotdist(exps[[1]], "UPCV-TMLE, K = 50")
p2 <- plotdist(exps[[2]], "UPCV-TMLE, K = 2")
p3 <- plotdist(exps[[3]], "CV-TMLE, K = 50")
p4 <- plotdist(exps[[4]], "CV-TMLE, K = 2")
gridExtra::grid.arrange(p2,p1,p4,p3, ncol = 2)


p1 <- exps[[1]]$plotci() + ggtitle(paste("UPCV-TMLE, K = 50. Coverage:",round(exps[[1]]$cvrg,3))) + theme(plot.title = element_text(size = 12,hjust = 0.5))
p2 <- exps[[2]]$plotci() + ggtitle(paste("UPCV-TMLE, K = 2. Coverage:",round(exps[[2]]$cvrg,3))) + theme(plot.title = element_text(size = 12,hjust = 0.5))
p3 <- exps[[3]]$plotci() + ggtitle(paste("CV-TMLE, K = 50. Coverage:",round(exps[[3]]$cvrg,3))) + theme(plot.title = element_text(size = 12,hjust = 0.5))
p4 <- exps[[4]]$plotci() + ggtitle(paste("CV-TMLE, K = 2. Coverage:",round(exps[[4]]$cvrg,3))) + theme(plot.title = element_text(size = 12,hjust = 0.5))
gridExtra::grid.arrange(p2,p1,p4,p3, ncol = 2)

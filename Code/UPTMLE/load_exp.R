setwd("/home/asr/Desktop/ProjStat/Code/UPTMLE")
exps <- readRDS("parametric_exp_w10.rds")
exps[[2]] <- NULL
exps[[4]] <- NULL

asvar <- mean(c(exps[[1]]$asvar,exps[[2]]$asvar,exps[[3]]$asvar,exps[[4]]$asvar) )
exps[[1]]$asvar <- asvar
exps[[2]]$asvar <- asvar
exps[[3]]$asvar <- asvar
exps[[4]]$asvar <- asvar


for(exp in exps){
  exp$plotdist()
}
library(R6)
library(tidyverse)
source("/home/asr/Desktop/ProjStat/Code/GenTable.R")
setwd("/home/asr/Desktop/ProjStat/Code/HALExp")
exp <- readRDS("HalExpFinalRegression.rds")




exp_comp <- readRDS("HalExpFinalRegressionTuned.rds")
gen_table <- gen_metrics_tbl(c(exp[1:6], exp[10:14]))


df <- tibble(Comp = 60*c(exp_comp[[2]]$comptime,exp_comp[[1]]$comptime,tuned[[1]]$comptime,tuned[[2]]$comptime,
                         cv_tuned[[1]]$comptime, cv_tuned[[2]]$comptime[1:249]),
             K = c(rep(0,500),rep(5,500),rep(0,500),rep(5,500),rep(0,500),rep(5,249)),
             tuning = c(rep("No tuning",1000),rep("Tuned (OOB)",1000), rep("Tuned (CV)",749)))

df %>% ggplot(aes(x = as.factor(K), y = Comp, fill = factor(K))) + geom_violin() + facet_wrap(~tuning, scales = "free_y")+theme_bw() +
  xlab("K") + ylab("Computation time (seconds)") + theme(legend.position = "none")

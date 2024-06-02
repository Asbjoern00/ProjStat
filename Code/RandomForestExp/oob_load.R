setwd("/home/asr/Desktop/ProjStat/Code/RandomForestExp")
exps <- readRDS("oob_experiment2.rds")
exps2 <- readRDS("rate_experiment2.rds")
all_exp <- c(exps,exps2)

metrics_tbl <- gen_metrics_tbl(all_exp)

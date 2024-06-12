source("/home/asr/Desktop/ProjStat/Code/GenTable.R")
setwd("/home/asr/Desktop/ProjStat/Code/RandomForestExp")

exps <- readRDS("oob_no_cf_regression.rds")
exps2 <- readRDS("oob_cf_regression.rds")
all_exp <- c(exps[1],exps[4],exps2[1],exps2[3])

metrics_tbl <- gen_metrics_tbl(all_exp)
metrics_tbl

all_exp <- c(exps[2],exps[3],exps2[2],exps2[4])
metrics_tbl <- gen_metrics_tbl(all_exp)
metrics_tbl




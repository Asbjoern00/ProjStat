library(R6)
library(tidyverse)
source("/home/asr/Desktop/ProjStat/Code/GenTable.R")
setwd("/home/asr/Desktop/ProjStat/Code/HALExp")

exp <- c(readRDS("hal_experiment2_1_2nd.rds"),
readRDS("hal_experiment2_1_3rd.rds"))
gen_table <- gen_metrics_tbl(exp)

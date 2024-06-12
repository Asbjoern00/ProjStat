setwd("/home/asr/Desktop/ProjStat/Code")
source("Estimator.R")
source("Simulator.R")
source("LearnerTypes.R")
options(error = recover)
df <- readRDS("/home/asr/Desktop/ProjStat/Data/processeddata.rds")


rf_oob_mean <- RF$new(pph~.-1, oob = TRUE, autotune = TRUE)
rf_oob_prp <- RF$new(intended_cs~.-1, oob = TRUE, autotune = TRUE)
est <- TMLE$new(prp_lrn = rf_oob_prp, mean_lrn = rf_oob_mean)
est$fit(df)

source("simulate.R")
source("LearnerTypes.R")

df <- simulate_from_model(g,m, theta = 1, n = 50)$out_frame
gam <- GAM$new(Y ~ s(w1, by = w2) + A)
glm <- GLM$new(Y ~ . )
glmnet <- GLMNet$new(Y ~. )

gam$fit(df)
glm$fit(df)
glmnet$fit(df)


new_sim <- simulate_from_model(g,m,theta = 5, n = 5000)
dfnew <- new_sim$out_frame
glm_pred <- glm$predict(dfnew)
gam_pred <- gam$predict(dfnew)
glmn_pred <- glmnet$predict(dfnew)

dfnew$gam_pred <- gam_pred
dfnew$glm_pred <- glm_pred
dfnew$glmnet_pred <- glmn_pred
dfnew$true_prob <- new_sim[["true_cond_mean"]][["cond_Y_true"]]

#equally good performance
dfnew %>% summarise(gam_perf = mean((true_prob - gam_pred)^2), glm_perf = mean((true_prob - glm_pred)^2), glmn_perf = mean((true_prob - glmnet_pred)^2))





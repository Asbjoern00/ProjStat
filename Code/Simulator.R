# Make R6 class for simulation. Takes arguments for number of observations, how to simulate covariates, how to simulate A, and how to simulate Y
Simulator <- R6::R6Class("Simulator",
                         public = list(
                           n = NULL,
                           sim_cov = NULL,
                           sim_A = NULL,
                           sim_Y = NULL,
                           out_frame = NULL,
                           ATE = NULL,
                           asvar = NULL,
                           initialize = function(n, sim_cov, sim_A, sim_Y){
                             self$n <- n
                             self$sim_cov <- sim_cov
                             self$sim_A <- sim_A
                             self$sim_Y <- sim_Y
                           },
                           simulate = function(){
                             #Simulate covariates
                             W <- self$sim_cov(self$n)
                             
                             #simulate A. Function sim_A should return a list with A and pA
                             A_lst <- self$sim_A(W)
                             A <- A_lst$A
                             propA <- A_lst$pA
                             
                             #Simulate Y. Function sim_Y should return a list with Y, pY1, pY0 and pY
                             Y_lst <- self$sim_Y(A,W)
                             Y <- Y_lst$Y
                             ATE <- mean(Y_lst$pY1 - Y_lst$pY0)
                             
                             # Bind W, A, Y together to tibble, such that the columns in W are named w1, w2, ..., w10
                             out_frame <- cbind(W, A, Y) %>% as_tibble()
                             self$ATE <- ATE
                             self$asvar <- mean(private$eif(Y = Y, pY1 = Y_lst$pY1, pY0 = Y_lst$pY0, A = A, propA = propA, pY = Y_lst$pY)^2)
                             self$out_frame <- out_frame
                           }
                         ),
                         private = list(
                           eif = function(Y, pY1, pY0, pY , A, propA){
                             clever_cov <- (A/propA) - (1-A)/(1-propA)
                             clever_cov*(Y-pY) + (pY1 - pY0) - self$ATE
                           }
                         )
)


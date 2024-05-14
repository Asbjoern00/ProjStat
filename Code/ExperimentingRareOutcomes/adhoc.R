
options(error = recover)
set.seed(123)
n=2500
W1<- rnorm(n, 0, .25)
W2<- runif(n, 0, 1)
W3<- rbinom(n, size=1, 0.5)
A<- rbinom(n, size=1, prob= plogis(-.5+ W1+W2+W3) )
pi<- plogis(-3+ 2*A + 1*W1 + 2*W2 - 4*W3 + .5*A*W1)/15
Y<- rbinom(n, size=1, prob= pi)
sum(Y) # total number of outcomes in the data set


grad<- function(beta, Y, X, wt=1){
  
  pi<- plogis( X%*%beta)  #Qbar(A,W)= expit(beta0 +beta1*X1 + beta2*X2... )
  pi[pi==0] <- .Machine$double.neg.eps # prevent from taking the log of 0 or 1
  pi[pi==1]<- 1-.Machine$double.neg.eps
  resid<- wt*(Y-pi) # weighted residuals
  gr<- crossprod(X, resid)
  return(-gr)
}

logLike<- function(beta, Y, X, wt=1){
  
  pi<- plogis( X%*%beta ) #Qbar(A,W)= expit(beta0 +beta1*X1 + beta2*X2... )
  pi[pi==0] <- .Machine$double.neg.eps	# prevent taking a log of 0 or 1
  pi[pi==1]<- 1-.Machine$double.neg.eps
  logLike<- sum( wt*(Y*log(pi)  + (1-Y)*log(1-pi))  )
  return(-logLike)		# negative quasi-loglikelihood loss function
}


#----------------------------------
# Demonstration of the minimizing the quasi log-likelihood 
#---------------------------
# 
# Qbounds (l,u)= (0,0.075)
l=0; u=0.075
#create the design matrix 
X <- model.matrix(as.formula(Y~W1+W2+W3+A*W1))
# transform Y to Y.tilde in between (l,u)
Y.tilde<- (Y - l)/(u-l)
summary(Y.tilde)

# call to the optim function. 
# par: initial parameter estimates; f:function to minimize; gr: gradient
# arguments to LogLikelihood() & grad() are Y and X
optim.out <- optim(par=rep(0, ncol(X)), fn= logLike, gr=grad, 
                   Y=Y.tilde, X=X, method="BFGS")
# see optim help files for more details and other optimization routines

# get parameter estimates
beta<- optim.out$par
# get predicted values and transform to proper scale
pred.prob.optim <- plogis(X%*%beta)
# compare with standard logistic regression
pred.prob.glm<- predict(glm(Y~W1+W2+W3+A*W1, family="binomial"), type="response")
predictions<- data.frame(optim=pred.prob.optim,  glm=pred.prob.glm)

Mscale_linkfunction <- function(oli = "logit", M = 1){
    if (class(oli) == "character")  oli <- make.link(oli) 
    structure(list(
      linkfun = function(mu) oli$linkfun(mu*M),
      linkinv = function(eta) oli$linkinv(eta)*M,
      mu.eta = function(eta) oli$mu.eta(eta)*M,
      
      valideta =  oli$valideta,
      name = paste0("M-scaled ",oli$name,", M = ",signif(M,3))),
      class = "link-glm"
    )
}

#customlink <- Mscale_linkfunction("logit", u)
#fitted2 <- glm(Y ~ W1+W2+W3+A*W1, family=binomial(link=customlink))

#predictions$glm_mod <- predict(fitted2, type = "response")



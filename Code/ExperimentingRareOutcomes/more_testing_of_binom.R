binomMod <- function (link = "logit") 
{
  linktemp <- substitute(link)
  if (!is.character(linktemp)) 
    linktemp <- deparse(linktemp)
  okLinks <- c("logit", "probit", "cloglog", "cauchit", "log")
  family <- "binomial"
  if (linktemp %in% okLinks) 
    stats <- make.link(linktemp)
  else if (is.character(link)) {
    stats <- make.link(link)
    linktemp <- link
  }
  else {
    if (inherits(link, "link-glm")) {
      stats <- link
      if (!is.null(stats$name)) 
        linktemp <- stats$name
    }
    else {
      stop(gettextf("link \"%s\" not available for %s family; available links are %s", 
                    linktemp, family, paste(sQuote(okLinks), collapse = ", ")), 
           domain = NA)
    }
  }
  #Careful with variance here
  M <- attr(stats, "M")
  variance <- function(mu) mu * (1/M - mu)
  validmu <- function(mu) all(is.finite(mu)) && all(mu > 0 & 
                                                      mu < 1/M)
  dev.resids <- function(y, mu, wt) {
      dev <- numeric(length(y))
      fil <- y == 0
      dev[fil] =  2*1/M*log((1/M-y[fil])/(1/M-mu[fil]))
      dev[!fil] = 2*y[!fil]*log(y[!fil]/mu[!fil])
      dev
  }
  #browser()
  aic <- function(y, n, mu, wt, dev) {
    m <- if (any(n > 1)) 
      n
    else wt
    -2 * sum(ifelse(m > 0, (wt/m), 0) * dbinom(round(m * 
                                                       y), round(m), mu, log = TRUE))
  }
  initialize <- expression({
    if (NCOL(y) == 1) {
      ## allow factors as responses
      ## added BDR 29/5/98
      if (is.factor(y)) y <- y != levels(y)[1L]
      n <- rep.int(1, nobs)
      ## anything, e.g. NA/NaN, for cases with zero weight is OK.
      y[weights == 0] <- 0
      mustart <- (weights * y + 0.5)/(weights + 1)
      m <- weights * y
      if(any(abs(m - round(m)) > 1e-3))
        warning("non-integer #successes in a binomial glm!")
    }
    else if (NCOL(y) == 2) {
      if(any(abs(y - round(y)) > 1e-3))
        warning("non-integer counts in a binomial glm!")
      n <- y[, 1] + y[, 2]
      y <- ifelse(n == 0, 0, y[, 1]/n)
      weights <- weights * n
      mustart <- (n * y + 0.5)/(n + 1)
    }
    else stop("for the 'binomial' family, y must be a vector of 0 and 1\'s\nor a 2 column matrix where col 1 is no. successes and col 2 is no. failures")
  })
  structure(list(family = family, link = linktemp, linkfun = stats$linkfun, 
                 linkinv = stats$linkinv, variance = variance, dev.resids = dev.resids, 
                 aic = aic, mu.eta = stats$mu.eta, initialize = initialize, 
                 validmu = validmu, valideta = stats$valideta), 
            class = "family")
}


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
l=0; u=0.03
#create the design matrix 
X <- model.matrix(as.formula(Y~W1+W2+W3+A*W1))


# call to the optim function. 
# par: initial parameter estimates; f:function to minimize; gr: gradient
# arguments to LogLikelihood() & grad() are Y and X
optim.out <- optim(par=rep(0, ncol(X)), fn= logLike, gr=grad, 
                   Y=Y.tilde, X=X, method="BFGS")
# see optim help files for more details and other optimization routines

# get parameter estimates
beta<- optim.out$par
# get predicted values and transform to proper scale
pred.prob.optim <- plogis(X%*%beta)*(u-l) + l
# compare with standard logistic regression
pred.prob.glm<- predict(glm(Y~W1+W2+W3+A*W1, family="binomial"), type="response")
predictions<- data.frame(optim=pred.prob.optim,  glm=pred.prob.glm)

Mscale_linkfunction <- function(oli = "logit", M = 1){
  if (class(oli) == "character")  oli <- make.link(oli) 
  structure(list(
    linkfun = function(mu) oli$linkfun(mu),
    linkinv = function(eta) oli$linkinv(eta)*M,
    mu.eta = function(eta) oli$mu.eta(eta)*M,
    
    valideta =  oli$valideta,
    name = paste0("M-scaled ",oli$name,", M = ",signif(M,3))),
    M = M,
    class = "link-glm"
  )
}

customlink <- Mscale_linkfunction("logit", u)
fitted2 <- glm(Y ~ W1+W2+W3+A*W1, family=binomMod(link=customlink))


fitted3 <- glm(Y ~ W1+W2+W3+A*W1, family=binomial(link = "logit"))
predictions$glm_mod <- predict(fitted2, type="response")

mean((predictions$optim - predictions$glm)^2)

mean((predictions$optim - predictions$glm_mod)^2)
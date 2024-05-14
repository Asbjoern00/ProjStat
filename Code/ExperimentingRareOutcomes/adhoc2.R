ScaelBinomial <- function(link = "logit"){
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
  variance <- function(mu) mu * (1 - mu)
  validmu <- function(mu) all(is.finite(mu)) && all(mu > 0 & 
                                                      mu < 1000)
  dev.resids <- function(y, mu, wt) 1
  aic <- function(y, n, mu, wt, dev) {
    m <- if (any(n > 1)) 
      n
    else wt
    -2 * sum(ifelse(m > 0, (wt/m), 0) * dbinom(round(m * 
                                                       y), round(m), mu, log = TRUE))
  }
  simfun <- function(object, nsim) {
    ftd <- fitted(object)
    n <- length(ftd)
    ntot <- n * nsim
    wts <- object$prior.weights
    if (any(wts%%1 != 0)) 
      stop("cannot simulate from non-integer prior.weights")
    if (!is.null(m <- object$model)) {
      y <- model.response(m)
      if (is.factor(y)) {
        yy <- factor(1 + rbinom(ntot, size = 1, prob = ftd), 
                     labels = levels(y))
        split(yy, rep(seq_len(nsim), each = n))
      }
      else if (is.matrix(y) && ncol(y) == 2) {
        yy <- vector("list", nsim)
        for (i in seq_len(nsim)) {
          Y <- rbinom(n, size = wts, prob = ftd)
          YY <- cbind(Y, wts - Y)
          colnames(YY) <- colnames(y)
          yy[[i]] <- YY
        }
        yy
      }
      else rbinom(ntot, size = wts, prob = ftd)/wts
    }
    else rbinom(ntot, size = wts, prob = ftd)/wts
  }
  structure(list(family = family, link = linktemp, linkfun = stats$linkfun, 
                 linkinv = stats$linkinv, variance = variance, dev.resids = dev.resids, 
                 aic = aic, mu.eta = stats$mu.eta, initialize = expression({
    if (NCOL(y) == 1) {
        if (is.factor(y)) 
            y <- y != levels(y)[1L]
        n <- rep.int(1, nobs)
        y[weights == 0] <- 0
        if (any(y < 0 | y > 100)) 
            stop("y values must be 0 <= y <= 1")
        mustart <- 0.5
        m <- weights * y
        if ("binomial" == "binomial" && any(abs(m - round(m)) > 
            0.001)) 
            warning(gettextf("non-integer #successes in a %s glm!", 
                "binomial"), domain = NA)
    }
    else if (NCOL(y) == 2) {
        if ("binomial" == "binomial" && any(abs(y - round(y)) > 
            0.001)) 
            warning(gettextf("non-integer counts in a %s glm!", 
                "binomial"), domain = NA)
        n <- (y1 <- y[, 1L]) + y[, 2L]
        y <- y1/n
        if (any(n0 <- n == 0)) 
            y[n0] <- 0
        weights <- weights * n
        mustart <- (n * y + 0.5)/(n + 1)
    }
    else stop(gettextf("for the '%s' family, y must be a vector of 0 and 1's\nor a 2 column matrix where col 1 is no. successes and col 2 is no. failures", 
        "binomial"), domain = NA)
}),
                 validmu = validmu, valideta = stats$valideta, simulate = simfun), 
            class = "family")
}

Mscale_linkfunction <- function(oli = "logit", M = 1){
  if (class(oli) == "character")  oli <- make.link(oli) 
  structure(list(
    linkfun = function(mu) oli$linkfun(mu*M),
    linkinv = function(eta) oli$linkinv(eta)/M,
    mu.eta = function(eta) oli$mu.eta(eta)/M,
    valideta =  oli$valideta,
    name = paste0("M-scaled ",oli$name,", M = ",signif(M,3))),
    class = "link-glm"
  )
}

customlink <- Mscale_linkfunction(oli = "logit", M = 0.10)
glm(Y.tilde ~ X-1, family = ScaelBinomial())

glm(Y ~ X-1, family = "binomial")

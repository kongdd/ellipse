"profile.glm" <-
  function (fitted, which = 1:npar, alphamax = 0.01, maxpts = 100, 
            delta.t = cutoff/5) 
{
  f.summary <- fitted$summary
  if (is.null(f.summary)) 
    f.summary <- summary(fitted)
  f.model <- fitted$model
  if (is.null(f.model)) 
    stop("Must include model with glm object (run glm with model=T)")
  X <- model.matrix(fitted$terms, f.model, NULL)
  Y <- model.extract(f.model, response)
  f.family <- eval(fitted$call$family)
  std.err <- f.summary$coefficients[, "Std. Error"]
  npar <- length(std.err)
                                        #	class(fitted) <- NULL
  f.class <- class(fitted)
  formula <- fitted$formula
  asgn <- fitted$assign
  p <- .pars <- fitted$coefficients
  pn <- names(p)
  npar <- ncol(X)
  nobs <- nrow(X)
  if (any(which > npar) || any(which < 1)) 
    stop(paste("which must be in the range 1:", npar))
  S.hat <- fitted$deviance
  cutoff <- sqrt(npar * qf(1 - alphamax, npar, nobs - npar))
  s.hat <- sqrt(f.summary$dispersion)
  outmat <- array(0, c(nobs, npar))
  out <- list()
  for (par in which) {
    sgn <- -1
    count <- 1
    varying <- rep(T, npar)
    varying[par] <- F
    .pars <- p
    tau <- double(2 * maxpts)
    par.vals <- array(0, c(2 * maxpts, npar), list(NULL, 
                                                   pn))
    tau[1] <- 0
    par.vals[1, ] <- .pars
    base <- .pars[par]
    profile.par.inc <- delta.t * std.err[par]
    .pars[par] <- .pars[par] - profile.par.inc
    x <- X[, -par, drop = F]
    while (count <= maxpts) {
      if (abs(.pars[par] - base)/std.err[par] > 10 * cutoff) 
        break
      count <- count + 1
      z <- glm.fit(x, Y, family = f.family, offset = .pars[par] * 
                   X[, par])
      tau[count] <- (sgn * sqrt(z$deviance - S.hat))/s.hat
      .pars[-par] <- z$coefficients
      par.vals[count, ] <- .pars
      if (abs(tau[count]) > cutoff) 
        break
      .pars <- .pars + ((.pars - par.vals[count - 1, ]) * 
                        delta.t)/abs(tau[count] - tau[count - 1])
    }
    tau[1:count] <- tau[count:1]
    par.vals[1:count, ] <- par.vals[count:1, ]
    sgn <- 1
    newmax <- count + maxpts
    .pars <- par.vals[count, ]
    while (count <= newmax) {
      .pars <- .pars + ((.pars - par.vals[count - 1, ]) * 
                        delta.t)/abs(tau[count] - tau[count - 1])
      if (abs(.pars[par] - base)/std.err[par] > 10 * cutoff) 
        break
      count <- count + 1
      z <- glm.fit(x, Y, family = f.family, offset = .pars[par] * 
                   X[, par])
      tau[count] <- (sgn * sqrt(z$deviance - S.hat))/s.hat
      .pars[-par] <- z$coefficients
      par.vals[count, ] <- .pars
      if (abs(tau[count]) > cutoff) 
        break
    }
    out[[par]] <- structure(list(tau = tau[1:count], par.vals = par.vals[1:count, 
                                                       ]), class = "data.frame", row.names = as.character(1:count), 
                            parameters = list(par = par, std.err = std.err[par]))
  }
  names(out)[which] <- pn[which]
  orig <- fitted
  class(orig) <- f.class
  attr(out, "original.fit") <- orig
  attr(out, "summary") <- f.summary
  class(out) <- c("profile.glm", "profile.nls", "profile")
  out
}

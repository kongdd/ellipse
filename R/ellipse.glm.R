"ellipse.glm" <-
  function (x, which = c(1, 2), level = 0.95, t, ...) 
{
  s <- summary(x)
  if (missing(t)) {
    t <- switch(as.character(x$family["family"]), binomial = sqrt(qchisq(level, 2)), poisson = sqrt(qchisq(level, 2)), sqrt(2 * qf(level, 2, s$df[2])))
  }
  ellipse.default(s$dispersion * s$cov.unscaled[which, which], 
                  centre = x$coefficients[which], t = t)
}

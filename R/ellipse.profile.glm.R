"ellipse.profile.glm" <-
  function (x, level = 0.95, t, ...) 
{
  if (missing(t)) {
    t <- switch(attr(x, "original.fit")$family$family, binomial = sqrt(qchisq(level,2)), poisson = sqrt(qchisq(level, 2)), sqrt(2 * qf(level,2, attr(attr(x,"original.fit"), "summary")$df[2])))
  }
  ellipse.profile(x, level = level, t = t, ...)
}


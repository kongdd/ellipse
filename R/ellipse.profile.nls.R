"ellipse.profile.nls" <-
  function (x, level = 0.95, t = sqrt(2 * qf(level, 2, attr(x, 
                               "summary")$df[2])), ...) 
{
  ellipse.profile(x, level = level, t = t, ...)
}

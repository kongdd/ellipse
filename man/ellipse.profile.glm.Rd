\name{ellipse.profile.glm}
\alias{ellipse.profile.glm}
\title{
Profile sketch 
}
\description{
This is the version of ellipse.profile specific to profiles from glm
objects.  See ellipse.profile for the description.
}
\usage{
\method{ellipse}{profile.glm}(x, level=0.95, t,\dots)
}
\details{
The t parameter is optional.  It defaults to a contour based on the
chi-square distribution for Binomial or Poisson models, and one based
on the F distribution for others.
}
\keyword{ellipse}
% Converted by Sd2Rd version 0.3-3.

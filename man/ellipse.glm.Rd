\name{ellipse.glm}
\alias{ellipse.glm}
\title{
Outline an approximate pairwise confidence region 
}
\description{
This function produces the ellipsoidal outline of an approximate 
pairwise confidence region for a generalized linear model fit.
}
\usage{
ellipse.glm(x, which=c(1, 2), level=0.95, t=(see below), \dots)
}
\arguments{
\item{x}{
The first argument should be an glm object, usually resulting from a call
to glm().
}
\item{which}{
Which selects the pair of parameters to be plotted.  The default is the
first two.
}
\item{level}{
The confidence level of the region.  Default 95\%.
}
\item{t}{
The t statistic on the boundary of the ellipse.  For Binomial or Poisson
families, sqrt(qchisq(level,2)) is used; for other distributions,
sqrt(2*qf(level,2,df)) where df is the residual degrees of freedom.
}
\item{...}{
Other ellipse.default parameters may also be used.
}}
\value{
A matrix with columns named according to which to outline the confidence region.
}
\details{
The summary function is used to obtain the approximate covariance matrix of the
fitted parameters, the dispersion estimate, and the degrees of freedom.
}
\seealso{
ellipse.default
}
\examples{
data(solder.balance)
fit <- glm(skips ~ ., data = solder.balance, family = poisson)
plot(ellipse(fit))
}
\keyword{ellipse}
% Converted by Sd2Rd version 0.3-3.

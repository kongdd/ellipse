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
\method{ellipse}{glm}(x, which = c(1, 2), level = 0.95, t, npoints = 100, 
        dispersion, \dots)
}
\arguments{
\item{x}{
The first argument should be a \code{glm} object, usually resulting from a call
to \code{glm()}.
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
families, \code{sqrt(qchisq(level, 2))} is used; for other distributions,
\code{sqrt(2*qf(level, 2, df))} where \code{df} is the residual degrees of freedom.
}
\item{npoints}{
How many points to return in the ellipse.
}
\item{dispersion}{
The value of dispersion to use.  If specified, it is treated as fixed,
and the chi-square limits for \code{t} are used. If missing, it is 
taken from \code{summary(x)}.
}
\item{\dots}{
Other \code{ellipse.default} parameters may also be used.
}}
\value{
A matrix with columns named according to which to outline the confidence region.
}
\details{
The summary function is used to obtain the approximate covariance matrix of the
fitted parameters, the dispersion estimate, and the degrees of freedom.
}
\seealso{
\code{\link{ellipse.default}}
}
\examples{     
# Plot an approximate 95 % confidence region for the TreatCont and TreatFT 
# coefficients for the MASS anorexia data

require(MASS)
data(anorexia)

fit <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                       family = gaussian, data = anorexia)
plot(ellipse(fit, which = c('TreatCont', 'TreatFT')), type = 'l')
points(fit$coefficients['TreatCont'], fit$coefficients['TreatFT'])
}
\keyword{dplot}
\keyword{regression}


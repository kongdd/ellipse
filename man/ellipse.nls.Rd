\name{ellipse.nls}
\alias{ellipse.nls}
\title{
Outline an approximate pairwise confidence region 
}
\description{
This function produces the ellipsoidal outline of an approximate 
pairwise confidence region for a nonlinear model fit.
}
\usage{
ellipse.nls(x, which=c(1, 2), level=0.95, t=sqrt(2 * qf(level, 2, s$df[2])), \dots)
}
\arguments{
\item{x}{
The first argument should be an nls object, usually resulting from a call
to nls().
}
\item{which}{
Which selects the pair of parameters to be plotted.  The default is the
first two.
}
\item{level}{
The confidence level of the region.  Default 95\%.
}
\item{t}{
The t statistic on the boundary of the ellipse.
}
\item{...}{
Other ellipse.default parameters may also be used.
}}
\value{
A matrix with columns x and y to outline the confidence region.
}
\details{
The summary function is used to obtain the approximate covariance matrix of the
fitted parameters.
}
\seealso{
ellipse.default, ellipse.profile
}
\examples{
# Plot an approximate 95\% confidence region for the weight and displacement
# parameters in the Michaelis Menten model
conc <- c(0.3330, 0.1670, 0.0833, 0.0416,
          0.0208, 0.0104, 0.0052)
vel <- c(3.636, 3.636, 3.236, 2.666, 2.114, 1.466, 0.866)
Micmen <- data.frame(conc=conc, vel=vel)
fit <- nls(vel~Vm*conc/(K+conc),Micmen,start=list(K=0.02,Vm=3.7))
plot(ellipse(fit))
}
\keyword{ellipse}
% Converted by Sd2Rd version 0.3-3.

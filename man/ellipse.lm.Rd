\name{ellipse.lm}
\alias{ellipse.lm}
\title{
Outline a pairwise confidence region for a linear model fit.
}
\description{
This function produces the ellipsoidal outline of a pairwise confidence
region for a linear model fit.
}
\usage{
ellipse.lm(x, which=c(1, 2), level=0.95, t=sqrt(2 * qf(level, 2, x$df.residual)), \dots)
}
\arguments{
\item{x}{
The first argument should be an lm object, usually resulting from a call
to lm().
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
The summary function is used to obtain the covariance matrix of the
fitted parameters.
}
\seealso{
ellipse.default
}
\examples{
# Plot the joint 95\% confidence region for the weight and displacement
# parameters in the Fuel dataset
data(fuel.frame)
fit <- lm(Fuel ~ . , fuel.frame)
plot(ellipse(fit,which=c('Weight','Disp.')))
}
\keyword{ellipse}
% Converted by Sd2Rd version 0.3-3.

\name{ellipse.default}
\alias{ellipse.default}
\title{
Produces an ellipse from a covariance matrix or a correlation
}
\description{
This function produces an ellipse representing a pairwise confidence
region for two parameters, given a covariance or correlation matrix or
just a correlation, and a position.
}
\usage{
ellipse.default(x, scale=c(1, 1), centre=c(0, 0), level=0.95, 
                t=sqrt(qchisq(level,2)), which=c(1, 2), npoints=100)
}
\arguments{
\item{x}{
The parameter x should be a correlation between -1 and 1 or a
square positive definite matrix at least 2x2
in size. It will be treated as the correlation or covariance 
of a multivariate normal distribution.
}
\item{scale}{
If x is a correlation matrix, then the standard deviations of each
parameter can be given in the scale parameter.  This defaults to c(1,1),
so no rescaling will be done.
}
\item{centre}{
The centre of the ellipse will be at this position.  Default c(0,0).
}
\item{level}{
The confidence level of a pairwise confidence region.  The default is
0.95, for a 95\% region.  This is used to control the size of the ellipse
being plotted.  A vector of levels may be used.
}
\item{t}{
The size of the ellipse may also be controlled by specifying the value
of a t-statistic on its boundary.  This defaults to the appropriate
value for the confidence region.
}
\item{which}{
This parameter selects which pair of variables from the matrix will be
plotted.  The default is the first 2.
}
\item{npoints}{
The number of points used in the ellipse.  Default is 100.
}}
\value{
An npoints x 2 matrix is returned with columns named according to the
row names of the matrix x (default 'x' and 'y'), suitable
for plotting.
}
\details{
The (cos(theta+d/2), cos(theta-d/2)) parametrization of an ellipse is
used, where cos(d) is the correlation of the parameters.
}
\references{
Murdoch, D.J. and Chow, E.D. (1994).  A graphical display of large
correlation matrices.  Mathematical preprint \#1994-09, Department of
Mathematics and Statistics, Queen's University, Kingston, Canada.
}
\seealso{
\code{\link{ellipse}}
}
\examples{
#  Make a 2 x 2 covariance matrix, and plot the corresponding 95\%
#  confidence region
cov <- matrix(c(1,0.5,0.5,1), 2,2)
plot(ellipse(cov), type='l')
}
\keyword{ellipse}
% Converted by Sd2Rd version 0.3-3.

\name{ellipse}
\alias{ellipse}
\title{
Make an ellipse
}
\description{
This function returns an ellipse or other outline of a confidence region
for two parameters.  
This is a generic function.
Functions with names beginning in \code{ellipse.} will be
methods for this function.
\code{lm}, \code{nls}, \code{profile}, \code{profile.nls}
}
\usage{
ellipse(x, \dots)
}
\arguments{
\item{x}{
An S object.
}
\item{...}{
Each of the specialized types uses different optional parameters.
}}
\value{
A matrix is returned with two columns outlining an ellipse or a
confidence region.
}
\seealso{
\code{\link{ellipse.lm}}, \code{\link{plotcorr}}
}
\examples{
# Plot an ellipse corresponding to a 95\% probability region for a
# bivariate normal distribution with mean 0, unit variances and 
# correlation 0.8.
plot(ellipse(0.8),type='l')
}
\keyword{ellipse}
% Converted by Sd2Rd version 0.3-3.

\name{pairs.profile.nls}
\alias{pairs.profile.nls}
\title{
Profile pairs
}
\description{
This function produces pairwise plots of profile traces, profile
sketches, and ellipse approximations to confidence intervals.
}
\usage{
pairs.profile.nls(x, labels=c(names(x), "Profile tau"), panel=lines, invert=TRUE, plot.tau=TRUE, plot.trace=TRUE, plot.sketch=TRUE, plot.ellipse=FALSE, level=0.95, \dots)
}
\arguments{
\item{x}{
An object of class profile, generally the result of the profile()
function.
}
\item{labels}{
The labels to use for each variable.  These default to the variable
names.
}
\item{panel}{
The function to use to draw the sketch in each panel.
}
\item{invert}{
Whether to swap the axes so things look better.
}
\item{plot.tau}{
Whether to do the profile tau (profile t) plots.
}
\item{plot.trace}{
Whether to do the profile trace plots.
}
\item{plot.sketch}{
Whether to do the profile sketch plots.
}
\item{plot.ellipse}{
Whether to do the ellipse approximations.
}
\item{level}{
The nominal confidence level for the profile sketches and ellipses.
}
\item{...}{
Other plotting parameters.
}}
\section{Side Effects}{
Produces a plot on the current device for each pair of variables in the
profile object.
}
\details{
This function implements the plots used in Bates and Watts (1988) for
nonlinear regression diagnostics.
}
\references{
Bates and Watts (1988), Nonlinear Regression Analysis \& its
Applications.
}
\seealso{
profile, ellipse.profile, ellipse.nls
}
\examples{
 # Plot everything for the Puromycin data
 library(nls)
 data(Puromycin)
 Purboth <- nls(formula = rate ~ ((Vm + delV * (state == "treated"))
   * conc)/(K + conc), data = Puromycin,
   start = list(Vm = 160, delV = 40, K = 0.05))
 Pur.prof <- profile(Purboth)
 pairs(Pur.prof, plot.ellipse=TRUE)
}
\keyword{~keyword}
% Converted by Sd2Rd version 0.3-3.

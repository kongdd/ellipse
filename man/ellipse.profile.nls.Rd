\name{ellipse.profile.nls}
\alias{ellipse.profile.nls}
\title{Profile sketch }
\description{
This is the version of ellipse.profile specific to profiles from nls objects.  See ellipse.profile for the description.
}
\usage{
\method{ellipse}{profile.nls}(x, level = 0.95, t = sqrt(2 * qf(level, 2, attr(x, "summary")$df[2])), 
    \dots)
}

\item{x}{An object of class \code{profile.nls}, generally the result of the
    profile() function}

\item{labels}{The labels to use for each variable.  These default to the
    variable names.}

\item{panel}{The function to use to draw the sketch in each panel.}

\item{invert}{Whether to swap the axes so things look better.}

\item{plot.tau}{Whether to do the profile tau (profile t) plots.}

\item{plot.trace}{Whether to do the profile trace plots.}

\item{plot.sketch}{Whether to do the profile sketch plots.}

\item{plot.ellipse}{Whether to do the ellipse approximations.}

\item{level}{The nominal confidence level for the profile sketches and
    ellipses.}

\item{\dots}{Other plotting parameters.}


\details{
Produces a plot on the current device for each pair of variables in the profile object.
This function implements the plots used in Bates and Watts (1988) for nonlinear regression diagnostics.
}
\seealso{\code{\link{profile}}, \code{\link{ellipse.profile}}, \code{\link{ellipse.nls}}}
\references{Bates and Watts (1988), Nonlinear Regression Analysis & its
    Applications.}
\examples{
 # Plot everything for the Puromycin data
library(nls)
data(Puromycin)
Purboth <- nls(formula = rate ~ ((Vm + delV * (state == "treated"))* conc)/(K + conc), 
    data = Puromycin, start = list(Vm = 160, delV = 40, K = 0.05))
Pur.prof <- profile(Purboth)
pairs(Pur.prof, plot.ellipse = TRUE)
}
\keyword{dplot}
\keyword{nonlinear}




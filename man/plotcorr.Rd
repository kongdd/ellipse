\name{plotcorr}
\alias{plotcorr}
\title{
Plot correlation matrix ellipses
}
\description{
This function plots a correlation matrix using ellipse-shaped glyphs for
each entry.  The ellipse represents a level curve of the density of a
bivariate normal with the matching correlation.  It works best with the
postscript output device.
}
\usage{
plotcorr(corr, outline = TRUE, dev = FALSE, col = "grey", paropts = NULL, 
    numbers = FALSE, \dots)
}
\arguments{
\item{corr}{
A matrix containing entries between \code{-1} and \code{1} to be plotted as
correlations.  Use \code{corr = NULL} to execute \code{plotcorr} only for the side effect
of opening the \code{postscript} device (see example below). 
}
\item{outline}{
Whether the ellipses should be outlined in the default colour.
}
\item{dev}{
Which device to plot to.  By default, continues to plot to the current
device.  If you want to produce a Postscript plot, use
\code{dev = postscript}.  Note that in order to install the fonts correctly, the
first plot to a postscript device must come from \code{plotcorr(dev = postscript, \dots)}, 
not from the standard \code{postscript()} call.

NB:  Postscript support is not present yet in this R port.
}
\item{col}{
Which colour to use to fill the ellipses.  
}
\item{paropts}{
Layout and other plot options to use after the device is open. When 
\code{plotcorr} opens the device, \code{par()} options specified before you
call it won't have any effect.  Specify the options you need in a list
here to affect the \code{plotcorr} output.  For example,
\code{paropts = list(mfrow = c(3, 2))} will specify a layout of 3 rows of 2 plots.
Only the top left corner will be plotted; use subsequent plot commands
(including \code{plotcorr(\dots{}, dev = FALSE, \dots)} commands) to fill in the other
plots.
}
\item{numbers}{
Whether to plot numerical correlations in place of ellipses.  If
numbers is \code{TRUE}, then the correlations will be rounded to a single decimal
place and placed on the plot.
}
\item{\dots}{
Parameters to pass to the graphics device.  For example, use
\code{file = 'output.ps'} to direct \code{postscript}  output to that file.
}}
\section{Side Effects}{
If \code{dev} specifies a graphics device, then 
this will open it. 
If \code{dev} is \code{postscript}, installs two ellipse-shaped fonts into the 
Postscript preamble:  an outline font in the second last position, and a solid
font in the last position.   [Not implemented in this R version!]
Produces a plot of the correlations on the given device or on the
current device.
}
\details{
The ellipses being plotted will be tangent to a unit character square,
with the shape chosen to match the required correlation.
On the \code{postscript} output device, plotting is done by installing special
fonts for the ellipse shapes.  On other devices, plotting is done using
the \code{ellipse()} function.  Postscript output is much, much faster than
other devices.
}
\references{
Murdoch, D.J. and Chow, E.D. (1996). A graphical display of large 
correlation matrices. The American Statistician 50, 178-180. 
}
\seealso{
\code{\link{ellipse}}
}
\examples{
# Plot the correlation matrix for the mtcars data full model fit 
data(mtcars)
fit <- lm(mpg ~ ., mtcars)
plotcorr(summary(fit, correlation = TRUE)$correlation)
# Plot a second figure to the same file, with numbers in place of the
# ellipses
plotcorr(summary(fit, correlation = TRUE)$correlation, numbers = TRUE)
}
\keyword{hplot}

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
plotcorr(corr, outline=TRUE, dev=FALSE, col="grey", paropts=NULL, numbers=FALSE, \dots)
}
\arguments{
\item{corr}{
A matrix containing entries between -1 and 1 to be plotted as
correlations.  Use corr=NULL to execute plotcorr only for the side effect
of opening the postscript device (see example below). 
}
\item{outline}{
Whether the ellipses should be outlined in the default colour.
}
\item{dev}{
Which device to plot to.  By default, continues to plot to the current
device.  If you want to produce a Postscript plot, use
dev=postscript.  Note that in order to install the fonts correctly, the
first plot to a postscript device must come from plotcorr(dev=postscript,\dots{}),not from the standard postscript() call.
}
\item{col}{
Which colour to use to fill the ellipses.  Use col=0 for empty, col=1
for black, other integers for shades according to the
ps.options('colors') vector.  NB:  some versions of S-PLUS map all
shaded text to black or white on a black and white laser printer; 
use "ps.options(black.and.white='false')" to disable this.
}
\item{paropts}{
Layout and other plot options to use after the device is open. When 
plotcorr opens the device, par() options specified before you
call it won't have any effect.  Specify the options you need in a list
here to affect the plotcorr output.  For example,
paropts=list(mfrow=c(3,2)) will specify a layout of 3 rows of 2 plots.
Only the top left corner will be plotted; use subsequent plot commands
(including plotcorr(\dots{}, dev=F, \dots{}) commands) to fill in the other
plots.
}
\item{numbers}{
Whether to plot numerical correlations in place of ellipses.  If
numbers is T, then the correlations will be rounded to a single decimal
place and placed on the plot.
}
\item{...}{
Parameters to pass to the graphics device.  For example, use
file='output.ps' to direct postscript  output to that file.
}}
\section{Side Effects}{
If dev specifies a graphics device, then 
this will open it. 
If dev is "postscript", installs two ellipse-shaped fonts into the 
postscript preamble:  an outline font in the second last position, and a solid
font in the last position.   [Not implemented in this version]
Produces a plot of the correlations on the given device or on the
current device.
}
\details{
The ellipses being plotted will be tangent to a unit character square,
with the shape chosen to match the required correlation.
On the postscript output device, plotting is done by installing special
fonts for the ellipse shapes.  On other devices, plotting is done using
the ellipse() function.  Postscript output is much, much faster than
other devices.
}
\seealso{
\code{\link{ellipse}}
}
\examples{
# Plot the correlation matrix for the mtcars data full model fit 
data(mtcars)
fit <- lm(mpg ~ ., mtcars)
plotcorr(summary(fit,correlation=T)$correlation)
# Plot a second figure to the same file, with numbers in place of the
# ellipses
plotcorr(summary(fit,correlation=T)$correlation, numbers=T)
}
\keyword{ellipse}
% Converted by Sd2Rd version 0.3-3.

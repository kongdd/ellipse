\name{plotcorr}
\alias{plotcorr}
\title{
Plot correlation matrix ellipses
}
\description{
This function plots a correlation matrix using ellipse-shaped glyphs for
each entry.  The ellipse represents a level curve of the density of a
bivariate normal with the matching correlation.
}
\synopsis{
plotcorr(corr, outline = TRUE, col = 'grey', numbers = FALSE, type = c("full","lower","upper"),
	diag = (type == "full"), bty = "n", axes = FALSE, xlab = "", ylab = "", asp = 1,
	cex.lab = par("cex.lab"), cex = 0.75*par("cex"), mar = 0.1 + c(2,2,4,2), ...)
}
\usage{
plotcorr(corr, outline = TRUE, col = "grey", numbers = FALSE,
  type = "full", diag = FALSE, \dots)
}
\arguments{
\item{corr}{
A matrix containing entries between \code{-1} and \code{1} to be plotted as
correlations.
}
\item{outline}{
Whether the ellipses should be outlined in the default colour.
}
\item{col}{
Which colour(s) to use to fill the ellipses.
}
\item{numbers}{
Whether to plot numerical correlations in place of ellipses.  If
numbers is \code{TRUE}, then the correlations will be rounded to a single decimal
place and placed on the plot.
}
\item{type}{Character. Plot \code{"full"} matrix or just \code{"upper"} or
  \code{"lower"} triangular part of it.}
\item{diag}{Logical. Plot diagonal elements or not. }
\item{\dots}{
Additional graphical parameters.
}}
\details{
The ellipses being plotted will be tangent to a unit character square,
with the shape chosen to match the required correlation.  If \code{numbers = FALSE},
the \code{col} vector will be recycled to colour each of the ellipses; if
\code{TRUE}, it will be ignored.
}
\references{
Murdoch, D.J. and Chow, E.D. (1996). A graphical display of large
correlation matrices. The American Statistician 50, 178-180.
}
\seealso{
\code{\link{ellipse}}
}
\author{Duncan Murdoch; Gregor Gorjanc suggested the \code{type} and \code{diag}
options.}
\examples{
save.par <- par(ask = interactive())

# Plot the correlation matrix for the mtcars data full model fit 
data(mtcars)
fit <- lm(mpg ~ ., mtcars)
plotcorr(summary(fit, correlation = TRUE)$correlation)

# Plot a second figure with numbers in place of the
# ellipses
plotcorr(summary(fit, correlation = TRUE)$correlation, numbers = TRUE)

# Colour the ellipses to emphasize the differences
corr.mtcars <- cor(mtcars)
ord <- order(corr.mtcars[1,])
xc <- corr.mtcars[ord, ord]
plotcorr(xc, col=cm.colors(11)[5*xc + 6])

plotcorr(xc, col=cm.colors(11)[5*xc + 6], type = "upper")
plotcorr(xc, col=cm.colors(11)[5*xc + 6], type = "lower", diag = TRUE)
par(save.par)
}
\keyword{hplot}

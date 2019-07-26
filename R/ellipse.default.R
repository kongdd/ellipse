#' @rdname ellipse
#' @export
ellipse <-function (x, ...) UseMethod("ellipse")

#' Make an ellipse
#' 
#' A generic function returning an ellipse or other outline of a confidence 
#' region for two parameters.
#' 
#' @param x An object. In the default method the parameter `x` should be a 
#' correlation between -1 and 1 or a square positive definite matrix at least 2x2 
#' in size. It will be treated as the correlation or covariance of a multivariate 
#' normal distribution.
#' @param scale If `x` is a correlation matrix, then the standard deviations of 
#' each parameter can be given in the scale parameter. This defaults to c(1, 1), 
#' so no rescaling will be done.
#' @param centre The centre of the ellipse will be at this position.
#' @param level The confidence level of a pairwise confidence region. The default 
#' is 0.95, for a 95% region. This is used to control the size of the ellipse 
#' being plotted. A vector of levels may be used.
#' @param t The size of the ellipse may also be controlled by specifying the value 
#' of a t-statistic on its boundary. This defaults to the appropriate value for 
#' the confidence region.
#' @param which This parameter selects which pair of variables from the matrix 
#' will be plotted. The default is the first two.
#' @param npoints The number of points used in the ellipse. Default is 100.
#' @param ... Descendant methods may require additional parameters.
#' 
#' @return An \code{npoints} x \code{2} matrix is returned with columns named 
#' according to the row names of the matrix \code{x} (default \code{'x'} and 
#' \code{'y'}), suitable plotting.
#' 
#' @details 
#' \code{(cos(theta + d/2), cos(theta - d/2))} parametrization of an ellipse, where 
#' \code{cos(d)} is the correlation of the parameters.
#' 
#' @references
#' Murdoch, D.J. and Chow, E.D. (1996). A graphical display of large correlation 
#' matrices. The American Statistician 50, 178-180.
#' @seealso \code{\link{ellipse.lm}}, \code{\link{ellipse.nls}}, 
#' \code{\link{ellipse.profile}}, \code{\link{ellipse.profile.nls}}, 
#' \code{\link{ellipse.arima0}},
#' \code{\link{plotcorr}}
#' 
#' @examples
#' # Plot an ellipse corresponding to a 95\% probability region for a
#' # bivariate normal distribution with mean 0, unit variances and 
#' # correlation 0.8.
#' plot(ellipse(0.8), type = 'l')
#' @rdname ellipse
#' @export
ellipse.default <- function (
    x, 
    scale = c(1, 1), centre = c(0, 0), 
    level = 0.95,
    t = sqrt(qchisq(level, 2)), 
    which = c(1, 2), 
    npoints = 100, ...)
{
    names <- c("x", "y")
    if (is.matrix(x)) {
    xind <- which[1]
    yind <- which[2]
    r <- x[xind, yind]
    
    if (missing(scale)) {
        scale <- sqrt(c(x[xind, xind], x[yind, yind]))
        if (scale[1] > 0) r <- r/scale[1]
        if (scale[2] > 0) r <- r/scale[2]
    }
    
    if (!is.null(dimnames(x)[[1]]))
        names <- dimnames(x)[[1]][c(xind, yind)]
    } else r <- x

    r <- min(max(r,-1),1)  # clamp to -1..1, in case of rounding errors
    d <- acos(r)
    a <- seq(0, 2 * pi, len = npoints)
    matrix(c(t * scale[1] * cos(a + d/2) + centre[1],
             t * scale[2] * cos(a - d/2) + centre[2]),
        npoints, 2, dimnames = list(NULL, names))
}

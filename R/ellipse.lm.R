#' Outline a pairwise confidence region for a linear model fit.
#' 
#' This function produces the ellipsoidal outline of a pairwise confidence
#' region for a linear model fit.
#' 
#' @param x The first argument should be an `lm` object, usually resulting 
#' from a call to `lm()`.
#' @param which Which selects the pair of parameters to be plotted. The default 
#' is the first two.
#' @param level The confidence level of the region.  Default 95\%.
#' @param t The t statistic on the boundary of the ellipse.
#' @param ... Other `ellipse.default` parameters may also be used.
#' 
#' @return
#' A matrix with columns `x` and `y` to outline the confidence region.
#' 
#' @details
#' The summary function is used to obtain the covariance matrix of the fitted parameters.
#' 
#' @seealso [ellipse.default()], [stats::summary.lm()]
#' 
#' @examples 
#' # Plot the estimate and joint 90\% confidence region for the displacement and cylinder 
#' # count linear coefficients in the mtcars dataset
#' data(mtcars)
#' fit <- lm(mpg ~ disp + cyl , mtcars)
#' s <- summary(fit)
#' plot(ellipse(fit, which = c('disp', 'cyl'), level = 0.90), type = 'l')
#' points(fit$coefficients['disp'], fit$coefficients['cyl'])
#' @keywords regression dplot
#' @export
ellipse.lm <- function (x, which = c(1, 2), level = 0.95, 
    t = sqrt(2 * qf(level, 2, x$df.residual)), ...) 
{
    s <- summary(x)
    ellipse.default(s$sigma^2 * s$cov.unscaled[which, which], 
        centre = x$coefficients[which], t = t, ...)
}


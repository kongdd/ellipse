"plotcorr" <-
  function (corr, outline = TRUE, dev = TRUE, col = 'grey', paropts = NULL, 
            numbers = FALSE, ...) 
{
  if (deparse(substitute(dev)) == "postscript") {
    cat("feature not yet implemented in ellipse for R, sorry\n")
#    postscript(preamble = ps.preamble.ellipse, font = ps.fonts.ellipse, 
#               ...)
#    assign("ellipse.fontnum", length(ps.fonts.ellipse), where = 0)
#    cat("postscript device started with ellipse fonts\n")
  }
  else if (is.function(dev)) 
    dev(...)
  par(pty = "s", mar = c(5, 0, 4, 0) + 0.1)
#  par(pty = "s", mar = c(5, 4, 4, 2) + 0.1)

  if (!is.null(paropts)) 
    par(paropts)
  if (is.null(corr)) 
    return(invisible())
  if ((!is.matrix(corr)) || (round(min(corr), 6) < -1) || (round(max(corr), 
                                                                 6) > 1)) 
    stop("Need a correlation matrix")
  rowdim <- dim(corr)[1]
  coldim <- dim(corr)[2]
  maxdim <- max(rowdim, coldim)
  rowlabs <- dimnames(corr)[[1]]
  collabs <- dimnames(corr)[[2]]
  if (is.null(rowlabs)) 
    rowlabs <- 1:rowdim
  if (is.null(collabs)) 
    collabs <- 1:coldim
#1st change
#cxy <- par("cxy")
#cxy[1] <- yinch(par("csi"))
#cxy[2] <- yinch(par("csi"))
#cxy <- c(0.57*xinch(par("csi")), yinch(par("csi")))
  cxy <- par("cin")/par("pin")
  xlabwidth <- (max(nchar(rowlabs)) * cxy[1])/cxy[2]
  ylabwidth <- (max(nchar(collabs)) * cxy[1])/cxy[2]
#  plot(c(-xlabwidth, maxdim + 0.5), c(0.5, maxdim + 1 + ylabwidth), 
#       type = "n", bty = "n", axes = FALSE, xlab = "", ylab = "", asp = 1)
  plot(c(-xlabwidth, maxdim + 0.5), c(0.5, maxdim + 1 + ylabwidth), 
       type = "n", bty = "n", axes = FALSE, xlab = "", ylab = "", asp = 1)
                                        #2nd change
                                        #par(cex = 0.57*par("cex"))
                                        #par(cex = par("cex")/(par("cxy")[2]))
  text(rep(0, rowdim), rowdim:1, labels = rowlabs, adj = 1)
  text(1:coldim, rep(rowdim + 1, coldim), labels = collabs, 
       srt = 90, adj = 0)
  cols <- rep(1:coldim, rep(rowdim, coldim))
  rows <- rep(1:rowdim, coldim)
  if (!numbers) {
#    if (names(dev.cur()) == "postscript") {
#      text(cols, rowdim + 1 - rows, ellipse.chars[round((as.vector(corr) + 
#                                                         1)/2 * (length(ellipse.chars) - 1) + 1)], 
#           col = col)
#      if (outline) 
#        text(cols, rowdim + 1 - rows, ellipse.chars[round((as.vector(corr) + 
#                                                           1)/2 * (length(ellipse.chars) - 1) + 1)])
#    }
#    else {
      mat <- diag(c(1, 1))
      for (i in 1:length(cols)) {
        mat[1, 2] <- as.vector(corr)[i]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + cols[i]
        ell[, 2] <- ell[, 2] + rowdim + 1 - rows[i]
        polygon(ell, col = col)
        if (outline) 
          lines(ell)
      }
#    }
  }
  else text(cols + 0.3, rowdim + 1 - rows, round(10 * as.vector(corr), 
                                                 0), adj = 1, cex = 0.75 * par("cex"))
  invisible()
}








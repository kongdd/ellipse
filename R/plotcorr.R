"plotcorr" <-
  function (corr, outline = TRUE, col = 'grey', numbers = FALSE, bty = "n", axes = FALSE,
            xlab = "", ylab = "", asp = 1, cex.lab = par("cex.lab"), cex = 0.75*par("cex"),
			mar = 0.1 + c(2,2,4,2), ...) 
{
    savepar <- par(pty = "s", mar = mar)
    on.exit(par(savepar))

    if (is.null(corr)) return(invisible())
    if ((!is.matrix(corr)) || (round(min(corr, na.rm = TRUE), 6) < -1) 
			   || (round(max(corr, na.rm = TRUE), 6) > 1)) 
	stop("Need a correlation matrix")

    plot.new()
    par(new = TRUE)

    rowdim <- dim(corr)[1]
    coldim <- dim(corr)[2]
    maxdim <- max(rowdim, coldim)

    rowlabs <- dimnames(corr)[[1]]
    collabs <- dimnames(corr)[[2]]
    if (is.null(rowlabs)) rowlabs <- 1:rowdim
    if (is.null(collabs)) collabs <- 1:coldim
    rowlabs <- as.character(rowlabs)
    collabs <- as.character(collabs)

    plt <- par('plt')
    xlabwidth <- max(strwidth(rowlabs,units='figure',cex=cex.lab))/(plt[2]-plt[1])
    xlabwidth <- xlabwidth*maxdim/(1-xlabwidth)
    ylabwidth <- max(strwidth(collabs,units='figure',cex=cex.lab))/(plt[4]-plt[3])
    ylabwidth <- ylabwidth*maxdim/(1-ylabwidth)

    plot(c(-xlabwidth-0.5, maxdim + 0.5), c(0.5, maxdim + 1 + ylabwidth), 
	 type = "n", bty = bty, axes = axes, xlab = "", ylab = "", asp = asp, 
	 cex.lab = cex.lab, ...)
    text(rep(0, rowdim), rowdim:1, labels = rowlabs, adj = 1, cex = cex.lab)
    text(1:coldim, rep(rowdim + 1, coldim), labels = collabs, 
	 srt = 90, adj = 0, cex = cex.lab)
    mtext(xlab,1,0)
    mtext(ylab,2,0) 
    cols <- rep(1:coldim, rep(rowdim, coldim))
    rows <- rep(1:rowdim, coldim)
    if (!numbers) {
	mat <- diag(c(1, 1))
	for (i in 1:length(cols)) {
	    mat[1, 2] <- as.vector(corr)[i]
	    mat[2, 1] <- mat[1, 2]
	    ell <- ellipse(mat, t = 0.43)
	    ell[, 1] <- ell[, 1] + cols[i]
	    ell[, 2] <- ell[, 2] + rowdim + 1 - rows[i]
	    polygon(ell, col = col)
	    if (outline) lines(ell)
	}
    }
    else text(cols + 0.3, rowdim + 1 - rows, round(10 * as.vector(corr), 
						   0), adj = 1, cex = cex)
    invisible()
}








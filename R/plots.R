#' @title Plot weights from \code{qsmooth} function.
#' 
#' @description This function plots a scatterplot
#' showing the \code{qsmoothWeights} along the y-axis
#' and the quantiles on the x-axis.
#' 
#' @param object a qsmooth object from \code{qsmooth} 
#' @param xLab label for x-axis. Default is "quantiles"
#' @param yLab label for y-axis. Default is "weights"
#' @param mainLab title of plot. Default is "qsmooth weights"
#' 
#' @return A scatterplot will be created showing the 
#' \code{qsmoothWeights} along the y-axis and the 
#' quantiles on the x-axis.
#' 
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics abline
#' 
#' @export
#' @examples
#' dat <- cbind(matrix(rnorm(1000), nrow=100, ncol=10), 
#'              matrix(rnorm(1000, .1, .7), nrow=100, ncol=10))
#' dat_qs <- qsmooth(object = dat, 
#'                   group_factor = rep(c(0,1), each=10))
#' qsmoothPlotWeights(dat_qs)
#' 
qsmoothPlotWeights <- function(object,
            xLab = "quantiles", yLab = "weights", 
            mainLab = "qsmooth weights"){
    oldpar = par(mfrow=c(1,1), mar=c(4, 4, 1.5, 0.5))
    w = object@qsmoothWeights
    lq = nrow(object@qsmoothData)
    u = (seq_len(lq) - 0.5) / lq
    
    if (length(u) > 1e4) { # do not plot > 10000 points
        sel = sample(seq_len(lq), 1e4)
        plot(u[sel], w[sel], pch=".", main = mainLab,
             xlab = xLab, ylab = yLab, ylim=c(0, 1))
    } else{
        plot(u, w, pch=".", main = mainLab,
             xlab = xLab, ylab = yLab, ylim=c(0, 1))
    }
    abline(h=0.5, v=0.5, col="red", lty=2)
    par(oldpar)
}
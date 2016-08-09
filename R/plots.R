#' @title Plot weights from \code{qsmooth} function.
#' 
#' @description This function plots the histogram of the
#' null test statistics from permutation test in the \code{qsmooth}
#' function. 
#' 
#' @param object a qsmooth object from \code{qsmooth} 
#' @param savePlot a TRUE/FALSE object argument
#' determining if the plot will be saved for further use or
#' immediately displayed on the screen.
#' @param xLab label for x-axis
#' @param yLab label for y-axis
#' @param mainLab title of plot
#' @param binWidth binwidth or histogram. Default is the stat_bin default. 
#' 
#' @return A histogram will be plotted containing the null 
#' test statistics when using boostrapped samples. 
#' The red line is the observed test statistic
#' \code{quantroStat} from \code{qsmooth()}.
#' 
#' @export
#' @examples
#' \donttest{
#' library(minfi)
#' data(flowSorted)
#' p <- getBeta(flowSorted, offset = 100)
#' pd <- pData(flowSorted)
#' 
#' library(doParallel)
#' registerDoParallel(cores=4)
#' qtest <- quantro(p, pd$CellType, B = 100)
#' quantroPlot(qtest)
#' }
qsmoothPlotWeights <- function(object, savePlot = FALSE, xLab = NULL, 
                               yLab = NULL, mainLab = NULL, binWidth = NULL){
    oldpar = par(mfrow=c(1,1), mar=c(4, 4, 1.5, 0.5))
    w = object@weights
    lq = nrow(object@qsmoothData)
    u = (1:lq - 0.5) / lq
    
    if (length(u) > 1e4) { # do not plot more than 10000 points
        sel = sample(1:lq, 1e4)
        plot(u[sel], w[sel], pch=".", main="qsmooth weights",
             xlab=" quantiles", ylab="Weight", ylim=c(0, 1))
    } else{
        plot(u, w, pch=".", main="qsmooth weights",
             xlab="quantiles", ylab="Weight", ylim=c(0, 1))
    }
    abline(h=0.5, v=0.5, col="red", lty=2)
    
    par(oldpar)
}

# qsmoothPlotWeights()
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
#' library(SummarizedExperiment)
#' library(bodymapRat)
#' bm_dat <- bodymapRat()
#' 
#' # select lung and liver samples, stage 21 weeks, and bio reps
#' keep_columns = (colData(bm_dat)$organ %in% c("Lung", "Liver")) & 
#'          (colData(bm_dat)$stage == 21) & 
#'          (colData(bm_dat)$techRep == 1)
#' keep_rows = rowMeans(assay(bm_dat)) > 10 # Filter out low counts
#' bm_dat <- bm_dat[keep_rows,keep_columns]
#' 
#' qs_norm <- qsmooth(object = assay(bm_dat), 
#'                   group_factor = colData(bm_dat)$organ)
#' qsmoothPlotWeights(qs_norm)
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
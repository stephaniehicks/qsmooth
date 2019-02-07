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
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics abline
#' 
#' @export
#' @examples
#' library(SummarizedExperiment)
#' library(bodymapRat)
#' data(bodymapRat)
#' # select lung and liver samples, stage 21 weeks, and bio reps
#' keepColumns = (colData(bodymapRat)$organ %in% c("Lung", "Liver")) & 
#'          (colData(bodymapRat)$stage == 21) & 
#'          (colData(bodymapRat)$techRep == 1)
#' keepRows = rowMeans(assay(bodymapRat)) > 10 # Filter out low counts
#' bodymapRat <- bodymapRat[keepRows,keepColumns]
#' pd <- colData(bodymapRat)
#' pd$group <- paste(pd$sex, pd$organ, sep="_")
#' 
#' qsNorm <- qsmooth(object = asssay(bodymapRat)[keepRows, keepColumns], 
#'                   groupFactor = colData(bodymapRat)[keepColumns, 
#'                   ]$organ)
#' qsmoothPlotWeights(qsNorm)
qsmoothPlotWeights <- function(object, savePlot = FALSE,
                               xLab = NULL, yLab = NULL, 
                               mainLab = NULL, 
                               binWidth = NULL){
    oldpar = par(mfrow=c(1,1), mar=c(4, 4, 1.5, 0.5))
    w = object@weights
    lq = nrow(object@qsmoothData)
    u = (seq_len(lq) - 0.5) / lq
    
    if (length(u) > 1e4) { # do not plot > 10000 points
        sel = sample(seq_len(lq), 1e4)
        plot(u[sel], w[sel], pch=".", main="qsmooth weights",
             xlab=" quantiles", ylab="Weight", ylim=c(0, 1))
    } else{
        plot(u, w, pch=".", main="qsmooth weights",
             xlab="quantiles", ylab="Weight", ylim=c(0, 1))
    }
    abline(h=0.5, v=0.5, col="red", lty=2)
    par(oldpar)
}
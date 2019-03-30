#' @title the qsmooth class
#'
#' @description  Objects of this class store all 
#' the values needed information to work with a
#' qsmooth object
#' 
#' @slot qsmoothWeights qsmooth weights
#' @slot qsmoothData qsmooth normalized data
#' 
#' @return \code{qsmoothWeights} returns the qsmooth weights and 
#' \code{qsmoothData} returns the qsmooth normalized data 
#' 
#' @name qsmooth-class
#' @import methods
#' @exportClass qsmooth
#' @aliases qsmooth-class
#'
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
#'
setClass(
    Class = "qsmooth", 
      slot = list(
        qsmoothWeights = "numeric", qsmoothData = "matrix")
)

#' @importFrom utils head
#' @importFrom utils tail
setMethod("show", "qsmooth", 
          function(object){
            cat("qsmooth: Smooth quantile normalization\n")
            cat("   qsmoothWeights (length =", 
                length(object@qsmoothWeights), "):", "\n")
            cat(c(head(round(object@qsmoothWeights,3), n=3), "...", 
                   tail(round(object@qsmoothWeights,3), n=3)), "\n")
            cat("   qsmoothWeights (nrows =", 
                dim(object@qsmoothData)[1], ", ncols =", 
                dim(object@qsmoothData)[2], "):", "\n")
            print(head(object@qsmoothData))
            if(dim(object@qsmoothData)[1] > 6){
              cat(" ....... \n") }
          }
)

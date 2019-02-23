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
#' data(bodymapRat)
#' 
#' # select lung and liver samples, stage 21 weeks, and bio reps
#' keepColumns = (colData(bodymapRat)$organ %in% c("Lung", "Liver")) & 
#'          (colData(bodymapRat)$stage == 21) & 
#'          (colData(bodymapRat)$techRep == 1)
#' keepRows = rowMeans(assay(bodymapRat)) > 10 # Filter out low counts
#' bodymapRat <- bodymapRat[keepRows,keepColumns]
#' 
#' qsNorm <- qsmooth(object = assay(bodymapRat), 
#'                   groupFactor = colData(bodymapRat)$organ)
#'
setClass(
    Class = "qsmooth", 
      slot = list(
        qsmoothWeights = "numeric", qsmoothData = "matrix")
)



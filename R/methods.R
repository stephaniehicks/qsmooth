#' @title Accessors for the 'qsmoothWeights' slot of a qsmooth object.
#' 
#' @description Accessors for the 'qsmoothWeights' slot of a qsmooth object.
#' 
#' @usage
#' \S4method{qsmoothWeights}{qsmooth}(object)
#'
#' @docType methods
#' @name qsmoothWeights
#' @rdname qsmoothWeights
#' @aliases qsmoothWeights qsmoothWeights,qsmooth-method
#' @param object an object of class \code{qsmooth}.
#' 
#' @return The weights calculated for each feature 
#' after applying smoothed quantile normalization. 
#' 
#' @export
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
#' qsmoothWeights(qsNorm)
#' 
setMethod(
  f = "qsmoothWeights", 
  signature = "qsmooth",
  definition = function(object) {
            return(object@qsmoothWeights)
          }
)

#' @title Accessors for the 'qsmoothData' slot of a qsmooth object.
#' 
#' @description Accessors for the 'qsmoothData' slot of a qsmooth object.
#' 
#' @usage
#' \S4method{qsmoothData}{qsmooth}(object)
#'
#' @docType methods
#' @name qsmoothData
#' @rdname qsmoothData
#' @aliases qsmoothData qsmoothData,qsmooth-method
#' @param object an object of class \code{qsmooth}.
#' 
#' @return The normalized data after applying 
#' smoothed quantile normalization. 
#' 
#' @export
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
#' qsmoothData(qsNorm)
#' 
setMethod(
  f = "qsmoothData", 
  signature = "qsmooth",
  definition = function(object) {
            return(object@qsmoothData)
          }
)

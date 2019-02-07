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
#' @param object a \code{qsmooth} object
#' @param ... other 
#' 
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
#' qsmoothWeights(qsNorm)
#' 
qsmoothWeights.qsmooth <- function(object) object@qsmoothWeights


#' @rdname qsmoothWeights
#' @export
#' @param object an object of class \code{qsmooth}.
setMethod("qsmoothWeights", signature(object="qsmooth"), 
          qsmoothWeights.qsmooth)

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
#' @param object a \code{qsmooth} object
#' @param ... other 
#' 
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
#' qsmoothData(qsNorm)
#' 
qsmoothData.qsmooth <- function(object) object@qsmoothData

#' @rdname qsmoothData
#' @export
#' data
#' @param object an object of class \code{qsmooth}.
setMethod("qsmoothData", signature(object="qsmooth"), 
          qsmoothData.qsmooth)

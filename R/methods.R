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
qsmoothWeights.qsmooth <- function(object) object@qsmoothWeights

#' @rdname qsmoothWeights
#' @export
setMethod("qsmoothWeights", signature(object="qsmooth"), qsmoothWeights.qsmooth)


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
qsmoothData.qsmooth <- function(object) object@qsmoothData

#' @rdname qsmoothData
#' @export
setMethod("qsmoothData", signature(object="qsmooth"), qsmoothData.qsmooth)


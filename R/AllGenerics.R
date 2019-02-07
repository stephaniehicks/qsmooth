#' Generic function that returns the qsmooth weights
#'
#' Given a qsmooth object, this function returns the 
#' qsmooth weights
#' @param object a qsmooth object
#' @return qsmooth weights
#' @rdname qsmoothWeights
#' @export
setGeneric(
  name = "qsmoothWeights", 
  def = function(object) { 
    standardGeneric("qsmoothWeights") 
  }
)

#' Generic function that returns the qsmooth normalized data
#'
#' Given a qsmooth object, this function returns the 
#' qsmooth normalized data
#' @param object a qsmooth object
#' @return qsmooth normalized data
#' @rdname qsmoothData
#' @export
setGeneric(
  name = "qsmoothData", 
  def = function(object) { 
    standardGeneric("qsmoothData") 
  }
)
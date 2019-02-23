#' Generic function that returns the qsmooth weights
#'
#' Given a qsmooth object, this function returns the 
#' qsmooth weights
#' @rdname qsmoothWeights
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
#' @rdname qsmoothData
setGeneric(
  name = "qsmoothData", 
  def = function(object) { 
    standardGeneric("qsmoothData") 
  }
)
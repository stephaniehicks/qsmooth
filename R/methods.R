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
#' dat <- cbind(matrix(rnorm(1000), nrow=100, ncol=10), 
#'              matrix(rnorm(1000, .1, .7), nrow=100, ncol=10))
#' dat_qs <- qsmooth(object = dat, 
#'                   group_factor = rep(c(0,1), each=10))
#' qsmoothWeights(dat_qs)
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
#' dat <- cbind(matrix(rnorm(1000), nrow=100, ncol=10), 
#'              matrix(rnorm(1000, .1, .7), nrow=100, ncol=10))
#' dat_qs <- qsmooth(object = dat, 
#'                   group_factor = rep(c(0,1), each=10))
#' qsmoothData(dat_qs)
#' 
setMethod(
  f = "qsmoothData", 
  signature = "qsmooth",
  definition = function(object) {
            return(object@qsmoothData)
          }
)

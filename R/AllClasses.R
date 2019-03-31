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
#' dat <- cbind(matrix(rnorm(1000), nrow=100, ncol=10), 
#'              matrix(rnorm(1000, .1, .7), nrow=100, ncol=10))
#' dat_qs <- qsmooth(object = dat, 
#'                   group_factor = rep(c(0,1), each=10))
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
            cat("   qsmoothData (nrows =", 
                dim(object@qsmoothData)[1], ", ncols =", 
                dim(object@qsmoothData)[2], "):", "\n")
            print(head(object@qsmoothData))
            if(dim(object@qsmoothData)[1] > 6){
              cat(" ....... \n") }
          }
)

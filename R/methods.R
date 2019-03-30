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
#' qsmoothWeights(qs_norm)
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
#' qs_norm
#' qsmoothData(qs_norm)
#' 
setMethod(
  f = "qsmoothData", 
  signature = "qsmooth",
  definition = function(object) {
            return(object@qsmoothData)
          }
)

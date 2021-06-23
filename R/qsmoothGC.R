.smooth_gcqn <- function(counts, 
                         gc,
                         group_factor, 
                         nGroups=50,
                         round=TRUE,
                         ...){
  gc_groups <- Hmisc::cut2(gc, g = nGroups)
  gcBinNormCounts <- matrix(NA, nrow=nrow(counts), ncol=ncol(counts), 
                            dimnames=list(rownames(counts),colnames(counts)))
  for(ii in seq_len(nlevels(gc_groups))){
    id <- which(gc_groups==levels(gc_groups)[ii])
    countBin <- counts[id,]
    qs <- qsmooth::qsmooth(countBin, group_factor=group_factor, ...)
    normCountBin <- qsmooth::qsmoothData(qs)
    if(round) normCountBin <- base::round(normCountBin)
    normCountBin[normCountBin<0] <- 0
    gcBinNormCounts[id,] <- normCountBin
  }
  return(gcBinNormCounts)
}


#' @title qsmoothGC
#' 
#' @description This function applies a generalization of 
#' quantile normalization called smoothed quantile 
#' normalization. This function defines the qsmooth class 
#' and constructor. 
#'
#' @param object an object which is a \code{matrix} or 
#' \code{data.frame} with observations (e.g. probes or genes) on 
#' the rows and samples as the columns. Alternatively, 
#' a user can provide a \code{SummarizedExperiment} object
#' and the \code{assay(object, "counts")} will be used as input 
#' for the qsmooth normalization.
#' @param gc GC-content of the features.
#' @param group_factor a group level continuous or categorial 
#' covariate associated with each sample or column in the 
#' \code{object}. The order of the \code{group_factor} must 
#' match the order of the columns in \code{object}.
#' @param nGroups The number of equally-sized bins used to group the 
#' GC-content values. Groups are created using \code{Hmisc::cut2}.
#' @param round Should normalized values be rounded to integers?
#' @param ... (Optional) Additional arguments passed to \code{\link{qsmooth}}.
#' @export 
#' 
#' @return A matrix of normalized counts.
#' 
#' @aliases qsmoothGC
#' 
#' @docType methods
#' @name qsmoothGC
#' @importFrom sva ComBat
#' @importFrom SummarizedExperiment assays
#' @importFrom SummarizedExperiment assay
#' @importFrom stats ave
#' @importFrom Hmisc cut2
#' 
#' @examples
#' dat <- cbind(matrix(rnorm(1000), nrow=100, ncol=10), 
#'              matrix(rnorm(1000, .1, .7), nrow=100, ncol=10))
#' gc <- runif(n=100, min=0.2, max=0.9)
#' dat_qs <- qsmoothGC(object = dat, 
#'                    gc = gc,
#'                    group_factor = rep(c(0,1), each=10))
#' 
#' @rdname qsmoothGC
#' @export
qsmoothGC <- function(object, 
                      group_factor,
                      gc,
                      nGroups = 50,
                      round = TRUE,
                      ...){
  
  if(!any(is(object, "matrix") | is(object, "data.frame") |
          is(object, "SummarizedExperiment"))){
    stop("The class of the object must be a matrix, 
         data.frame or SummarizedExperiment")
  }
  
  if(is.data.frame(object)){ object <- as.matrix(object) }
  
  if(is(object, "SummarizedExperiment")){ 
    if("counts" %in% names(assays(object))){ 
      object <- assay(object, "counts")
    } else {
      stop("There is no assay slot named 'counts' inside 
           the object. Please check the names of the 
           assay slots using names(assays(object)).")
    }
  }
  
  objectNorm <- .smooth_gcqn(counts = object,
                             gc = gc,
                             group_factor = group_factor,
                             nGroups = nGroups, 
                             round = round,
                             ...)
  results <- new("qsmooth")
  results@qsmoothData <- objectNorm
  return(results)
}
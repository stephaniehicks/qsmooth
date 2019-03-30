#' @title qsmooth
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
#' @param group_factor a group level continuous or categorial 
#' covariate associated with each sample or column in the 
#' \code{object}. The order of the \code{group_factor} must 
#' match the order of the columns in \code{object}.
#' @param batch (Optional) batch covariate (multiple 
#' batches are not allowed). 
#' If batch covariate is provided, \code{Combat()} from 
#' \code{sva} is used prior to qsmooth normalization to 
#' remove batch effects. See \code{Combat()} for more details. 
#' @param norm_factors optional normalization scaling factors.
#' @param window window size for running median which is 
#' a fraction of the number of rows in \code{object}. 
#' Default is 0.05. 
#' @export 
#' 
#' @return A object of the class \code{qsmooth} that 
#' contains a numeric vector of the qsmooth weights in 
#' the \code{qsmoothWeights} slot and a matrix of normalized  
#' values after applying smoothed quantile normalization in 
#' the \code{qsmoothData} slot. 
#' 
#' @details 
#' Quantile normalization is one of the most widely used 
#' normalization tools for data analysis in genomics. Although it 
#' was originally developed for gene expression microarrays it is
#' now used across many different high-throughput applications 
#' including RNAseq and ChIPseq. The methodology 
#' relies on the assumption that observed changes in the empirical 
#' distribution of samples are due to unwanted variability. 
#' Because the data is transformed to remove these differences 
#' it has the potential to remove interesting biologically driven
#' global variation. Therefore, applying quantile normalization, 
#' or other global normalization methods that rely on similar 
#' assumptions, may not be an appropriate depending on the type 
#' and source of variation. 
#' 
#' This function computes a weight at every 
#' quantile that compares the variability between groups 
#' relative to within groups. In one extreme quantile 
#' normalization is applied and in the other extreme quantile 
#' normalization within each biological condition is applied. 
#' The weight shrinks the group-level quantile normalized data 
#' towards the overall reference quantiles if variability 
#' between groups is sufficiently smaller than the variability 
#' within groups. 
# 
#' See the vignette for more details. 
#' 
#' @aliases qsmooth
#' 
#' @docType methods
#' @name qsmooth
#' @importFrom sva ComBat
#' @importFrom SummarizedExperiment assays
#' @importFrom SummarizedExperiment assay
#' @importFrom stats ave
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
#' 
#' @rdname qsmooth
#' @export
qsmooth <- function(object, group_factor,
                    batch = NULL, norm_factors = NULL, 
                    window = 0.05)
{

  if(!(class(object) %in% c("matrix", "data.frame", 
                       "SummarizedExperiment"))){
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
  
  if(is.null(group_factor)){  
    stop("Must provide group_factor to specify the group 
        level information associated with each sample or 
             or column in object.")
  }
  
  if(ncol(object) != length(group_factor)){
    stop("Number of columns in object does not match 
             length of group_factor.")
  }
  
  if(is.factor(group_factor) & length(levels(group_factor)) < 2){
    stop(paste0("group_factor is a factor and number of levels in 
                  group_factor is less than 2 (levels(group_factor): ",
                levels(group_factor), "). Must provide a factor with 
                  2 or more levels to use qsmooth."))
  }
  
  if(any(is.na(object))){ stop("Object must not contains NAs.") }    
  
  # Scale normalization step
  if(!is.null(norm_factors)) { 
    object <- sweep(object, 2, norm_factors, FUN = "/") 
  }
  
  # If batch is provided, run sva::ComBat to remove batch effects
  if(!is.null(batch)){
    object <- ComBat(object, factor(batch))
  }
  
  # Compute quantile statistics
  qs <- qstats(object=object, group_factor=group_factor,
               window=window)
  Qref <- qs$Qref 
  Qhat <- qs$Qhat  
  w <- qs$smoothWeights
  
  # Weighted quantiles
  objectNorm = w * Qref + (1 - w) * Qhat
  
  # Re-order objectNorm by rank of object (columnwise)
  for (i in seq_len(ncol(objectNorm))) {
    # Grab ref. i
    ref = objectNorm[,i]
    
    # Grab object column i
    x = object[,i]
    
    # Grab ranks of x (using min rank for ties)
    rmin = rank(x, ties.method="min")
    
    # If x has rank ties then average the values of ref at 
    # those ranks
    dups = duplicated(rmin)
    
    if (any(dups)) {
      # Grab ranks of x (using random ranks for ties) 
      # (needed to uniquely identify the indices of tied ranks)
      rrand = rank(x, ties.method="random")
      
      # Grab tied ranks
      tied.ranks = unique(rmin[dups])
      
      for (k in tied.ranks) {
        # Select the indices of tied ranks 
        sel = rrand[rmin == k] 
        ref[sel] = ave(ref[sel])
      }
    }
    
    # Re-order ref and replace in objectNorm
    objectNorm[,i] = ref[rmin]
  }
  
  results <- new("qsmooth")
  results@qsmoothWeights <- w
  
  rownames(objectNorm) = rownames(object)
  colnames(objectNorm) = colnames(object)
  results@qsmoothData <- objectNorm
  
  return(results)
}
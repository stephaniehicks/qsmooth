#' @title qsmooth
#' 
#' @description This function applies a generalization of 
#' quantile normalization called smoothed quantile 
#' normalization. This function defines the qsmooth class 
#' and constructor. 
#'
#' @param object an object which is a data frame or 
#' matrix with observations (e.g. probes or genes) on 
#' the rows and samples as the columns.  
#' @param groupFactor a group level continuous or categorial 
#' covariate associated with each sample or column in the 
#' \code{object}. The order of the \code{groupFactor} must 
#' match the order of the columns in \code{object}. 
#' @param batch (Optional) batch covariate (multiple 
#' batches are not allowed). 
#' If batch covariate is provided, \code{Combat()} from 
#' \code{sva} is used prior to qsmooth normalization to 
#' remove batch effects. See \code{Combat()} for more details. 
#' @param normFactors optional normalization scaling factors.
#' @param window window size for running median which is 
#' a fraction of the number of rows in \code{object}. 
#' Default is 0.05. 
#' @export 
#' 
#' @return A normalized matrix of values after applying 
#' smoothed quantile normalization. 
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
#' @importFrom SummarizedExperiment assay
#' @importFrom stats ave
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
#' qsNorm
#' 
#' @rdname qsmooth
#' @export
qsmooth <- function(object, groupFactor, 
                    batch = NULL, normFactors = NULL, 
                    window = 0.05)
{
  
  if(is.null(groupFactor)){  
    stop("Must provide groupFactor to specify the group 
        level information associated with each sample or 
             or column in object.")
  }
  
  if(ncol(object) != length(groupFactor)){
    stop("Number of columns in object does not match 
             length of groupFactor.")
  }
  
  if(is.factor(groupFactor) & length(levels(groupFactor)) < 2){
    stop(paste0("groupFactor is a factor and number of levels in 
                  groupFactor is less than 2 (levels(groupFactor): ",
                levels(groupFactor), "). Must provide a factor with 
                  2 or more levels to use qsmooth."))
  }
  
  if(any(is.na(object))){ stop("Object must not contains NAs.") }    
  
  # Scale normalization step
  if(!is.null(normFactors)) { 
    object <- sweep(object, 2, normFactors, FUN = "/") 
  }
  
  # If batch is provided, run sva::ComBat to remove batch effects
  if(!is.null(batch)){
    object <- ComBat(object, factor(batch))
  }
  
  # Compute quantile statistics
  qs <- qstats(object=object, groupFactor=groupFactor,
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
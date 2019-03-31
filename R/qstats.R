#' @title qstats 
#'
#' @description This function is a helper function that 
#' computes quantile statistics for the function 
#' \code{qsmooth}. 
#'
#' @param object an object which is a data frame or 
#' matrix with observations (e.g. probes or genes) on 
#' the rows and samples as the columns.  
#' @param group_factor a group level continuous or categorial 
#' covariate associated with each sample or column in the 
#' \code{object}. The order of the \code{group_factor} must 
#' match the order of the columns in \code{object}. 
#' @param window window size for running median which is 
#' a fraction of the number of rows in \code{object}. 
#' Default is 0.05. 
#' 
#' @return A list of quantile statistics including 
#' \item{Q}{sample quantiles}
#' \item{Qref}{reference quantile}
#' \item{Qhat}{linear model fit at each quantile}
#' \item{SST}{total sum of squares}
#' \item{SSB}{between sum of squares}
#' \item{SSE}{within sum of squares}
#' \item{roughWeights}{SSE / SST}
#' \item{smoothWeights}{smoothed weights computed using a 
#' running median with a given \code{window} size.}
#' 
#' @aliases qstats
#' 
#' @docType methods
#' @importFrom stats runmed
#' @importFrom stats model.matrix
#'
#' @examples
#' dat <- cbind(matrix(rnorm(1000), nrow=100, ncol=10), 
#'              matrix(rnorm(1000, .1, .7), nrow=100, ncol=10))
#' qs <- qstats(object = dat, 
#'              group_factor = rep(c(0,1), each=10), 
#'              window = 0.05)
#' 
#' @rdname qstats
#' @export
qstats <- function(object, group_factor, window = 0.05)
  {
  # Compute sample quantiles
  Q = apply(object, 2, sort) 
  
  # Compute quantile reference
  Qref = rowMeans(Q)  
  
  # Compute SST
  SST = rowSums((Q - Qref)^2)
  
  # Compute SSB
  if(is.factor(group_factor)){
    X = model.matrix(~ 0 + group_factor)
  } else {
    X = model.matrix(~ group_factor)
  }
  
  QBETAS = t(solve(t(X) %*% X) %*% t(X) %*% t(Q))
  Qhat = QBETAS %*% t(X)
  SSB = rowSums((Qhat - Qref)^2)
  
  # Compute weights
  roughWeights = 1 - SSB / SST
  roughWeights[SST < 1e-6] = 1
  
  # Compute smooth weights
  k = floor(window * nrow(Q))
  if (k %% 2 == 0) k = k + 1
  smoothWeights = runmed(roughWeights, k = k, endrule="constant")
  
  list(Q=Q, Qref=Qref, Qhat=Qhat, SST=SST, SSB=SSB, SSE=SST-SSB, 
       roughWeights=roughWeights, smoothWeights=smoothWeights)
}


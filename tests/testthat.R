context("checking the qsmooth function")
library(testthat)
library(qsmooth)

test_that("checking dimensions of qsmooth object", {
  n_genes <- 1000
  n_samples <- 10
  set.seed(1234)
  x <- matrix(rnorm(n_genes*n_samples), n_genes, n_samples)
  group_factor <- rep(c(1,2),each=5)
  qsNorm <- qsmooth(object = x, 
                    groupFactor = group_factor)
  
  expect_equal(length(qsNorm@weights), n_genes)
  expect_equal(nrow(qsNorm@qsmoothData), n_genes)
  expect_equal(ncol(qsNorm@qsmoothData), n_samples)
})

test_that("checking qsmooth normalized data", {
  n_genes <- 1000
  n_samples <- 10
  set.seed(1234)
  x <- matrix(rnorm(n_genes*n_samples), n_genes, n_samples)
  group_factor <- rep(c(1,2),each=5)
  qsNorm <- qsmooth(object = x, 
                    groupFactor = group_factor)
  expect_equal(qsmoothData(qsNorm)[1,1], -1.25856, tolerance = .0001)
  expect_equal(qsmoothWeights(qsNorm)[1], 0.97495, tolerance = .0001)
})

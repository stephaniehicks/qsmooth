library(testthat)
context("checking the qsmooth function")

library(qsmooth)

test_that("checking dimensions of qsmooth object", {
  n_genes <- 1000
  n_samples <- 10
  set.seed(1234)
  x <- matrix(rnorm(n_genes*n_samples), n_genes, n_samples)
  group_factor <- rep(c(1,2),each=5)
  qs_norm <- qsmooth(object = x, 
                    group_factor = group_factor)
  
  expect_equal(length(qsmoothWeights(qs_norm)), n_genes)
  expect_equal(nrow(qsmoothData(qs_norm)), n_genes)
  expect_equal(ncol(qsmoothData(qs_norm)), n_samples)
})

test_that("checking qsmooth normalized data", {
  n_genes <- 1000
  n_samples <- 10
  set.seed(1234)
  x <- matrix(rnorm(n_genes*n_samples), n_genes, n_samples)
  group_factor <- rep(c(1,2),each=5)
  qs_norm <- qsmooth(object = x, group_factor = group_factor)
  expect_equal(qsmoothData(qs_norm)[1,1], -1.25856, tolerance = .0001)
  expect_equal(qsmoothWeights(qs_norm)[1], 0.97495, tolerance = .0001)
})


test_that("checking qsmoothGC normalized data", {
  n_genes <- 1000
  n_samples <- 10
  set.seed(1234)
  x <- matrix(rpois(n_genes*n_samples, lambda=12), n_genes, n_samples)
  group_factor <- rep(c(1,2),each=5)
  gc <- runif(n=n_genes, min=0.2, max=0.9)
  ## one group should be same as qsmooth
  qs_norm <- qsmooth(object = x, group_factor = group_factor)
  qs_normGC <- qsmoothGC(object = x, 
                         gc = gc, 
                         group_factor = group_factor, 
                         nGroups = 1,
                         round = FALSE)
  expect_equal(qsmoothData(qs_norm), qsmoothData(qs_normGC), tolerance = .001)
})

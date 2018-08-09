context("NPI calculations")
library(imptree)

structure_tests <- function(pI, nclass) {
  expect_equal(length(pI), 5)
  expect_named(pI,  c("probint", "maxEntDist", "maxEntCorr", "minEntDist", "minEntCorr"))
  
  expect_equivalent(sapply(pI, length), c(3 * nclass, nclass, 1, nclass, 1))
  expect_equal(rownames(pI$probint), c("Frequency", "Lower", "Upper"))
  expect_null(colnames(pI$probint))
  expect_equal(dim(pI$probint), c(3, nclass))
}

calcEntropyBase<- function(values, nobs) {
  -sum(values[values>0] * log(values[values>0], base = 2))
}

value_tests <- function(pI, values, entropyfun) {
  expect_equal(pI$probint[1,], values[[1]])
  expect_equal(pI$probint[2,], pmax(0, (values[[1]] - 1) / sum(values[[1]])))
  expect_equal(pI$probint[3,], pmin(1, (values[[1]] + 1) / sum(values[[1]])))
  expect_equal(pI$maxEntDist, values[[2]])
  expect_equal(pI$maxEntCorr, entropyfun(values[[2]]), sum(values[[1]]))
  expect_equal(pI$minEntDist, values[[3]])
  expect_equal(pI$minEntCorr, entropyfun(values[[3]]), sum(values[[1]]))
}


ip <- "NPI"


test_that("Exact NPI, krem=0", {
  
  vec <- c(0,0,1,1)
  pI <- probInterval(vec, ip, correction = "no")
  
  expected_vals <- list(vec, 
                        rep(0.25, 4), 
                        c(0.5, 0.5, 0, 0))
  
  structure_tests(pI, length(vec))
  value_tests(pI, expected_vals, calcEntropyBase)
})

test_that("Exact NPI, krem < k0; h < k1+1", {
  
  vec <- c(1,0,0,4)
  pI <- probInterval(vec, ip, correction = "no")
  
  expected_vals <- list(vec, 
                        c(rep(4/30, 3), 0.6),
                        c(0, 0, 0, 1))
  
  structure_tests(pI, length(vec))
  value_tests(pI, expected_vals, calcEntropyBase)
})

test_that("Exact NPI, krem < k0; h > k1+1", {
  
  vec <- c(rep(0,6), 1:4)
  pI <- probInterval(vec, ip, correction = "no")
  
  expected_vals <- list(vec, 
                        c(rep(0.05, 6), 0.1, 0.1, 0.2, 0.3),
                        c(rep(0, 7), 0.1, 0.4, 0.5))
  
  structure_tests(pI, length(vec))
  value_tests(pI, expected_vals, calcEntropyBase)
})

test_that("Exact NPI, krem >= k0", {
  
  vec <- c(3,3,2,3,4,5,0,0)
  pI <- probInterval(vec, ip, correction = "no")
  
  expected_vals <- list(vec,
                        c(rep(0.1375,4), 0.15, 0.2, 0.05, 0.05),
                        c(0.2, 0.1, 0.05, 0.1, 0.25, 0.3, 0, 0))
  
  structure_tests(pI, length(vec))
  value_tests(pI, expected_vals, calcEntropyBase)
})

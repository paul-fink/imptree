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

value_tests <- function(pI, values) {
  expect_equal(pI$probint[1,], values[[1]])
  expect_equal(pI$probint[2,], pmax(0, (values[[1]] - 1) / sum(values[[1]])))
  expect_equal(pI$probint[3,], pmin(1, (values[[1]] + 1) / sum(values[[1]])))
  expect_equal(pI$maxEntDist, values[[2]])
  expect_equal(pI$maxEntCorr, values[[3]])
  expect_equal(pI$minEntDist, values[[4]])
  expect_equal(pI$minEntCorr, values[[5]])
}


ip <- "NPI"
vec <- c(1,2,1,1)

test_that("Exact NPI, small table, w/o 0, no corr", {
  pI <- probInterval(vec, ip, correction = "no")
  
  expected_vals <- list(vec, 
                        rep(0.25, 4), 
                        2,
                        c(0.4, 0.6, 0, 0),
                        -0.6 * log(0.6, base = 2) - 0.4 * log(0.4, base = 2))
  
  structure_test(pI, length(vec))
  value_tests(pI, expected_vals)
})

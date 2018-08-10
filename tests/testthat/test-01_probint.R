#------------------------ Preparations ------------------------------#

## test template #####################################################
structure_tests <- function(pI, hasmin, hasmax, nc) {
  expect_equal(length(pI), 1 + 2 * hasmax + 2 * hasmin)
  listnames <- c("probint", "maxEntDist", "maxEntCorr", "minEntDist",
                 "minEntCorr")[c(1, c(2,3) * hasmax, c(4,5) * hasmin)]

  expect_named(pI,  listnames)
  expect_equivalent(sapply(pI, length), 
                    c(3 * nc, rep(c(nc, 1), times = hasmin + hasmax)))
                                          
  expect_equal(rownames(pI$probint), c("Frequency", "Lower", "Upper"))
  expect_null(colnames(pI$probint))
  expect_equal(dim(pI$probint), c(3, nc))
}


# loading library
library(imptree)

#----------------- Test Suite starts here! --------------------------#

## some vector and iptype
ip <- "NPI"
vec <- c(0,0,1,1)

## Structure of probInterval #########################################
context("ProbInterval structure")

test_that("Probint, full", {
  pI <- probInterval(table = vec, iptype = ip, correction = "no",
                     entropymin = TRUE, entropymax = TRUE)
  structure_tests(pI, TRUE, TRUE, length(vec))
})

test_that("Probint, only max entropy", {
  pI <- probInterval(table = vec, iptype = ip, correction = "no",
                     entropymin = FALSE, entropymax = TRUE)
  structure_tests(pI, FALSE, TRUE, length(vec))
})

test_that("Probint, only min entropy", {
  pI <- probInterval(table = vec, iptype = ip, correction = "no",
                     entropymin = TRUE, entropymax = FALSE)
  structure_tests(pI, TRUE, FALSE, length(vec))
})

test_that("Probint, no entropy", {
  pI <- probInterval(table = vec, iptype = ip, correction = "no",
                     entropymin = FALSE, entropymax = FALSE)
  structure_tests(pI, FALSE, FALSE, length(vec))
})

context("discard_not_used_imputations")

test_that("test exception (longitudinal)", {
  
  datalist <- list(1:10, 1:10, 1:10, 1:10)
  names(datalist) <- rep("imp1pv3", 4)
  regr.coeff <- list(1:10, 1:10, 1:10, 1:10)
  EAP.rel <- list(1:10, 1:10, 1:10, 1:10)
  longitudinal <- TRUE
  
  test <- discard_not_used_imputations(datalist, regr.coeff, EAP.rel,
                                       longitudinal)
  expect_equal(names(test), c("regr.coeff", "EAP.rel"))
  expect_equal(length(test$EAP.rel), length(datalist))
  expect_equal(length(test$regr.coeff), length(datalist))
})


test_that("correct data structures returned (longitudinal)", {
  datalist <- list(1:10, 1:10, 1:10, 1:10)
  regr.coeff <- list(1:10, 1:10, 1:10, 1:10)
  EAP.rel <- list(1:10, 1:10, 1:10, 1:10)
  longitudinal <- TRUE
  names(datalist) <- c("imp1pv", "imp1pv", "imp3pv", "imp4pv")
  len <-
    length(unique(as.numeric(stringr::str_match(names(datalist),
                                                "imp\\s*(.*?)\\s*pv")[, 2])))
  
  test <- discard_not_used_imputations(datalist, regr.coeff, EAP.rel,
                                       longitudinal)
  expect_equal(names(test), c("regr.coeff", "EAP.rel"))
  expect_equal(length(test$EAP.rel), len)
  expect_equal(length(test$regr.coeff), len)
})


test_that("correct data structures returned (cross-sectional)", {
  datalist <- list(1:10, 1:10, 1:10, 1:10)
  names(datalist) <- c("imp1pv", "imp1pv", "imp3pv", "imp4pv")
  len <-
    length(unique(as.numeric(stringr::str_match(names(datalist),
                                                "imp\\s*(.*?)\\s*pv")[, 2])))
  longitudinal <- FALSE
  ncol <-
    max(as.numeric(stringr::str_match(names(datalist), "imp\\s*(.*?)\\s*pv")[, 2]))
  regr.coeff <- matrix(1:16, ncol = 2*ncol)
  EAP.rel <- 1:4
  
  test <- discard_not_used_imputations(datalist, regr.coeff, EAP.rel,
                                       longitudinal)
  expect_equal(names(test), c("regr.coeff", "EAP.rel"))
  expect_equal(length(test$EAP.rel), len)
  expect_equal(test$EAP.rel, c("imp1" = 1, "imp3" = 3, "imp4" = 4))
  expect_equal(ncol(test$regr.coeff), len*2)
})

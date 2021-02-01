context("reformat_cross_tmp_pvs")

test_that("reformat_cross_tmp_pvs", {
  pvs <- list(NULL)
  tmp_pvs <- replicate(3, data.frame(ID_t = 1:10, PV = 1:10, pweights = 1),
                       simplify =  FALSE)
  bgdata <- data.frame(ID_t = 1:10)

  result <- list(replicate(3, data.frame(ID_t = 1:10, PV = 1:10),
                           simplify = FALSE))
  expect_equal(reformat_cross_tmp_pvs(pvs, tmp_pvs, bgdata, npv = 3, i = 1),
               result)

  tmp_pvs <- replicate(3, data.frame(pid = 1:10, PV = 1:10, pweights = 1),
                       simplify =  FALSE)
  result <- list(replicate(3, data.frame(ID_t = 1:10, PV = 1:10),
                           simplify = FALSE))
  expect_equal(reformat_cross_tmp_pvs(pvs, tmp_pvs, bgdata = NULL, npv = 3,
                                      i = 1),
               result)
})

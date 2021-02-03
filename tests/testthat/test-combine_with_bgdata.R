context("combine_with_bgdata")

test_that("combine_with_bgdata", {
  bgdata <- data.frame(ID_t = 1:10, var = 1:10)
  school_data <- data.frame(ID_t = 1:10, var2 = 1:10, var2_schavg = 10:1)

  expect_equivalent(combine_with_bgdata(bgdata, school_data),
                    cbind(bgdata, var2_schavg = 10:1))

  expect_equivalent(combine_with_bgdata(bgdata, school_data[, -3]),
                    bgdata)
})

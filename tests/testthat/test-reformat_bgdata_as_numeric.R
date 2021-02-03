context("reformat_bgdata_as_numeric")

test_that("reformat_bgdata_as_numeric", {
  data <- data.frame(ID_t = 1:10, var1 = 1:10, var2 = as.character(1:10))
  expect_equivalent(reformat_bgdata_as_numeric(data),
                    data.frame(ID_t = 1:10, var1 = 1:10, var2 = 1:10))

  data <- data.frame(ID_t = 1:10,
                     var1 = as.character(1:10), var2 = as.factor(1:10))
  expect_equivalent(reformat_bgdata_as_numeric(data),
                    data.frame(ID_t = 1:10, var1 = 1:10, var2 = 1:10))
})

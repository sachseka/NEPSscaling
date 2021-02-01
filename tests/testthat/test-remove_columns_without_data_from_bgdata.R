context("remove_columns_without_data_from_bgdata")

test_that("remove_columns_without_data_from_bgdata: removal", {
  bgdata <- data.frame(ID_t = 1:10, var = NA)

  expect_equivalent(remove_columns_without_data_from_bgdata(bgdata),
                    bgdata[, -2, drop = FALSE])

  bgdata <- data.frame(ID_t = 1:10, var = NA, var2 = 1)

  expect_equivalent(remove_columns_without_data_from_bgdata(bgdata),
                    bgdata[, -2])
})

test_that("remove_columns_without_data_from_bgdata: no removal", {
  bgdata <- data.frame(ID_t = 1:10, var = 1)

  expect_equivalent(remove_columns_without_data_from_bgdata(bgdata),
                    bgdata)
})

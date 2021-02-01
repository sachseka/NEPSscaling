context("substitute_missing_persons_in_bgdata")

test_that("substitute_missing_persons_in_bgdata: substition", {
  bgdata <- data.frame(ID_t = 1:10, var = 1:10)
  data <- data.frame(ID_t = 1:20, var = 1:20)

  expect_equivalent(substitute_missing_persons_in_bgdata(bgdata, data),
                    dplyr::bind_rows(bgdata, data.frame(ID_t = 11:20)))
})

test_that("substitute_missing_persons_in_bgdata: no substitution", {
  bgdata <- data.frame(ID_t = 1:10, var = 1:10)
  data <- data.frame(ID_t = 1:10, var = 1:10)

  expect_equivalent(substitute_missing_persons_in_bgdata(bgdata, data),
                    bgdata)
})

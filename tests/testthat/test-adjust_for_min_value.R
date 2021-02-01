context("adjust_for_min_value")

test_that("adjust_for_min_value: min_valid > 0", {
  data <- data.frame(ID_t = 1:5)
  bgdata <- data.frame(ID_t = 1:10, var = 1:10)

  expect_equivalent(adjust_for_min_value(min_valid = 1, bgdata, data),
                    list(bgdata = bgdata[1:5, ], data = data))
})

test_that("adjust_for_min_value: min_valid == 0", {
  data <- data.frame(ID_t = 1:5, var = 1:5)
  bgdata <- data.frame(ID_t = 1:10, var = 1:10)

  expect_equivalent(adjust_for_min_value(min_valid = 0, bgdata, data),
                    list(bgdata = bgdata,
                         data = dplyr::bind_rows(data, data.frame(ID_t = 6:10))))
})

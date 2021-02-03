context("select_correct_cross_sc6_reading_subsample")

test_that("select_correct_cross_sc6_reading_subsample", {
  data <- data.frame(
    ID_t = 1:10,
    wave_w3 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
    wave_w5 = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
  )
  expect_equivalent(select_correct_cross_sc6_reading_subsample(data, "w3"),
                    data[6:10, ])
  expect_equivalent(select_correct_cross_sc6_reading_subsample(data, "w5"),
                    data[1:5, ])
})

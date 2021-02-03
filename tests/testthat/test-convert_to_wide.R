context("convert_to_wide")

test_that("convert_to_wide", {

  data <- data.frame(ID_t = rep(1:25, each = 4),
                     ID_i = sample(1:10, 100, replace = TRUE),
                     wave = rep(1:4, 25))

  result <- data.frame(ID_t = 1:25,
                       school_w1 = data$ID_i[data$wave == 1],
                       school_w2 = data$ID_i[data$wave == 2],
                       school_w3 = data$ID_i[data$wave == 3],
                       school_w4 = data$ID_i[data$wave == 4])

  expect_equivalent(convert_to_wide(data), result)
})

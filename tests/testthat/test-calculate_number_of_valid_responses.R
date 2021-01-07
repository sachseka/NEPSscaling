context("calculate_number_of_valid_responses")

test_that("number of valid responses (cross-sectional)", {
  longitudinal <- FALSE
  resp <- data.frame(ID_t = 1:100,
                     item1 = rbinom(100, 1, 0.5),
                     item2 = rbinom(100, 1, 0.5),
                     item3 = rbinom(100, 1, 0.5))
  resp$item1[1:10] <- NA
  resp$item2[11:20] <- NA
  resp$item3[6:15] <- NA

  waves <- c("_w1", "_w2")

  test <- calculate_number_of_valid_responses(longitudinal, resp, waves)
  expect_equal(test$valid, c(rep(2, 5), rep(1, 10), rep(2, 5), rep(3, 80)))
})


test_that("number of valid responses (longitudinal)", {
  resp <- data.frame(ID_t = 1:100,
                     item1 = rbinom(100, 1, 0.5),
                     item2 = rbinom(100, 1, 0.5),
                     item3 = rbinom(100, 1, 0.5))
  resp$item1[1:10] <- NA
  resp$item2[11:20] <- NA
  resp$item3[6:15] <- NA
  longitudinal <- TRUE
  resp <- list(resp, resp)

  waves <- c("_w1", "_w2")

  test <- calculate_number_of_valid_responses(longitudinal, resp, waves)
  expect_equal(test$valid_w1, c(rep(2, 5), rep(1, 10), rep(2, 5), rep(3, 80)))
  expect_equal(test$valid_w2, c(rep(2, 5), rep(1, 10), rep(2, 5), rep(3, 80)))
})

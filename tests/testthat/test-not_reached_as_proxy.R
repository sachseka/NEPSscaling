context("not_reached_as_proxy")

test_that("not-reached as proxy: include_nr = FALSE", {
  
  longitudinal <- FALSE
  data <- data.frame(ID_t = 1:100,
                     res10110_c = c(rep(-94, 10), 11:100),
                     res1012s_c = c(rep(-94, 10), 11:100),
                     res10130_c = c(rep(-94, 10), 11:100),
                     rea906010_sc5s12_c = c(rep(-94, 10), 11:100),
                     rea906020_sc5s12_c = c(rep(-94, 10), 11:100),
                     rea906030_sc5s12_c = c(rep(-94, 10), 11:100))
  SC <- "SC5"
  domain <- "RE"
  wave <- "w1"
  waves <- c("_w1", "_w12")
  
  result <- data.frame(ID_t = 1:100,
                       items_not_reached = c(rep(3, 10), rep(0, 90)))

    test <- not_reached_as_proxy(include_nr = FALSE, longitudinal, data, SC,
                               domain, wave, waves)
  expect_equal(test$nr, NULL)
  expect_equal(test$include_nr, FALSE)
  expect_equal(sum(is.na(test$data)), 60)
})

test_that("not-reached as proxy: include_nr = TRUE, general cross-sec. case", {
  
  include_nr <- TRUE
  longitudinal <- FALSE
  data <- data.frame(ID_t = 1:100,
                     res10110_c = c(rep(-94, 10), 11:100),
                     res1012s_c = c(rep(-94, 10), 11:100),
                     res10130_c = c(rep(-94, 10), 11:100),
                     rea906010_sc5s12_c = c(rep(-94, 10), 11:100),
                     rea906020_sc5s12_c = c(rep(-94, 10), 11:100),
                     rea906030_sc5s12_c = c(rep(-94, 10), 11:100))
  SC <- "SC5"
  domain <- "RE"
  wave <- "w1"
  waves <- c("_w1", "_w12")
  
  result <- data.frame(ID_t = 1:100,
                       items_not_reached = c(rep(3, 10), rep(0, 90)))
  
  test <- not_reached_as_proxy(include_nr, longitudinal, data, SC,
                               domain, wave, waves)
  expect_equal(test$nr, result)
  expect_equal(test$include_nr, TRUE)
  expect_equal(sum(is.na(test$data)), 60)
})


test_that("not-reached as proxy: include_nr = TRUE, general longit. case", {
  
  include_nr <- TRUE
  data <- data.frame(ID_t = 1:100,
                     res10110_c = c(rep(-94, 10), 11:100),
                     res1012s_c = c(rep(-94, 10), 11:100),
                     res10130_c = c(rep(-94, 10), 11:100),
                     rea906010_sc5s12_c = c(rep(-94, 10), 11:100),
                     rea906020_sc5s12_c = c(rep(-94, 10), 11:100),
                     rea906030_sc5s12_c = c(rep(-94, 10), 11:100))
  SC <- "SC5"
  domain <- "RE"
  wave <- "w1"
  waves <- c("_w1", "_w12")
  result <- data.frame(ID_t = 1:100,
                       items_not_reached_w1 = c(rep(3, 10), rep(0, 90)),
                       items_not_reached_w12 = c(rep(3, 10), rep(0, 90)))
  
  test <- not_reached_as_proxy(include_nr, longitudinal = TRUE, data, SC,
                               domain, wave, waves)
  expect_equal(test$nr, result)
  expect_equal(test$include_nr, TRUE)
  expect_equal(sum(is.na(test$data)), 60)
})


test_that("not-reached as proxy: messages if constant number", {
  include_nr <- TRUE
  longitudinal <- FALSE
  SC <- "SC5"
  domain <- "RE"
  wave <- "w1"
  waves <- c("_w1", "_w12")
  result <- data.frame(ID_t = 1:100,
                       items_not_reached_w1 = c(rep(3, 10), rep(0, 90)),
                       items_not_reached_w12 = c(rep(3, 10), rep(0, 90)))
  data <- data.frame(ID_t = 1:100,
                     res10110_c = c(1:100),
                     res1012s_c = c(1:100),
                     res10130_c = c(1:100),
                     rea906010_sc5s12_c = c(rep(-94, 10), 11:100),
                     rea906020_sc5s12_c = c(rep(-94, 10), 11:100),
                     rea906030_sc5s12_c = c(rep(-94, 10), 11:100))
  
  # cross-sectional
  expect_message(not_reached_as_proxy(include_nr, longitudinal, data, SC,
                                      domain, wave, waves),
                 "The number of not-reached missing values is constant. Thus, it is not considered in the background model.")

  # longitudinal
  expect_message(not_reached_as_proxy(include_nr, longitudinal = TRUE, data, SC,
                                      domain, wave, waves),
                 "w1 is constant. It is excluded from the background model.")
})


test_that("not-reached as proxy: exception for SC6/RE (longitudinal)", {
  include_nr <- TRUE
  data <- data.frame(ID_t = 1:100,
                     rea30110_c = c(rep(-94, 10), 11:100),
                     rea3012s_c = c(11:100, rep(-94, 10)),
                     rea30130_c = c(rep(-94, 10), 11:100),
                     rea90101s_c = c(rep(-94, 10), 11:100),
                     rea90102s_c = c(rep(-94, 10), 11:100),
                     rea901030_c = c(rep(-94, 10), 11:100),
                     rea3_sc1u = c(rep(NA, 50), 1:50),
                     rea5_sc1u = c(1:50, rep(NA, 50)))
  wave <- "w3"
  waves <- c("_w3", "_w5", "_w9")
  result <- data.frame(ID_t = 1:100,
                       items_not_reached_w3 = c(rep(NA, 50), rep(0, 40),
                                                rep(1, 10)),
                       items_not_reached_w5 = c(rep(2, 10), rep(0, 40),
                                                rep(NA, 50)),
                       items_not_reached_w9 = c(rep(3, 10), rep(0, 90)))
  
  test <- not_reached_as_proxy(include_nr, longitudinal = TRUE, data,
                               SC = "SC6", domain = "RE", wave, waves)
  expect_equal(test$nr, result)
  expect_equal(test$include_nr, TRUE)
  expect_equal(sum(is.na(test$data)), 60 + 100)
})

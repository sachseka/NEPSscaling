context("select_test_responses_and_test_takers: no test of min_valid; see sc6 long for this!")

test_that("select_test_responses_and_test_takers: longitudinal, general case, e.g. SC5/RE", {
  domain <- "RE"
  wave <- "w9"
  
  min_valid <- 3
  
  data <- data.frame(ID_t = 1:100,
                     res10110_c = 1:100,
                     res1012s_c = 1:100,
                     res10130_c = 1:100,
                     rea906010_sc5s12_c = 1:100,
                     rea906020_sc5s12_c = 1:100,
                     rea906030_sc5s12_c = 1:100)

  result <- list(
    resp = list(
      data.frame(ID_t = 1:100,
                 res10110_c = 1:100,
                 res1012s_c = 1:100,
                 res10130_c = 1:100),
      data.frame(ID_t = 1:100,
                 rea906010_sc5s12_c = 1:100,
                 rea906020_sc5s12_c = 1:100,
                 rea906030_sc5s12_c = 1:100)
    ),
    data = data
  )

  test <- select_test_responses_and_test_takers(longitudinal = TRUE, SC = "SC5",
                                                domain, data, wave, min_valid)
  expect_equal(names(test), c("resp", "data"))
  expect_equal(test, result)
})


test_that("select_test_responses_and_test_takers: cross-sec, general case, e.g. SC6/RE/w9", {
  
  SC <- "SC6"
  domain <- "RE"
  wave <- "w9"
  
  min_valid <- 3
  
  data <- cbind(
    data.frame(ID_t = 1:100,
               dummy = 0),
    as.data.frame(replicate(length(item_labels[[SC]][[domain]][[wave]]), 1:100))
  )
  names(data) <- c("ID_t", "dummy", item_labels[[SC]][[domain]][[wave]])

  result <- list(
    resp = data[, -2],
    data = data
  )
  test <- select_test_responses_and_test_takers(longitudinal = FALSE, SC,
                                                domain, data, wave,
                                                min_valid)
  expect_equal(names(test), c("resp", "data"))
  expect_equal(test, result)
})


test_that("select_test_responses_and_test_takers: cross-sec, SC6/RE/w3+5", {
  SC <- "SC6"
  domain <- "RE"

  min_valid <- 3
  
  wave <- "w3"
  data <- cbind(
    data.frame(ID_t = 1:100,
               wave_w3 = rep(1:0, 50),
               wave_w5 = rep(0:1, 50)),
    as.data.frame(replicate(length(item_labels[[SC]][[domain]][[wave]]), 1:100))
  )
  names(data) <- c("ID_t", "wave_w3", "wave_w5",
                   item_labels[[SC]][[domain]][[wave]])
  result <- list(
    resp = data[-seq(2, 100, 2), c("ID_t",
                                   item_labels[[SC]][[domain]][[wave]])],
    data = data[-seq(2, 100, 2), ]
  )
  test <- select_test_responses_and_test_takers(longitudinal = FALSE, SC,
                                                domain, data, wave = "w3",
                                                min_valid)
  expect_equal(names(test), c("resp", "data"))
  expect_equal(test, result)

  wave <- "w5"
  data <- cbind(
    data.frame(ID_t = 1:100,
               wave_w3 = rep(1:0, 50),
               wave_w5 = rep(0:1, 50)),
    as.data.frame(replicate(length(item_labels[[SC]][[domain]][[wave]]), 1:100))
  )
  names(data) <- c("ID_t", "wave_w3", "wave_w5",
                   item_labels[[SC]][[domain]][[wave]])
  result <- list(
    resp = data[-seq(1, 100, 2), c("ID_t",
                                   item_labels[[SC]][[domain]][[wave]])],
    data = data[-seq(1, 100, 2), ]
  )
  test <- select_test_responses_and_test_takers(longitudinal = FALSE, SC,
                                                domain, data, wave = "w5",
                                                min_valid)
  expect_equal(names(test), c("resp", "data"))
  expect_equal(test, result)
})


test_that("select_test_responses_and_test_takers: cross-sec, SC4/ST", {
  min_valid <- 3
  
  data <- cbind(
    data.frame(ID_t = 1:100, dummy = 0),
    as.data.frame(replicate(length(item_labels[["SC4"]][["ST"]][["w7"]]), 1:100))
  )
  names(data) <- c("ID_t", "dummy", item_labels[["SC4"]][["ST"]][["w7"]])
  result <- list(
    resp = data.frame(ID_t = 1:100,
                      stg12nhs_c = 1:100 * 5,
                      stg12egs_c = 1:100 * 7,
                      stg12mts_c = 1:100 * 6,
                      stg12cws_c = 1:100 * 7,
                      stg12pds_c = 1:100 * 7),
    data = data
  )
  test <- select_test_responses_and_test_takers(longitudinal = FALSE,
                                                SC = "SC4", domain = "ST", data,
                                                wave = "w7", min_valid)
  expect_equal(names(test), c("resp", "data"))
  expect_equal(test, result)
})

#TODO: English imputation is done in this step, but tested in a separate file
#TODO: SC6/RE longitudinal tested in separate file

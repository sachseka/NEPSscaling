context("get_test_rotation_info_cross_sec")

rotation <- TRUE
data <- data.frame(ID_t = 1:100,
                   tx80211_w1 = 1:100,
                   tx80211_w7 = 1:100,
                   tx80211_w6 = 1:100,
                   tx80211_w9 = 1:100,
                   tx80211_w3 = c(rep(c(123, 125), 25), rep(c(122, 124), 25)))
SC <- "SC6"
wave <- "w3"
domain <- "RE"
resp <- data.frame(ID_t = 1:100, item1 = 1:100)
bgdata <- data.frame(ID_t = 1:100, var1 = 1:100)
ID_t <- data.frame(ID_t = 1:100)

test_that("test rotation info cross-sec: exceptions without rotation", {
  result <- data.frame(ID_t = 1:100, position = rep(1, 100))
  test <- get_test_rotation_info_cross_sec(rotation, data, SC = "SC1",
                                           wave = "w1", domain, resp, bgdata,
                                           ID_t)
  expect_equal(test$position, result)

  test <- get_test_rotation_info_cross_sec(rotation, data, SC = "SC1",
                                           wave = "w7", domain, resp, bgdata,
                                           ID_t)
  expect_equal(test$position, result)
  expect_equal(test$rotation, FALSE)

  test <- get_test_rotation_info_cross_sec(rotation, data, SC = "SC2",
                                           wave = "w6", domain, resp, bgdata,
                                           ID_t)
  expect_equal(test$position, result)
  expect_equal(test$rotation, FALSE)

  test <- get_test_rotation_info_cross_sec(rotation, data, SC = "SC2",
                                           wave = "w9", domain, resp, bgdata,
                                           ID_t)
  expect_equal(test$position, result)
  expect_equal(test$rotation, FALSE)

  test <- get_test_rotation_info_cross_sec(rotation, data, SC = "SC4",
                                           wave = "w3", domain, resp, bgdata,
                                           ID_t)
  expect_equal(test$position, result)
  expect_equal(test$rotation, FALSE)

  test <- get_test_rotation_info_cross_sec(rotation, data, SC = "SC5",
                                           wave = "w7", domain, resp, bgdata,
                                           ID_t)
  expect_equal(test$position, result)
  expect_equal(test$rotation, FALSE)
})

test_that("test rotation info cross-sec: general case", {
  result <- data.frame(position = c(rep(1, 50), rep(2, 50)))
  test <- get_test_rotation_info_cross_sec(rotation, data, SC, wave, domain,
                                           resp, bgdata, ID_t)
  expect_equal(test$position, result)
  expect_equal(test$rotation, TRUE)
})

data[1:10, "tx80211_w3"] <- NA
test_that("test rotation info cross-sec: discarding of NA", {
  result <- data.frame(position = c(rep(1, 50), rep(2, 50)))
  result <- result[-(1:10), , drop = FALSE]
  test <- get_test_rotation_info_cross_sec(rotation, data, SC, wave, domain,
                                           resp, bgdata, ID_t)
  expect_equal(test$position, result)
  expect_equal(nrow(test$data), 90)
  expect_equal(nrow(test$resp), 90)
  expect_equal(nrow(test$ID_t), 90)
  expect_equal(test$rotation, TRUE)
})

rm(bgdata, data, ID_t, resp, domain, rotation, SC, wave)

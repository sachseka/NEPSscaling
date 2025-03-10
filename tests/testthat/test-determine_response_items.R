context("determine_response_items")

test_that("determine_response_items: longitudinal", {
  data <- as.data.frame(
    matrix(data = 0, nrow = 10, ncol = 4)
  )
  names(data) <- c("sck10420_c", "scg10820_c", "scg30109_c", "scg9611s_c")
  result <- list(w1 = c(T, F, F, F), w3 = c(F, T, F, F), w5 = c(F, F, T, F))
  expect_equal(
    determine_response_items(SC = "SC2", domain = "SC", data = data, wave = NULL, longitudinal = T),
    result
  )

  names(data) <- c("reg50110_sc2g4_c", "reg5012s_sc2g4_c", "reg70110_sc2g7_c", "reg70120_sc2g7_c")
  result <- list(w6 = c(T, T, F, F), w9 = c(F, F, T, T))
  expect_equal(
    determine_response_items(SC = "SC2", domain = "RE", data = data, wave = NULL, longitudinal = T),
    result
  )
})



test_that("determine_response_items: cross-sectional", {
  data <- as.data.frame(
    matrix(data = 0, nrow = 10, ncol = 4)
  )
  names(data) <- c("sck10420_c", "scg10820_c", "scg30109_c", "scg9611s_sc2g7_c")
  result <- list(w9 = c(F, F, F, T))
  expect_equal(
    determine_response_items(SC = "SC2", domain = "SC", data = data, wave = "w9", longitudinal = F),
    result
  )

  names(data) <- c("reg50110_sc2g4_c", "reg5012s_sc2g4_c", "reg70110_sc2g7_c", "reg70120_sc2g7_c")
  result <- list(w6 = c(T, T, F, F))
  expect_equal(
    determine_response_items(SC = "SC2", domain = "RE", data = data, wave = "w6", longitudinal = F),
    result
  )
})


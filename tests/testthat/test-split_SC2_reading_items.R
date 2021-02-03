context("split_SC2_reading_items")

test_that("split_SC2_reading_items: longitudinal", {

  testletSetting <- data.frame(ID_t = 1:100,
                               easy = c(rep(TRUE, 50), rep(FALSE, 50)))
  resp <- list(
    list(),
    data.frame(ID_t = 1:100,
               reg7024s_sc2g7_c = 1:100,
               reg7033s_sc2g7_c = 1:100,
               reg7045s_sc2g7_c = 1:100)
  )
  wave <- "w9"
  result <- list(
    list(),
    result <- data.frame(ID_t = 1:100,
                         reg7024s_sc2g7_c = c(1:50, rep(NA, 50)),
                         reg7024s_sc2g7_c_d = c(rep(NA, 50), 51:100),
                         reg7033s_sc2g7_c = c(1:50, rep(NA, 50)),
                         reg7033s_sc2g7_c_d = c(rep(NA, 50), 51:100),
                         reg7045s_sc2g7_c = c(1:50, rep(NA, 50)),
                         reg7045s_sc2g7_c_d = c(rep(NA, 50), 51:100))
  )
  test <- split_SC2_reading_items(testletSetting, resp, longitudinal = TRUE,
                                  wave)
  expect_equal(test, result)
})


test_that("split_SC2_reading_items: cross-sectional", {

  testletSetting <- data.frame(ID_t = 1:100,
                               easy = c(rep(TRUE, 50), rep(FALSE, 50)))
  resp <- data.frame(ID_t = 1:100,
                     reg7024s_sc2g7_c = 1:100,
                     reg7033s_sc2g7_c = 1:100,
                     reg7045s_sc2g7_c = 1:100)
  wave <- "w9"
  result <- data.frame(ID_t = 1:100,
                       reg7024s_sc2g7_c = c(1:50, rep(NA, 50)),
                       reg7024s_sc2g7_c_d = c(rep(NA, 50), 51:100),
                       reg7033s_sc2g7_c = c(1:50, rep(NA, 50)),
                       reg7033s_sc2g7_c_d = c(rep(NA, 50), 51:100),
                       reg7045s_sc2g7_c = c(1:50, rep(NA, 50)),
                       reg7045s_sc2g7_c_d = c(rep(NA, 50), 51:100))
  test <- split_SC2_reading_items(testletSetting, resp, longitudinal = FALSE,
                                  wave)
  expect_equal(test, result)
})


test_that("split_SC2_reading_items: cross-sectional - wrong wave", {

  testletSetting <- data.frame(ID_t = 1:100,
                               easy = c(rep(TRUE, 50), rep(FALSE, 50)))
  resp <- data.frame(ID_t = 1:100,
                     reg7024s_sc2g7_c = 1:100,
                     reg7033s_sc2g7_c = 1:100,
                     reg7045s_sc2g7_c = 1:100)
  result <- resp
  test <- split_SC2_reading_items(testletSetting, resp, longitudinal = FALSE,
                               wave = "w1")
  expect_equal(test, result)
})

context("split_SC4_math_items")

test_that("split_SC4_math_items: longitudinal", {
  
  testletSetting <- data.frame(ID_t = 1:100,
                               atHome = c(rep(TRUE, 50), rep(FALSE, 50)),
                               difficultTestlet = c(rep(TRUE, 50),
                                                    rep(FALSE, 50)))
  resp <- list(
    list(),
    data.frame(ID_t = 1:100,
               mag9d201_sc4g12_c = 1:100,
               mag9r051_sc4g12_c = 1:100)
  )
  wave <- "w7"
  
  result <- list(
    list(),
    result <- data.frame(ID_t = 1:100,
                         mag9d201_sc4g12_c_g = c(rep(NA, 50), 51:100),
                         mag9d201_sc4g12_c_i = c(1:50, rep(NA, 50)),
                         mag9r051_sc4g12_c_d = c(1:50, rep(NA, 50)),
                         mag9r051_sc4g12_c_e = c(rep(NA, 50), 51:100))
  )
  test <- split_SC4_math_items(testletSetting, resp, longitudinal = TRUE, wave)
  expect_equal(test, result)
})


test_that("split_SC4_math_items: cross-sectional", {
  
  testletSetting <- data.frame(ID_t = 1:100,
                               atHome = c(rep(TRUE, 50), rep(FALSE, 50)),
                               difficultTestlet = c(rep(TRUE, 50),
                                                    rep(FALSE, 50)))
  resp <- data.frame(ID_t = 1:100,
                     mag9d201_sc4g12_c = 1:100,
                     mag9r051_sc4g12_c = 1:100)
  wave <- "w7"
  
  result <- data.frame(ID_t = 1:100,
                       mag9d201_sc4g12_c_g = c(rep(NA, 50), 51:100),
                       mag9d201_sc4g12_c_i = c(1:50, rep(NA, 50)),
                       mag9r051_sc4g12_c_d = c(1:50, rep(NA, 50)),
                       mag9r051_sc4g12_c_e = c(rep(NA, 50), 51:100))
  test <- split_SC4_math_items(testletSetting, resp, longitudinal = FALSE, wave)
  expect_equal(test, result)
})


test_that("split_SC4_math_items: cross-sectional - wrong wave", {
  
  testletSetting <- data.frame(ID_t = 1:100,
                               atHome = c(rep(TRUE, 50), rep(FALSE, 50)),
                               difficultTestlet = c(rep(TRUE, 50),
                                                    rep(FALSE, 50)))
  resp <- data.frame(ID_t = 1:100,
                     mag9d201_sc4g12_c = 1:100,
                     mag9r051_sc4g12_c = 1:100)
  result <- resp
  test <- split_SC4_math_items(testletSetting, resp, longitudinal = FALSE,
                               wave = "w1")
  expect_equal(test, result)
})

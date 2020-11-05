context("get_rotation_change_info_longitudinal")

test_that("rotation change info SC2", {
  data <- data.frame(ID_t = 1:100,
                     tx80211_w4 = c(rep(391, 50), rep(391, 50)))
  result <- data.frame(ID_t = 1:100, position = rep(1, 100))
  test <- get_rotation_change_info_longitudinal("SC2", "MA", data)
  expect_equal(test, result)

  data <- data.frame(ID_t = 1:160,
                     tx80211_w3 = c(rep(c(252:259, 270:277), 5),
                                    rep(c(252:259, 270:277), 5)))
  result <- data.frame(ID_t = 1:160, position = rep(1, 160))
  testlet_position[["SC2"]][["VO"]][["w3"]]
  test <- get_rotation_change_info_longitudinal("SC2", "VO", data)
  expect_equal(test, result)
})

test_that("rotation change info SC3", {
  data <- data.frame(ID_t = 1:100,
                     tx80211_w6 = c(rep(374:375, 25), rep(374:375, 25)))
  result <- data.frame(ID_t = 1:100, position = rep(1, 100))
  test <- get_rotation_change_info_longitudinal("SC3", "RE", data)
  expect_equal(test, result)
})

test_that("rotation change info SC4", {
  data <- data.frame(ID_t = 1:144,
                     tx80211_w7 = c(
                       rep(c(289:294), 12),
                       rep(c(284, 285, 287, 288, 296:303), 6)))
  result <- data.frame(ID_t = 1:144, position = c(rep(1, 72), rep(2, 72)))
  test <- get_rotation_change_info_longitudinal("SC4", "MA", data)
  expect_equal(test, result)

  data <- data.frame(ID_t = 1:100,
                     tx80211_w7 = c(
                       rep(c(283, 284, 285, 286, 287, 288, 300, 301, 302, 303), 5),
                       rep(c(281, 282, 289, 290, 291, 292, 296, 297, 298, 299), 5)))
  result <- data.frame(ID_t = 1:100, position = c(rep(1, 50), rep(2, 50)))
  test <- get_rotation_change_info_longitudinal("SC4", "RE", data)
  expect_equal(test, result)
})

test_that("rotation change info: no change", {
  test <- get_rotation_change_info_longitudinal("SC5", "MA", NULL)
  expect_equal(test, NULL)
})

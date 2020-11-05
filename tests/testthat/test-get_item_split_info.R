context("get_item_split_info")

data <- data.frame(
  ID_t = 1:78,
  tx80211_w7 = c(1:30, rep(NA, 9), rep(c(285, 288, 291:293, 296:303), 3)),
  tx80220_w9 = c(rep(c(751, 753, 757), 13), 1:30, rep(NA, 9))
)

test_that("correct classification of item split info", {
  test <- get_item_split_info("SC1", "MA", data)
  expect_equal(test, NULL)

  test <- get_item_split_info("SC4", "MA", data)
  expect_equal(sum(test$difficultTestlet, na.rm = TRUE), 39)
  expect_equal(sum(test$atHome, na.rm = TRUE), 15)
  expect_equal(sum(is.na(test$difficultTestlet)), 9)
  expect_equal(sum(is.na(test$atHome)), 9)

  test <- get_item_split_info("SC2", "RE", data)
  expect_equal(sum(test$easy, na.rm = TRUE), 39)
  expect_equal(sum(is.na(test$easy)), 9)
})

rm(data)

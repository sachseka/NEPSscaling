context("create_loading_matrix_q_longitudinal")

# SC1 MA - 2 waves, 20 and 25 items
# SC2 MA - 2 waves, 26 and 21 items
# SC2 RE - 2 waves, 31 and 45 items (split!)
# SC4 MA - 3 waves, 22, 31 and 52 items (split!)
# SC4 RE - 3 waves, 31, 41 and 36 items

test_that("correct dimensions of Q matrix list", {
  # length of list of Qs
  items <- lapply(xsi.fixed$long[["MA"]][["SC1"]], rownames)
  expect_equal(length(create_loading_matrix_q_longitudinal("SC1", "MA", items)),
               2)
  items <- lapply(xsi.fixed$long[["RE"]][["SC2"]], rownames)
  expect_equal(length(create_loading_matrix_q_longitudinal("SC2", "RE", items)),
               2)
  items <- lapply(xsi.fixed$long[["MA"]][["SC4"]], rownames)
  expect_equal(length(create_loading_matrix_q_longitudinal("SC4", "MA", items)),
               3)
  # nrow of specific Qs
  items <- lapply(xsi.fixed$long[["MA"]][["SC1"]], rownames)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC1", "MA", items)[[1]]),
               20)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC1", "MA", items)[[2]]),
               25)
  items <- lapply(xsi.fixed$long[["MA"]][["SC2"]], rownames)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC2", "MA", items)[[1]]),
               26)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC2", "MA", items)[[2]]),
               21)
  items <- lapply(xsi.fixed$long[["RE"]][["SC2"]], rownames)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC2", "RE", items)[[1]]),
               31)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC2", "RE", items)[[2]]),
               45)
  items <- lapply(xsi.fixed$long[["MA"]][["SC4"]], rownames)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC4", "MA", items)[[1]]),
               22)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC4", "MA", items)[[2]]),
               31)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC4", "MA", items)[[3]]),
               52)
  items <- lapply(xsi.fixed$long[["RE"]][["SC4"]], rownames)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC4", "RE", items)[[1]]),
               31)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC4", "RE", items)[[2]]),
               41)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC4", "RE", items)[[3]]),
               36)
})

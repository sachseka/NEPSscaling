context("create_loading_matrix_q_longitudinal")

# SC1 MA - 2 waves, 20 and 25 items
# SC2 MA - 2 waves, 26 and 21 items
# SC2 RE - 2 waves, 31 and 45 items (split!)
# SC4 MA - 3 waves, 22, 31 and 52 items (split!)
# SC4 RE - 3 waves, 31, 41 and 36 items

test_that("correct dimensions of Q matrix list", {
  # length of list of Qs
  expect_equal(length(create_loading_matrix_q_longitudinal("SC1", "MA")),
               2)
  expect_equal(length(create_loading_matrix_q_longitudinal("SC2", "RE")),
               2)
  expect_equal(length(create_loading_matrix_q_longitudinal("SC4", "MA")),
               3)
  # nrow of specific Qs
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC1", "MA")[[1]]),
               20)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC1", "MA")[[2]]),
               25)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC2", "MA")[[1]]),
               26)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC2", "MA")[[2]]),
               21)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC2", "RE")[[1]]),
               31)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC2", "RE")[[2]]),
               45)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC4", "MA")[[1]]),
               22)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC4", "MA")[[2]]),
               31)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC4", "MA")[[3]]),
               52)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC4", "RE")[[1]]),
               31)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC4", "RE")[[2]]),
               41)
  expect_equal(nrow(create_loading_matrix_q_longitudinal("SC4", "RE")[[3]]),
               36)
})

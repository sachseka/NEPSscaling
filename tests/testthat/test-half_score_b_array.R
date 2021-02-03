context("half_score_b_array")

test_that("half_score_b_array: exception", {
  B <- result <- array(data = 1, dim = c(10, 3, 3))
  items <- paste0("item", 1:10)
  ind <- list(paste0("item", 1:5), paste0("item", 6:10))

  result[1:5, , ] <- 2/3
  result[6:10, , ] <- 0.5
  expect_equal(half_score_b_array(SC = "SC4", domain = "SC", waves = "_w1",
                                  B, items, ind),
               result)
})

test_that("half_score_b_array: regular case", {
  B <- result <- array(data = 1, dim = c(10, 3, 3))
  items <- paste0("item", 1:10)
  ind <- paste0("item", 1:5)

  result[1:5, , ] <- 0.5
  expect_equal(half_score_b_array(SC = "SC4", domain = "SC", waves = "_w2",
                                  B, items, ind),
               result)
  expect_equal(half_score_b_array(SC = "SC3", domain = "SC", waves = "_w1",
                                  B, items, ind),
               result)
  expect_equal(half_score_b_array(SC = "SC4", domain = "RE", waves = "_w1",
                                  B, items, ind),
               result)
})

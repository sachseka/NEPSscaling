context("half_score_q_matrix")

test_that("half_score_q_matrix: exception", {
  Q <- list(matrix(1, ncol = 1, nrow = 10))
  items <- list(paste0("item", 1:10))
  ind <- list(paste0("item", 1:5), paste0("item", 6:10))
  result <- list(matrix(c(rep(2/3, 5), rep(0.5, 5)), ncol = 1, nrow = 10))
  expect_equal(half_score_q_matrix(SC = "SC4", domain = "SC", i = 1,
                                   Q, items, ind),
               result)
})

test_that("half_score_q_matrix: regular case", {
  Q <- list(matrix(1, ncol = 1, nrow = 10),
            matrix(1, ncol = 1, nrow = 10))
  items <- list(paste0("item", 1:10), paste0("item", 1:10))
  ind <- paste0("item", 1:5)
  result <- list(matrix(1, ncol = 1, nrow = 10),
                 matrix(c(rep(0.5, 5), rep(1, 5)), ncol = 1, nrow = 10))
  expect_equal(half_score_q_matrix(SC = "SC4", domain = "SC", i = 2,
                                   Q, items, ind),
               result)

  result <- list(matrix(c(rep(0.5, 5), rep(1, 5)), ncol = 1, nrow = 10),
                 matrix(1, ncol = 1, nrow = 10))
  expect_equal(half_score_q_matrix(SC = "SC5", domain = "SC", i = 1,
                                   Q, items, ind),
               result)
})

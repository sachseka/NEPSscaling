context("is_PCM")

test_that("is_PCM: longitudinal", {
  resp <- replicate(3, data.frame(i1 = sample(c(0, 1), 20, replace = TRUE),
                                  i2 = sample(c(0, 1), 20, replace = TRUE),
                                  i3 = sample(c(0, 1), 20, replace = TRUE),
                                  i4 = sample(c(0, 1), 20, replace = TRUE),
                                  i5 = sample(c(0, 1), 20, replace = TRUE)),
                    simplify = FALSE)
  expect_equal(is_PCM(longitudinal = T, resp = resp), list(FALSE, FALSE, FALSE))

  resp[[3]]$i3 <- sample(0:4, 20, replace = TRUE)
  expect_equal(is_PCM(longitudinal = T, resp = resp), list(FALSE, FALSE, TRUE))
})

test_that("is_PCM: cross-sectional", {
  resp <- data.frame(i1 = sample(c(0, 1), 20, replace = TRUE),
                     i2 = sample(c(0, 1), 20, replace = TRUE),
                     i3 = sample(c(0, 1), 20, replace = TRUE),
                     i4 = sample(c(0, 1), 20, replace = TRUE),
                     i5 = sample(c(0, 1), 20, replace = TRUE))
  expect_false(is_PCM(longitudinal = F, resp = resp))

  resp$i3 <- sample(0:4, 20, replace = TRUE)
  expect_true(is_PCM(longitudinal = F, resp = resp))
})

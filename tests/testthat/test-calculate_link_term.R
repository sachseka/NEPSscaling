context("calculate_link_terms")

# wle = NULL not tested, because it is simply omitted

test_that("calculate_link_terms: SC6/RE", {
  MEAN <- list(eap = c(1, 1, 2, 3), wle = c(1, 1, 2, 3),
               pv = replicate(5, c(1, 1, 2, 3), simplify = FALSE))

  # because link_constant is not checked here, instead of the exact values,
  # the structure of the object is tested
  result <- list(eap = c(0, 1), wle = c(0, 1), pv = replicate(5, 0:1, simplify = F))
  test <- calculate_link_terms(MEAN, SC = "SC6", domain = "RE", waves_ = NULL,
                               w = NULL)
  expect_equal(names(test), names(result))
  expect_equal(length(test$eap), length(result$eap))
  expect_equal(length(test$wle), length(result$wle))
  expect_equal(length(test$pv), length(result$pv))
  expect_equal(length(test$pv[[1]]), length(result$pv[[1]]))
})

test_that("calculate_link_terms: regular case", {
  MEAN <- list(eap = c(1, 2), wle = c(1, 2),
               pv = replicate(5, c(1, 2), simplify = FALSE))

  # because link_constant is not checked here, instead of the exact values,
  # the structure of the object is tested
  # w starts to count at 2 (no linking for 1st assessment!)
  expect_error(calculate_link_terms(MEAN, SC = "SC6", domain = "MA",
                                    waves_ = c("w3", "w9"),
                                    w = 1))

  result <- list(eap = 0, wle = 0, pv = replicate(5, 0, simplify = F))
  test <- calculate_link_terms(MEAN, SC = "SC6", domain = "MA",
                               waves_ = c("w3", "w9"),
                               w = 2)
  expect_equal(names(test), names(result))
  expect_equal(length(test$eap), length(result$eap))
  expect_equal(length(test$wle), length(result$wle))
  expect_equal(length(test$pv), length(result$pv))
  expect_equal(length(test$pv[[1]]), length(result$pv[[1]]))
})

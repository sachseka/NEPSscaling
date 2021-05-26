context("was_assessed_in_school")

test_that("was_assessed_in_school: longitudinal", {
  expect_false(was_assessed_in_school(longitudinal = TRUE, SC = "SC1", wave = NULL))
  expect_false(was_assessed_in_school(longitudinal = TRUE, SC = "SC5", wave = NULL))
  expect_false(was_assessed_in_school(longitudinal = TRUE, SC = "SC6", wave = NULL))

  expect_true(was_assessed_in_school(longitudinal = TRUE, SC = "SC2", wave = NULL))
  expect_true(was_assessed_in_school(longitudinal = TRUE, SC = "SC3", wave = NULL))
  expect_true(was_assessed_in_school(longitudinal = TRUE, SC = "SC4", wave = NULL))
})

test_that("was_assessed_in_school: cross-sectional", {
  expect_false(was_assessed_in_school(longitudinal = F, SC = "SC1", wave = "w3"))
  expect_false(was_assessed_in_school(longitudinal = F, SC = "SC5", wave = "w3"))
  expect_false(was_assessed_in_school(longitudinal = F, SC = "SC6", wave = "w3"))

  expect_false(was_assessed_in_school(longitudinal = F, SC = "SC2", wave = "w1"))
  expect_false(was_assessed_in_school(longitudinal = F, SC = "SC3", wave = "w10"))
  expect_false(was_assessed_in_school(longitudinal = F, SC = "SC4", wave = "w4"))

  expect_true(was_assessed_in_school(longitudinal = F, SC = "SC2",
                                     wave = sample(c("w3", "w4", "w5", "w6", "w9"), 1, replace = F)))
  expect_true(was_assessed_in_school(longitudinal = F, SC = "SC3",
                                     wave = sample(paste0("w", c(1:3, 5:9)), 1, replace = F)))
  expect_true(was_assessed_in_school(longitudinal = F, SC = "SC4",
                                     wave = sample(c("w1", "w2", "w3", "w5", "w7"), 1, replace = F)))
})

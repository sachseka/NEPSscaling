context("adjust_rotation_indicator")

test_that("adjust_rotation_indicator", {
  expect_false(
    adjust_rotation_indicator(rotation = FALSE,
                              position = data.frame(position = 1:10))
  )
  expect_false(
    adjust_rotation_indicator(rotation = FALSE,
                              position = data.frame(position = rep(1, 10)))
  )
  expect_false(
    adjust_rotation_indicator(rotation = TRUE,
                              position = data.frame(position = rep(1, 10)))
  )
  expect_true(
    adjust_rotation_indicator(rotation = TRUE,
                              position = data.frame(position = 1:10))
  )
})

context("remove_constant_not_reached")

test_that("remove_constant_not_reached: no removed", {
  nr <- data.frame(
    ID_t = 1:50,
    nr1 = 1:50,
    nr2 = 1:50
  )
  sel <- list(1:10)
  expect_equivalent(remove_constant_not_reached(nr, sel), nr)
})

test_that("remove_constant_not_reached: 1 removed", {
  nr <- result <- data.frame(
    ID_t = 1:50,
    nr1 = rep(1, 50),
    nr2 = 1:50
  )
  sel <- list(1:10, 1:10)
  result$nr1 <- NULL
  expect_equivalent(remove_constant_not_reached(nr, sel), result)
})

test_that("remove_constant_not_reached: all removed", {
  nr <- data.frame(
    ID_t = 1:50,
    nr1 = rep(1, 50),
    nr2 = rep(1, 50)
  )
  sel <- list(1:10, 1:10)
  expect_equivalent(remove_constant_not_reached(nr, sel), NULL)
})

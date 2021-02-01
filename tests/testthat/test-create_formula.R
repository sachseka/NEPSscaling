context("create_formula")

test_that("create_formula", {
  data <- data.frame(ID_t = 1:10, var1 = 1:10, var2 = 1:10)
  expect_equal(create_formula(data), as.formula("~ var1 + var2"))
})

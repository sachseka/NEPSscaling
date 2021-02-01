context("determine_used_imputations")

test_that("determine_used_imputations", {
  datalist <- replicate(5, list(), simplify = FALSE)
  names(datalist) <- c("imp1pv1", "imp1pv2", "imp3pv1", "imp4pv1", "imp5pv1")
  expect_equal(determine_used_imputations(datalist),
               list(c(1, 3, 4, 5), c(1, 2, 5, 6, 7, 8, 9, 10)))
})

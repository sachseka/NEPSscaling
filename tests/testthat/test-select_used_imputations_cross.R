context("select_used_imputations_cross")

test_that("select_used_imputations_cross", {
  keep <- list(c(1, 2, 3), c(1, 2, 3, 4, 5, 6))
  EAP.rel <- treeplot <- variable_importance <-
    replicate(5, list(), simplify = FALSE)
  info_crit <- matrix(1, ncol = 5, nrow = 5)
  regr.coeff <- matrix(1, ncol = 10, nrow = 5)

  result <- list(
    regr.coeff = matrix(1, ncol = 6, nrow = 5),
    EAP.rel = EAP.rel[1:3],
    info_crit = matrix(1, ncol = 3, nrow = 5),
    treeplot = treeplot[1:3],
    variable_importance = variable_importance[1:3]
  )
  names(result$EAP.rel) <- names(result$info_crit) <- names(result$treeplot) <-
    names(result$variable_importance) <- paste0("imp", 1:3)
  expect_equal(select_used_imputations_cross(EAP.rel, regr.coeff, info_crit,
                                             treeplot, variable_importance,
                                             keep),
               result)
})

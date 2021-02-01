context("select_used_imputations_long")

test_that("select_used_imputations_long", {
  keep <- 1:3
  EAP.rel <- treeplot <- variable_importance <- regr.coeff <- info_crit <-
    replicate(5, list(), simplify = FALSE)

  result <- list(
    regr.coeff = regr.coeff[1:3],
    EAP.rel = EAP.rel[1:3],
    info_crit = info_crit[1:3],
    treeplot = treeplot[1:3],
    variable_importance = variable_importance[1:3]
  )
  names(result$EAP.rel) <- names(result$info_crit) <- names(result$treeplot) <-
    names(result$variable_importance) <- names(result$regr.coeff) <-
    paste0("imp", 1:3)
  expect_equal(select_used_imputations_long(EAP.rel, regr.coeff, info_crit,
                                            treeplot, variable_importance,
                                            keep),
               result)
})

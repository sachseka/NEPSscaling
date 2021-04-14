context("select_used_imputations_cross")

test_that("select_used_imputations_cross", {
  keep <- list(c(1, 2, 3), c("imp1", "imp2", "imp3"))
  EAP.rel <- treeplot <- variable_importance <- variance <-
    replicate(5, list(), simplify = FALSE)
  info_crit <- matrix(1, ncol = 5, nrow = 5)
  eap <- replicate(5, data.frame(ID_t = 1:10, eap = 0, se = 1), simplify = FALSE)
  regr.coeff <- data.frame(Variable = letters[1:5],
                           matrix(1, ncol = 10, nrow = 5))
  names(regr.coeff)[-1] <- paste0("imp", rep(1:5, each = 2), rep(c("_coeff", "_se"), 5))

  result <- list(
    regr.coeff = regr.coeff[, 1:7],
    EAP.rel = EAP.rel[1:3],
    info_crit = matrix(1, ncol = 3, nrow = 5),
    treeplot = treeplot[1:3],
    variable_importance = variable_importance[1:3],
    variance = variance[1:3],
    eap = eap[1:3]
  )
  names(result$EAP.rel) <- names(result$info_crit) <- names(result$treeplot) <-
    names(result$variable_importance) <- names(result$variance) <-
    names(result$eap) <- paste0("imp", 1:3)
  expect_equal(select_used_imputations_cross(EAP.rel, regr.coeff, info_crit,
                                             treeplot, variable_importance,
                                             variance, eap, keep),
               result)
})

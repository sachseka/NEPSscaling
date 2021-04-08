context("gather_additional_parameters_cross")

test_that("gather_additional_parameters_cross", {
  bgdata <- data.frame(ID_t = 1:2000)
  data(data.sim.rasch, package = "TAM")
  mod <- TAM::tam.mml(resp=data.sim.rasch, verbose = FALSE)
  eap <- list(data.frame(ID_t = 1:2000), data.frame(ID_t = 1:2000))
  EAP.rel <- regr.coeff <- info_crit <- variance <- NULL

  regr_result <- data.frame(Variable = "Intercept")
  regr_result <- cbind(regr_result, TAM::tam.se(mod)$beta)
  names(regr_result)[-1] <- c("imp1_coeff", "imp1_se")
  result <- list(eap = list(data.frame(ID_t = 1:2000, eap = mod$person$EAP,
                                       se = mod$person$SD.EAP),
                            data.frame(ID_t = 1:2000)),
                 EAP.rel = mod$EAP.rel,
                 regr.coeff = regr_result,
                 info_crit = matrix(c(AIC(mod), BIC(mod)), nrow = 2, ncol = 1),
                 variance = mod$variance[1])
  rownames(result$info_crit) <- c("AIC", "BIC")
  colnames(result$info_crit) <- "imp1"
  expect_equivalent(gather_additional_parameters_cross(eap, mod, EAP.rel,
                                                       regr.coeff, info_crit,
                                                       i = 1, bgdata = NULL, frmY = NULL,
                                                       variance),
                    result)

  regr_result <- cbind(regr_result, TAM::tam.se(mod)$beta)
  names(regr_result)[-(1:3)] <- c("imp2_coeff", "imp2_se")
  result2 <- list(eap = list(data.frame(ID_t = 1:2000, eap = mod$person$EAP,
                                        se = mod$person$SD.EAP),
                             data.frame(ID_t = 1:2000, eap = mod$person$EAP,
                                        se = mod$person$SD.EAP)),
                  EAP.rel = c(mod$EAP.rel, mod$EAP.rel),
                  regr.coeff = regr_result,
                  info_crit = matrix(c(AIC(mod), BIC(mod)), nrow = 2, ncol = 2),
                  variance = c(mod$variance[1], mod$variance[1]))
  rownames(result2$info_crit) <- c("AIC", "BIC")
  colnames(result2$info_crit) <- c("imp1", "imp2")
  expect_equivalent(gather_additional_parameters_cross(result$eap, mod,
                                                       result$EAP.rel,
                                                       result$regr.coeff,
                                                       result$info_crit,
                                                       i = 2, bgdata = NULL, frmY = NULL,
                                                       result$variance),
                    result2)
})

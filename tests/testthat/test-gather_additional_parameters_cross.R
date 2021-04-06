context("gather_additional_parameters_cross")

test_that("gather_additional_parameters_cross", {
  bgdata <- data.frame(ID_t = 1:2000)
  data(data.sim.rasch, package = "TAM")
  mod <- TAM::tam.mml(resp=data.sim.rasch, verbose = FALSE)
  eap <- list(data.frame(ID_t = 1:2000), data.frame(ID_t = 1:2000))
  EAP.rel <- regr.coeff <- info_crit <- NULL

  result <- list(eap = list(data.frame(ID_t = 1:2000, eap = mod$person$EAP,
                                       se = mod$person$SD.EAP),
                            data.frame(ID_t = 1:2000)),
                 EAP.rel = mod$EAP.rel,
                 regr.coeff = TAM::tam.se(mod)$beta,
                 info_crit = matrix(c(AIC(mod), BIC(mod)), nrow = 2, ncol = 1))
  rownames(result$regr.coeff) <- "Intercept"
  colnames(result$regr.coeff) <- c("imp1_coeff", "imp1_se")
  rownames(result$info_crit) <- c("AIC", "BIC")
  colnames(result$info_crit) <- "imp1"
  expect_equivalent(gather_additional_parameters_cross(eap, mod, EAP.rel,
                                                       regr.coeff, info_crit,
                                                       i = 1, bgdata),
                    result)

  result2 <- list(eap = list(data.frame(ID_t = 1:2000, eap = mod$person$EAP,
                                        se = mod$person$SD.EAP),
                             data.frame(ID_t = 1:2000, eap = mod$person$EAP,
                                        se = mod$person$SD.EAP)),
                  EAP.rel = c(mod$EAP.rel, mod$EAP.rel),
                  regr.coeff = cbind(TAM::tam.se(mod)$beta,
                                          TAM::tam.se(mod)$beta),
                  info_crit = matrix(c(AIC(mod), BIC(mod)), nrow = 2, ncol = 2))
  rownames(result2$regr.coeff) <- "Intercept"
  colnames(result2$regr.coeff) <- c("imp1_coeff", "imp1_se", "imp2_coeff", "imp2_se")
  rownames(result2$info_crit) <- c("AIC", "BIC")
  colnames(result2$info_crit) <- c("imp1", "imp2")
  expect_equivalent(gather_additional_parameters_cross(result$eap, mod,
                                                       result$EAP.rel,
                                                       result$regr.coeff,
                                                       result$info_crit,
                                                       i = 2, bgdata),
                    result2)
})

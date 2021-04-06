context("gather_additional_parameters_long")

test_that("gather_additional_parameters_long", {
  bgdata <- data.frame(ID_t = 1:2000)
  data(data.sim.rasch, package = "TAM")
  mod <- TAM::tam.mml(resp=data.sim.rasch, verbose = FALSE)
  eap <- list(data.frame(ID_t = 1:2000))
  EAP.rel <- info_crit <- list(NULL)
  regr.coeff <- list(data.frame(Variable = c("Intercept")))
  waves <- c("_w1", "_w2")

  regr_result <- data.frame(Variable = "Intercept",
                            TAM::tam.se(mod)$beta)
  names(regr_result) <- c("Variable", "coeff_w1", "se_w1")
  result <- list(eap = list(data.frame(ID_t = 1:2000, eap_w1 = mod$person$EAP,
                                       se_w1 = mod$person$SD.EAP)),
                 regr.coeff = list(regr_result),
                 EAP.rel = list(mod$EAP.rel),
                 info_crit = list(matrix(c(AIC(mod), BIC(mod)), nrow = 2, ncol = 1)))
  rownames(result$info_crit[[1]]) <- c("AIC", "BIC")
  colnames(result$info_crit[[1]]) <- "w1"
  expect_equivalent(gather_additional_parameters_long(eap, mod, EAP.rel,
                                                      regr.coeff, info_crit,
                                                      i = 1, j = 1, waves,
                                                      bgdata),
                    result)
  
  regr_result <- data.frame(Variable = "Intercept",
                            TAM::tam.se(mod)$beta,
                            TAM::tam.se(mod)$beta)
  names(regr_result) <- c("Variable", "coeff_w1", "se_w1", "coeff_w2", "se_w2")
  result2 <- list(eap = list(data.frame(ID_t = 1:2000, eap_w1 = mod$person$EAP,
                                        se_w1 = mod$person$SD.EAP,
                                        eap_w2 = mod$person$EAP,
                                        se_w2 = mod$person$SD.EAP)),
                  regr.coeff = list(regr_result),
                  EAP.rel = list(c(mod$EAP.rel, mod$EAP.rel)),
                  info_crit = list(matrix(c(AIC(mod), BIC(mod)), nrow = 2, ncol = 2)))
  rownames(result2$info_crit[[1]]) <- c("AIC", "BIC")
  colnames(result2$info_crit[[1]]) <- c("w1", "w2")
  expect_equivalent(gather_additional_parameters_long(result$eap, mod,
                                                      result$EAP.rel,
                                                      result$regr.coeff,
                                                      result$info_crit,
                                                      i = 1, j = 2, waves,
                                                      bgdata),
                    result2)
})

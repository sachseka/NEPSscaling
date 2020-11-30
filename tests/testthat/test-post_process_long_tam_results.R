context("post_process_long_tam_results")

library(TAM)
data(data.sim.rasch)
bgdata <- data.frame(ID_t = 1:2000,
                     var1 = 1:2000) # run with complete bg, impute_pvs tested at different place
mod <- tam.mml(data.sim.rasch, Y = bgdata[, -1], verbose = FALSE)
npv <- 3
control <- list(ML = list(
  nmi = 10L, ntheta = 2000, normal.approx = FALSE, samp.regr = FALSE,
  theta.model = FALSE, np.adj = 8, na.grid = 5, minbucket = 5, cp = 0.0001
))
waves <- c("_w1", "_w2")

# i = 1 imputation!
pvs <- EAP.rel <- regr.coeff <- list(list())
eap <- list(data.frame(ID_t = 1:2000))

result <- list(
  eap = list(
    data.frame(ID_t = 1:2000,
               EAP = mod$person$EAP, SD.EAP = mod$person$SD.EAP) # renamed later
  ),
  regr.coeff = list(TAM::tam.se(mod)$beta),
  tmp_pvs = list(# wave
    data.frame(ID_t = 1:2000, var1 = 1:2000, pweights = 1, PV_w1 = 1), # npv
    data.frame(ID_t = 1:2000, var1 = 1:2000, pweights = 1, PV_w1 = 1),
    data.frame(ID_t = 1:2000, var1 = 1:2000, pweights = 1, PV_w1 = 1)
  ),
  EAP.rel = list(mod$EAP.rel)
)
colnames(result$regr.coeff[[1]]) <- c("coeff_w1", "se_w1")
rownames(result$regr.coeff[[1]]) <- c("Intercept", "var1")

test_that("post_process_long_tam_results: i = 1, j = 1", {
  test <- post_process_long_tam_results(mod, npv, control, imp = NULL,
                                        bgdatacom = NULL, eap, i = 1, j = 1,
                                        EAP.rel, regr.coeff, bgdata, waves)
  expect_equal(test$eap, result$eap)
  expect_equal(test$regr.coeff, result$regr.coeff)
  expect_equal(test$EAP.rel, result$EAP.rel)
  expect_equal(dim(test$tmp_pvs), dim(result$tmp_pvs))
  expect_equal(dim(test$tmp_pvs[[1]]), dim(result$tmp_pvs[[1]]))
  expect_equal(test$tmp_pvs[[1]][, -4], result$tmp_pvs[[1]][, -4])
})

eap <- result$eap
regr.coeff <- result$regr.coeff
EAP.rel <- result$EAP.rel
result <- list(
  eap = list(
    data.frame(ID_t = 1:2000,
               EAP.x = mod$person$EAP, SD.EAP.x = mod$person$SD.EAP,
               EAP.y = mod$person$EAP, SD.EAP.y = mod$person$SD.EAP)
  ),
  regr.coeff = list(cbind(regr.coeff[[1]], TAM::tam.se(mod)$beta)),
  tmp_pvs = list( # wave
    data.frame(ID_t = 1:2000, var1 = 1:2000, pweights = 1, PV_w2 = 1), # npv
    data.frame(ID_t = 1:2000, var1 = 1:2000, pweights = 1, PV_w2 = 1), # TAM stuff discarded later
    data.frame(ID_t = 1:2000, var1 = 1:2000, pweights = 1, PV_w2 = 1)
  ),
  EAP.rel = list(c(mod$EAP.rel, mod$EAP.rel))
)
colnames(result$regr.coeff[[1]]) <- c("coeff_w1", "se_w1", "coeff_w2", "se_w2")
rownames(result$regr.coeff[[1]]) <- c("Intercept", "var1")

test_that("post_process_long_tam_results: i = 1, j = 1", {
  test <- post_process_long_tam_results(mod, npv, control, imp = NULL,
                                        bgdatacom = NULL, eap, i = 1, j = 2,
                                        EAP.rel, regr.coeff, bgdata, waves)
  expect_equal(test$eap, result$eap)
  expect_equal(test$regr.coeff, result$regr.coeff)
  expect_equal(test$EAP.rel, result$EAP.rel)
  expect_equal(dim(test$tmp_pvs), dim(result$tmp_pvs))
  expect_equal(dim(test$tmp_pvs[[1]]), dim(result$tmp_pvs[[1]]))
  expect_equal(test$tmp_pvs[[1]][, -4], result$tmp_pvs[[1]][, -4])
})

detach("package:TAM")

rm(data.sim.rasch, eap, mod, pvs, result, bgdata, control, EAP.rel, npv,
   regr.coeff, waves)

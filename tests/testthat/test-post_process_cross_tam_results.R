context("post_process_cross_tam_results")

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

# the variable i addresses the different nested lists!
eap <- list(data.frame(ID_t = 1:2000),
            data.frame(ID_t = 1:2000)) # nested lists per imputed data set
EAP.rel <- NULL
regr.coeff <- NULL
pvs <- list(list(), list()) # nested lists per imputed data set

result <- list(
  eap = list(
    data.frame(ID_t = 1:2000, eap = mod$person$EAP, se = mod$person$SD.EAP),
    data.frame(ID_t = 1:2000)
  ),
  regr.coeff = TAM::tam.se(mod)$beta,
  pvs = list(
    list(# nmi
      data.frame(ID_t = 1:2000, var1 = 1:2000, PV = 1), # npv
      data.frame(ID_t = 1:2000, var1 = 1:2000, PV = 1),
      data.frame(ID_t = 1:2000, var1 = 1:2000, PV = 1)
    ),
    list()
  ),
  EAP.rel = mod$EAP.rel
)
colnames(result$regr.coeff) <- c("imp1_coeff", "imp1_se")
rownames(result$regr.coeff) <- c("Intercept", "var1")

test_that("post_process_cross_tam_results", {
  test <- post_process_cross_tam_results(mod, npv, control, imp = NULL,
                                         bgdatacom = NULL, eap, i = 1,
                                         EAP.rel, regr.coeff, pvs, bgdata)
  expect_equal(test$eap, result$eap)
  expect_equal(test$regr.coeff, result$regr.coeff)
  expect_equal(test$EAP.rel, result$EAP.rel)
  expect_equal(names(test$pvs[[1]][[1]]), names(result$pvs[[1]][[1]]))
  expect_equal(dim(test$pvs[[1]]), dim(result$pvs[[1]]))
  expect_equal(dim(test$pvs[[1]][[1]]), dim(result$pvs[[1]][[1]]))
  expect_equal(test$pvs[[1]][[1]][, -3], result$pvs[[1]][[1]][, -3])
})

regr.coeff <- result$regr.coeff
EAP.rel <- result$EAP.rel
result$regr.coeff <- cbind(result$regr.coeff, result$regr.coeff)
colnames(result$regr.coeff) <- c("imp1_coeff", "imp1_se",
                                 "imp2_coeff", "imp2_se")
result$eap = list(
  data.frame(ID_t = 1:2000),
  data.frame(ID_t = 1:2000, eap = mod$person$EAP, se = mod$person$SD.EAP)
)
result$EAP.rel <- c(result$EAP.rel, result$EAP.rel)
result$pvs[[2]] <- list(# 2
  data.frame(ID_t = 1:2000, var1 = 1:2000, PV = 1), # npv
  data.frame(ID_t = 1:2000, var1 = 1:2000, PV = 1),
  data.frame(ID_t = 1:2000, var1 = 1:2000, PV = 1)
)

test_that("post_process_cross_tam_results", {
  test <- post_process_cross_tam_results(mod, npv, control, imp = NULL,
                                         bgdatacom = NULL, eap, i = 2,
                                         EAP.rel, regr.coeff, pvs, bgdata)
  expect_equal(test$eap, result$eap)
  expect_equal(test$regr.coeff, result$regr.coeff)
  expect_equal(test$EAP.rel, result$EAP.rel)
  expect_equal(names(test$pvs[[2]][[1]]), names(result$pvs[[2]][[1]]))
  expect_equal(dim(test$pvs[[2]]), dim(result$pvs[[2]]))
  expect_equal(dim(test$pvs[[2]][[1]]), dim(result$pvs[[2]][[1]]))
  expect_equal(test$pvs[[2]][[1]][, -3], result$pvs[[2]][[1]][, -3])
})

detach("package:TAM")

rm(data.sim.rasch, eap, mod, pvs, result, bgdata, control, EAP.rel, npv,
   regr.coeff)

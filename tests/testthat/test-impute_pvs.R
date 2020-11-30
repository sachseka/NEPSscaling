context("impute_pvs")

library(TAM)
data(data.sim.rasch)
mod <- tam.mml(data.sim.rasch, verbose = FALSE)

npv <- 3
control <- list(ML = list(
  nmi = 10L, ntheta = 2000, normal.approx = FALSE, samp.regr = FALSE,
  theta.model = FALSE, np.adj = 8, na.grid = 5, minbucket = 5, cp = 0.0001
))
bgdata <- data.frame(ID_t = 1:100,
                     var1 = 1:100)
waves <- c("_w1")
j <- 1L

test_that("with and without background data", {
  # no bgdata
  test <- impute_pvs(mod, npv, control, bgdata = NULL, imp = NULL,
                     bgdatacom = NULL, waves, j)
  expect_equal(length(test), 3)
  expect_equal(ncol(test[[npv]]), 3) # incl pid/ID_t, pweights
  expect_equal(names(test[[npv]][1]), "pid")
  expect_equal(names(test[[npv]][3]), "PV_w1")

  # complete bgdata
  test <- impute_pvs(mod, npv, control, bgdata, NULL, NULL, waves, j)
  expect_equal(length(test), 3)
  expect_equal(ncol(test[[npv]]), 4) # incl pid/ID_t, pweights
  expect_equal(names(test[[npv]][1]), "ID_t")
  expect_equal(names(test[[npv]][4]), "PV_w1")

  # completed bgdata
  test <- impute_pvs(mod, npv, control, bgdata, 10L, bgdata, waves, j)
  expect_equal(length(test), 3)
  expect_equal(ncol(test[[npv]]), 4) # incl pid/ID_t, pweights
  expect_equal(names(test[[npv]][1]), "ID_t")
  expect_equal(names(test[[npv]][4]), "PV_w1")
})

detach("package:TAM")

rm(bgdata, control, data.sim.rasch, mod, j, npv, waves)

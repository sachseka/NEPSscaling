context("estimate_cross_rasch_uncorrected")

set.seed(1)

bgdata <- NULL
imp <- NULL
frmY <- NULL
resp <- data.frame(ID_t = 1:500)
PCM <- FALSE
ID_t <- data.frame(ID_t = 1:500)
waves <- "_w3"
type <- "cross"
domain <- "MA"
SC <- "SC6"
control <- list(ML = list(
  nmi = 10L, ntheta = 2000, normal.approx = FALSE, samp.regr = FALSE,
  theta.model = FALSE, np.adj = 8, na.grid = 5, minbucket = 5, cp = 0.0001
))
npv <- 2

items <- rownames(xsi.fixed$cross[[domain]][[SC]][["w3"]])
for (i in items) {
  resp[[i]] <- rbinom(500, 1, 0.5)
}
rm(i)

mod <- TAM::tam.mml(resp[, -1], Q = matrix(1, nrow = length(items)),
                    irtmodel = "1PL", dataY = bgdata, formulaY = frmY,
                    xsi.fixed = xsi.fixed[[type]][[domain]][[SC]][["w3"]],
                    verbose = FALSE)

result <- list(
  eap = list(
    data.frame(ID_t = 1:500,
               eap = mod$person$EAP,
               se = mod$person$SD.EAP)
  ),
  pvs = list(
    list( # nmi
      data.frame(ID_t = 1:500, PV = 1),
      data.frame(ID_t = 1:500, PV = 1)
    )
  ),
  EAP.rel = mod$EAP.rel,
  regr.coeff = TAM::tam.se(mod)$beta,
  mod = mod
)
rownames(result$regr.coeff) <- "Intercept"
colnames(result$regr.coeff) <- c("imp1_coeff", "imp1_se")

test_that("estimate_cross_rasch_uncorrected", {
  test <- estimate_cross_rasch_uncorrected(bgdata, imp, resp,
                                           waves, frmY,
                                           ID_t, type, domain,
                                           SC, control, npv)
  expect_equal(test$eap, result$eap)
  expect_equal(test$EAP.rel, result$EAP.rel)
  expect_equal(test$regr.coeff, result$regr.coeff)
  expect_equal(names(test$pvs[[1]][[1]]), names(result$pvs[[1]][[1]]))
})

rm(control, ID_t, items, mod, PCM, resp, result, bgdata, domain, frmY, imp, npv,
   SC, type, waves)

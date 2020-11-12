context("estimate_longitudinal")

set.seed(1)

bgdata <- NULL
imp <- NULL
frmY <- NULL
resp <- list(data.frame(ID_t = 1:500),
             data.frame(ID_t = 1:500))
PCM <- list(FALSE, FALSE)
ID_t <- data.frame(ID_t = 1:500)
waves <- c("_w3", "_w9")
type <- "long"
domain <- "MA"
SC <- "SC6"
control <- list(ML = list(
  nmi = 10L, ntheta = 2000, normal.approx = FALSE, samp.regr = FALSE,
  theta.model = FALSE, np.adj = 8, na.grid = 5, minbucket = 5, cp = 0.0001
))
npv <- 2

items <- lapply(xsi.fixed$long[[domain]][[SC]], rownames)
for (w in 1:length(waves)) {
  for (i in items[[w]]) {
    resp[[w]][[i]] <- rbinom(500, 1, 0.5)
  }
}
rm(w, i)

mod <- list(TAM::tam.mml(resp[[1]][, -1], Q = matrix(1, nrow = length(items[[1]])),
                         irtmodel = "1PL", dataY = bgdata, formulaY = frmY,
                         xsi.fixed = xsi.fixed[[type]][[domain]][[SC]][["w3"]],
                         verbose = FALSE),
            TAM::tam.mml(resp[[2]][, -1], Q = matrix(1, nrow = length(items[[2]])),
                         irtmodel = "1PL", dataY = bgdata, formulaY = frmY,
                         xsi.fixed = xsi.fixed[[type]][[domain]][[SC]][["w9"]],
                         verbose = FALSE))

result <- list(
  eap = list(
    data.frame(ID_t = 1:500,
               eap_w3 = mod[[1]]$person$EAP,
               se_w3 = mod[[1]]$person$SD.EAP,
               eap_w9 = mod[[2]]$person$EAP,
               se_w9 = mod[[2]]$person$SD.EAP)
    ),
  pvs = list(
    list( # nmi
      data.frame(ID_t = 1:500, pweights = 1, # discarded later
                 PV_w3 = 1, PV_w9 = 1),
      data.frame(ID_t = 1:500, pweights = 1,
                 PV_w3 = 1, PV_w9 = 1)
    )
  ),
  EAP.rel = list(
    c(mod[[1]]$EAP.rel, mod[[2]]$EAP.rel)
  ),
  regr.coeff = list(
    cbind(TAM::tam.se(mod[[1]])$beta, TAM::tam.se(mod[[2]])$beta)
  ),
  mod = mod
)
rownames(result$regr.coeff[[1]]) <- "Intercept"
colnames(result$regr.coeff[[1]]) <- c("coeff_w3", "se_w3", "coeff_w9", "se_w9")

test_that("estimate_longitudinal", {
  test <- estimate_longitudinal(bgdata, imp, frmY, resp, PCM, ID_t,
                                waves, type, domain, SC, control, npv)
  expect_equal(test$eap, result$eap)
  expect_equal(test$EAP.rel, result$EAP.rel)
  expect_equal(test$regr.coeff, result$regr.coeff)
  expect_equal(names(test$pvs[[1]][[1]]), names(result$pvs[[1]][[1]]))
})

rm(control, ID_t, items, mod, PCM, resp, result, bgdata, domain, frmY, imp, npv,
   SC, type, waves)

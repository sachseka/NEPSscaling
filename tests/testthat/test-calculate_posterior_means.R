context("calculate_posterior_means")

test_that("posterior means (longitudinal)", {
  
  eap <- data.frame(ID_t = 1:100,
                    eap_w1 = 1:100,
                    se_w1 = 1:100,
                    eap_w2 = 1:100,
                    se_w2 = 1:100)
  wle <- data.frame(ID_t = 1:100,
                    wle_w1 = 1:100,
                    se_w1 = 1:100,
                    wle_w2 = 1:100,
                    se_w2 = 1:100)
  npv <- 3
  pv <- replicate(npv,
                  data.frame(
                    ID_t = 1:100,
                    PV_w1 = 1:100,
                    PV_w2 = 1001:1100
                  ), simplify = FALSE)
  waves <- c("_w1", "_w2")
  
  test <- calculate_posterior_means(eap, wle = NULL, pv, waves, npv)
  expect_equal(names(test), c("eap", "pv"))

  test <- calculate_posterior_means(eap, wle, pv, waves, npv)
  expect_equal(names(test), c("eap", "wle", "pv"))
  expect_equal(names(test$pv), c("total", "imputations"))
  expect_equal(test$eap, c(w1 = mean(1:100), w2 = mean(1:100)))
  expect_equal(test$wle, c(w1 = mean(1:100), w2 = mean(1:100)))
  expect_equal(test$pv$total, c(PV_w1 = (mean(1:100)*3)/npv,
                                PV_w2 = (mean(1001:1100)*3)/npv))
  expect_equal(test$pv$imputations,
               list(
                 c(PV_w1 = mean(1:100), PV_w2 = mean(1001:1100)),
                 c(PV_w1 = mean(1:100), PV_w2 = mean(1001:1100)),
                 c(PV_w1 = mean(1:100), PV_w2 = mean(1001:1100))
               ))
})


test_that("posterior means (cross-sectional)", {
  
  eap <- data.frame(ID_t = 1:100,
                    eap_w1 = 1:100,
                    se_w1 = 1:100)
  wle <- data.frame(ID_t = 1:100,
                    wle_w1 = 1:100,
                    se_w1 = 1:100)
  npv <- 3
  pv <- replicate(npv,
                  data.frame(
                    ID_t = 1:100,
                    PV_w1 = 1:100
                  ), simplify = FALSE)
  waves <- c("_w1", "_w2")
  
  test <- calculate_posterior_means(eap, wle = NULL, pv, waves[1], npv)
  expect_equal(names(test), c("eap", "pv"))

  test <- calculate_posterior_means(eap, wle, pv,
                                    waves[1], npv)
  expect_equal(names(test), c("eap", "wle", "pv"))
  expect_equal(names(test$pv), c("total", "imputations"))
  expect_equal(test$eap, c(w1 = mean(1:100)))
  expect_equal(test$wle, c(w1 = mean(1:100)))
  expect_equal(test$pv$total, c(PV_w1 = (mean(1:100)*3)/npv))
  expect_equal(test$pv$imputations,
               list(
                 c(PV_w1 = mean(1:100)),
                 c(PV_w1 = mean(1:100)),
                 c(PV_w1 = mean(1:100))
               ))
})

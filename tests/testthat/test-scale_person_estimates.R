context("scale_person_estimates")

test_that("scale_person_estimates: example SC5 MA (no correction of link term)", {
  SC <- "SC5"
  domain <- "MA"
  waves <- c("_w1", "_w12")
  long_IDs <- list(26:75)
  pv <- list(
    data.frame(ID_t = 1:100,
               PV_w1 = 0,
               PV_w12 = c(rep(0, 50), rep(1, 50))),
    data.frame(ID_t = 1:100,
               PV_w1 = 0,
               PV_w12 = c(rep(0, 50), rep(1, 50)))
  )
  eap <- data.frame(ID_t = 1:100,
                    eap_w1 = 0,
                    eap_w12 = c(rep(0, 50), rep(1, 50)))
  wle <- data.frame(ID_t = 1:100,
                    wle_w1 = 0,
                    wle_w12 = c(rep(0, 50), rep(1, 50)))
  # means: w1: 0, w2: 0.5
  
  term <- 0.5 - link_constant[[SC]][[domain]][["w12"]]
  result <- list(
    pv = list(
      data.frame(ID_t = 1:100, PV_w1 = 0,
                 PV_w12 = c(rep(-term, 50), rep(1 - term, 50))),
      data.frame(ID_t = 1:100, PV_w1 = 0,
                 PV_w12 = c(rep(-term, 50), rep(1 - term, 50)))
    ),
    wle = data.frame(ID_t = 1:100, wle_w1 = 0,
                     wle_w12 = c(rep(-term, 50), rep(1 - term, 50))),
    eap = data.frame(ID_t = 1:100, eap_w1 = 0,
                     eap_w12 = c(rep(-term, 50), rep(1 - term, 50)))
  )
  test <- scale_person_estimates(pv, wle, eap, SC, domain, waves, long_IDs)
  expect_equal(test, result)
})


test_that("scale_person_estimates: example SC4 MA (correction of link term)", {
  
  SC <- "SC4"
  domain <- "MA"
  waves <- c("_w1", "_w7", "_w10")
  long_IDs <- list(26:75, 1:50)
  pv <- list(
    data.frame(ID_t = 1:100,
               PV_w1 = 0,
               PV_w7 = c(rep(0, 50), rep(1, 50)),
               PV_w10 = 2),
    data.frame(ID_t = 1:100,
               PV_w1 = 0,
               PV_w7 = c(rep(0, 50), rep(1, 50)),
               PV_w10 = 2)
  )
  eap <- data.frame(ID_t = 1:100,
                    eap_w1 = 0,
                    eap_w7 = c(rep(0, 50), rep(1, 50)),
                    eap_w10 = 2)
  wle <- data.frame(ID_t = 1:100,
                    wle_w1 = 0,
                    wle_w7 = c(rep(0, 50), rep(1, 50)),
                    wle_w10 = 2)
  # means: w1: 0, w7: 0.5, w10: 2
  
  term7 <- 0.5 - link_constant[[SC]][[domain]][["w7"]] - 0.1
  result <- list(
    pv = list(
      data.frame(ID_t = 1:100, PV_w1 = 0,
                 PV_w7 = c(rep(-term7, 50), rep(1 - term7, 50)),
                 PV_w10 = 2),
      data.frame(ID_t = 1:100, PV_w1 = 0,
                 PV_w7 = c(rep(-term7, 50), rep(1 - term7, 50)),
                 PV_w10 = 2)
    ),
    wle = data.frame(ID_t = 1:100, wle_w1 = 0,
                     wle_w7 = c(rep(-term7, 50), rep(1 - term7, 50)),
                     wle_w10 = 2),
    eap = data.frame(ID_t = 1:100, eap_w1 = 0,
                     eap_w7 = c(rep(-term7, 50), rep(1 - term7, 50)),
                     eap_w10 = 2)
  )
  term10 <- 2 - mean(result$wle$wle_w7[1:50]) - # longitudinal at tp 2
    link_constant[[SC]][[domain]][["w10"]]
  result$pv[[1]]$PV_w10 <- 2 - term10
  result$pv[[2]]$PV_w10 <- 2 - term10
  result$wle$wle_w10 <- 2 - term10
  result$eap$eap_w10 <- 2 - term10
  test <- scale_person_estimates(pv, wle, eap, SC, domain, waves, long_IDs)
  expect_equal(test, result)
})

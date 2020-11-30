context("rescale_sc6_reading")

test_that("rescale_sc6_reading: pv, eap and wle", {
  
  SC <- "SC6"
  domain <- "RE"
  long_IDs <- list(w3 = 1:50, w5 = 51:100)
  eap <- data.frame(ID_t = 1:100, eap_w3 = 0, eap_w5 = 1, eap_w9 = 0)
  pv <- list(
    data.frame(ID_t = 1:100, PV_w3 = 0, PV_w5 = 1, PV_w9 = 0),
    data.frame(ID_t = 1:100, PV_w3 = 0, PV_w5 = 1, PV_w9 = 0)
  )
  wle <- data.frame(ID_t = 1:100, wle_w3 = 0, wle_w5 = 1, wle_w9 = 0)
  # means: w3: 0, w5: 1, w9/w3: 0, w9/w5: 0
  
  term1 <- -link_constant[[SC]][[domain]][["w9"]][["B67"]] - 0.08
  term2 <- -1 - link_constant[[SC]][[domain]][["w9"]][["B69"]] - 0.15
  result <- list(
    eap = data.frame(ID_t = 1:100,
                     eap_w3 = 0,
                     eap_w5 = 1,
                     eap_w9 = c(rep(-term1, 50), rep(-term2, 50))),
    pv = list(
      data.frame(ID_t = 1:100,
                 PV_w3 = 0,
                 PV_w5 = 1,
                 PV_w9 = c(rep(-term1, 50), rep(-term2, 50))),
      data.frame(ID_t = 1:100,
                 PV_w3 = 0,
                 PV_w5 = 1,
                 PV_w9 = c(rep(-term1, 50), rep(-term2, 50)))
    ),
    wle = data.frame(ID_t = 1:100,
                     wle_w3 = 0,
                     wle_w5 = 1,
                     wle_w9 = c(rep(-term1, 50), rep(-term2, 50)))
  )
  test <- rescale_sc6_reading(SC, domain, long_IDs, eap, pv, wle)
  expect_equal(test, result)
})

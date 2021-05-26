context("apply_correction_for_changed_test_rotation")

test_that("apply_correction_for_changed_test_rotation", {
  pos <- 1
  wave <- "w1"
  correction <- 1
  wle <- data.frame(ID_t = 1:100, wle_w1 = 0)
  eap <- list(data.frame(ID_t = 1:100, eap_w1 = 0),
              data.frame(ID_t = 1:100, eap_w1 = 0))
  pv <- list(data.frame(ID_t = 1:100, PV_w1 = 0),
             data.frame(ID_t = 1:100, PV_w1 = 0))
  position <- data.frame(ID_t = 1:100, position = rep(1:2, 50))

  result <- list(
    wle = data.frame(ID_t = 1:100, wle_w1 = rep(1:0, 50)),
    eap = list(data.frame(ID_t = 1:100, eap_w1 = rep(1:0, 50)),
               data.frame(ID_t = 1:100, eap_w1 = rep(1:0, 50))),
    pv = list(data.frame(ID_t = 1:100, PV_w1 = rep(1:0, 50)),
              data.frame(ID_t = 1:100, PV_w1 = rep(1:0, 50)))
  )
  expect_equivalent(apply_correction_for_changed_test_rotation(pos, wave,
                                                               correction, wle,
                                                               eap, pv,
                                                               position),
                    result)
})

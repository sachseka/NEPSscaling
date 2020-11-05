context("correct_for_changed_test_rotation")

position <- data.frame(ID_t = 1:100,
                       position = rep(1:2, 50))
eap <- data.frame(ID_t = 1:100,
                  eap_w1 = rep(0, 100),
                  eap_w3 = rep(0, 100),
                  eap_w4 = rep(0, 100))
wle <- data.frame(ID_t = 1:100,
                  wle_w1 = rep(0, 100),
                  wle_w3 = rep(0, 100),
                  wle_w4 = rep(0, 100))
pv <- replicate(2, data.frame(ID_t = 1:100,
                              PV_w1 = rep(0, 100),
                              PV_w3 = rep(0, 100),
                              PV_w4 = rep(0, 100)), simplify = FALSE)

test_that("rotation change: test SC2", {
  result <- list(
    wle = data.frame(ID_t = 1:100,
                     wle_w1 = rep(0.03, 100),
                     wle_w3 = rep(c(0, 0.03), 50),
                     wle_w4 = rep(0, 100)),
    eap = data.frame(ID_t = 1:100,
                     eap_w1 = rep(0.03, 100),
                     eap_w3 = rep(c(0, 0.03), 50),
                     eap_w4 = rep(0, 100)),
    pv = replicate(2, data.frame(ID_t = 1:100,
                                 PV_w1 = rep(0.03, 100),
                                 PV_w3 = rep(c(0, 0.03), 50),
                                 PV_w4 = rep(0, 100)), simplify = FALSE)
  )
  test <- correct_for_changed_test_rotation(SC = "SC2", domain = "VO", position,
                                            wle, eap, pv)
  expect_equal(names(test), c("wle", "eap", "pv"))
  expect_equal(test, result)

  result <- list(
    wle = data.frame(ID_t = 1:100,
                     wle_w1 = rep(0, 100),
                     wle_w3 = rep(0, 100),
                     wle_w4 = rep(c(0, 0.0035), 50)),
    eap = data.frame(ID_t = 1:100,
                     eap_w1 = rep(0, 100),
                     eap_w3 = rep(0, 100),
                     eap_w4 = rep(c(0, 0.0035), 50)),
    pv = replicate(2, data.frame(ID_t = 1:100,
                                 PV_w1 = rep(0, 100),
                                 PV_w3 = rep(0, 100),
                                 PV_w4 = rep(c(0, 0.0035), 50)), simplify = FALSE)
  )
  test <- correct_for_changed_test_rotation(SC = "SC2", domain = "MA", position,
                                            wle, eap, pv)
  expect_equal(names(test), c("wle", "eap", "pv"))
  expect_equal(test, result)
})

eap <- data.frame(ID_t = 1:100,
                  eap_w6 = rep(0, 100))
wle <- data.frame(ID_t = 1:100,
                  wle_w6 = rep(0, 100))
pv <- list(
  data.frame(ID_t = 1:100,
             PV_w6 = rep(0, 100)),
  data.frame(ID_t = 1:100,
             PV_w6 = rep(0, 100))
)
test_that("rotation change: test SC3", {
  result <- list(
    wle = data.frame(ID_t = 1:100,
                     wle_w6 = rep(c(0, 0.174), 50)),
    eap = data.frame(ID_t = 1:100,
                     eap_w6 = rep(c(0, 0.174), 50)),
    pv = replicate(2, data.frame(ID_t = 1:100,
                                 PV_w6 = rep(c(0, 0.174), 50)), simplify = FALSE)
  )
  test <- correct_for_changed_test_rotation(SC = "SC3", domain = "RE", position,
                                            wle, eap, pv)
  expect_equal(names(test), c("wle", "eap", "pv"))
  expect_equal(test, result)
})

eap <- data.frame(ID_t = 1:100,
                  eap_w7 = rep(0, 100))
wle <- data.frame(ID_t = 1:100,
                  wle_w7 = rep(0, 100))
pv <- list(
  data.frame(ID_t = 1:100,
             PV_w7 = rep(0, 100)),
  data.frame(ID_t = 1:100,
             PV_w7 = rep(0, 100))
)
test_that("rotation change: test SC4", {
  result <- list(
    wle = data.frame(ID_t = 1:100,
                     wle_w7 = rep(c(0, 0.164), 50)),
    eap = data.frame(ID_t = 1:100,
                     eap_w7 = rep(c(0, 0.164), 50)),
    pv = replicate(2, data.frame(ID_t = 1:100,
                                 PV_w7 = rep(c(0, 0.164), 50)), simplify = FALSE)
  )
  test <- correct_for_changed_test_rotation(SC = "SC4", domain = "RE", position,
                                            wle, eap, pv)
  expect_equal(names(test), c("wle", "eap", "pv"))
  expect_equal(test, result)

  result <- list(
    wle = data.frame(ID_t = 1:100,
                     wle_w7 = rep(c(-0.060, 0), 50)),
    eap = data.frame(ID_t = 1:100,
                     eap_w7 = rep(c(-0.060, 0), 50)),
    pv = replicate(2, data.frame(ID_t = 1:100,
                                 PV_w7 = rep(c(-0.060, 0), 50)), simplify = FALSE)
  )
  test <- correct_for_changed_test_rotation(SC = "SC4", domain = "MA", position,
                                            wle, eap, pv)
  expect_equal(names(test), c("wle", "eap", "pv"))
  expect_equal(test, result)
})

rm(eap, wle, pv, position)

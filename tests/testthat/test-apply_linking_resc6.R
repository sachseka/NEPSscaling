context("apply_linking_resc6")

test_that("apply_linking_resc6", {
  eap <- list(data.frame(ID_t = 1:100, eap_w9 = 0),
              data.frame(ID_t = 1:100, eap_w9 = 0))
  wle <- data.frame(ID_t = 1:100, wle_w9 = 0)
  pv <- list(
    data.frame(ID_t = 1:100, PV_w9 = 0), data.frame(ID_t = 1:100, PV_w9 = 0)
  )
  term <- list(eap = list(c(1, 2), c(1, 2)), wle = c(1, 2),
               pv = list(c(1, 2), c(1, 2)))
  long_IDs <- list(w3 = 1:50, w5 = 51:100)

  result <- list(
    eap = list(
      data.frame(ID_t = 1:100, eap_w9 = rep(c(-1, -2), each = 50)),
      data.frame(ID_t = 1:100, eap_w9 = rep(c(-1, -2), each = 50))
    ),
    pv = list(
      data.frame(ID_t = 1:100, PV_w9 = rep(c(-1, -2), each = 50)),
      data.frame(ID_t = 1:100, PV_w9 = rep(c(-1, -2), each = 50))
    ),
    wle = data.frame(ID_t = 1:100, wle_w9 = rep(c(-1, -2), each = 50))
  )
  expect_equivalent(apply_linking_resc6(eap, pv, wle, term, long_IDs),
                    result)
})

context("apply_linking")

test_that("apply_linking", {
  eap <- list(data.frame(ID_t = 1:100, eap_w9 = 0),
              data.frame(ID_t = 1:100, eap_w9 = 0))
  wle <- data.frame(ID_t = 1:100, wle_w9 = 0)
  pv <- list(
    data.frame(ID_t = 1:100, PV_w9 = 0), data.frame(ID_t = 1:100, PV_w9 = 0)
  )
  term <- list(eap = list(1, 1), wle = 2, pv = list(1, 2))

  result <- list(
    eap = list(data.frame(ID_t = 1:100, eap_w9 = -1),
               data.frame(ID_t = 1:100, eap_w9 = -1)),
    pv = list(
      data.frame(ID_t = 1:100, PV_w9 = rep(-1, 100)),
      data.frame(ID_t = 1:100, PV_w9 = rep(-2, 100))
    ),
    wle = data.frame(ID_t = 1:100, wle_w9 = -2)
  )
  # w starts at 2 because not linking is needed for 1st assessment
  expect_equivalent(apply_linking(eap, pv, wle, term,
                                  waves = c("_w1", "_w9"), w = 2),
                    result)
})

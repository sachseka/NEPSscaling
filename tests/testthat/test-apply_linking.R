context("apply_linking")

test_that("apply_linking", {
  eap <- vector("numeric", 100)
  wle <- vector("numeric", 100)
  pv <- list(
    data.frame(ID_t = 1:100, PV_w9 = 0), data.frame(ID_t = 1:100, PV_w9 = 0)
  )
  term <- list(eap = 1, wle = 2, pv = list(1, 2))

  result <- list(
    eap = rep(-1, 100),
    pv = list(
      data.frame(ID_t = 1:100, PV_w9 = rep(-1, 100)),
      data.frame(ID_t = 1:100, PV_w9 = rep(-2, 100))
    ),
    wle = rep(-2, 100)
  )
  # w starts at 2 because not linking is needed for 1st assessment
  expect_equivalent(apply_linking(eap, pv, wle, term,
                                  waves = c("_w1", "_w9"), w = 2),
                    result)
})

context("link_longitudinal_plausible_values")

npv <- 2
min_valid <- 3
valid_responses_per_person <- data.frame(ID_t = 1:100,
                                         valid_w1 = 10, valid_w12 = 10)
SC <- "SC5"
domain <- "MA"
waves <- c("_w1", "_w12")
datalist <- list(
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
data <- data.frame(ID_t = 1:100, rea9_sc1u = rnorm(100),
                   wave_w5 = c(rep(1, 50), rep(0, 50)),
                   wave_w3 = c(rep(0, 50), rep(1, 50)))

test_that("link_longitudinal_plausible_values: ...", {
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
  test <- link_longitudinal_plausible_values(datalist, npv, min_valid,
                                             valid_responses_per_person,
                                             waves, eap, wle,
                                             data, SC, domain)
  expect_equal(test, result)
})

rm(data, datalist, eap, valid_responses_per_person, wle, domain, min_valid,
   npv, SC, waves)

context("identify_longitudinal_ids")

SC <- "SC1"
domain <- "MA"
data <- data.frame(ID_t = 1:100, rea9_sc1u = rnorm(100),
                   wave_w5 = c(rep(1, 50), rep(0, 50)),
                   wave_w3 = c(rep(0, 50), rep(1, 50)))
ind <- c(13, 66, 78, 80, 85)
data$rea9_sc1u[ind] <- NA
waves <- c("_w1", "_w3")
eap <- data.frame(ID_t = 1:100, eap_w1 = rnorm(100), eap_w3 = rnorm(100))
eap$eap_w1[90:100] <- NA
eap$eap_w3[1:10] <- NA

test_that("longitudinal ids for regular case", {
  test <- identify_longitudinal_ids(SC, domain, data, waves, eap)
  expect_equal(test, list(11:89))
})

SC <- "SC6"
domain <- "RE"

test_that("longitudinal ids for SC6/RE", {
  test <- identify_longitudinal_ids(SC, domain, data, waves, eap)
  expect_equal(test, list(w3 = c(51:65, 67:77, 79, 81:84, 86:100),
                          w5 = c(1:12, 14:50)))
})

rm(data, eap, domain, ind, SC, waves)

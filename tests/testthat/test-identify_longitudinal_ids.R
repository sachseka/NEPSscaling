context("identify_longitudinal_ids")

test_that("longitudinal ids for regular case", {

  SC <- "SC6"
  domain <- "MA"
  data <- data.frame(ID_t = 1:100, rea9_sc1u = rnorm(100),
                     maa3_sc1u = 1,
                     maa9_sc1u = 1)
  data$maa3_sc1u[1:40] <- NA
  data$maa9_sc1u[61:100] <- NA
  waves <- c("_w3", "_w9")

  test <- identify_longitudinal_ids(SC, domain, data, waves)
  expect_equal(test, list(41:60))
})


test_that("longitudinal ids for SC6/RE", {

  SC <- "SC6"
  domain <- "RE"
  data <- data.frame(ID_t = 1:100, rea9_sc1u = rnorm(100),
                     wave_w5 = c(rep(1, 50), rep(0, 50)),
                     wave_w3 = c(rep(0, 50), rep(1, 50)))
  ind <- c(13, 66, 78, 80, 85)
  data$rea9_sc1u[ind] <- NA
  waves <- c("_w3", "_w5", "_w9")

  test <- identify_longitudinal_ids(SC, domain, data, waves)
  expect_equal(test, list(w3 = c(51:65, 67:77, 79, 81:84, 86:100),
                          w5 = c(1:12, 14:50)))
})

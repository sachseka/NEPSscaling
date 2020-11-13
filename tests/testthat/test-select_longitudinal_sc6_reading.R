context("select_longitudinal_sc6_reading")

test_that("select_longitudinal_sc6_reading: different values of min_valid", {

  data <- data.frame(ID_t = 1:100,
                     wave_w3 = rep(1:0, 50),
                     wave_w5 = rep(0:1, 50),
                     rea30110_c = 1:100,
                     rea3012s_c = 1:100,
                     rea30130_c = 1:100,
                     rea30140_c = 1:100,
                     rea90101s_c = 1:100,
                     rea90102s_c = 1:100,
                     rea901030_c = 1:100,
                     rea90104s_c = 1:100,
                     rea9_sc1u = rnorm(100))
  data$rea9_sc1u[1:10] <- NA
  data$rea90101s_c[11:50] <- NA
  data$rea90102s_c[26:75] <- NA
  data$rea901030_c[51:100] <- NA

  SC <- "SC6"
  domain <- "RE"
  result <- list(
    data.frame(ID_t = seq(1, 100, 2),
               rea30110_c = seq(1, 100, 2),
               rea3012s_c = seq(1, 100, 2),
               rea30130_c = seq(1, 100, 2),
               rea30140_c = seq(1, 100, 2)),
    data.frame(ID_t = seq(2, 100, 2),
               rea30110_c = seq(2, 100, 2),
               rea3012s_c = seq(2, 100, 2),
               rea30130_c = seq(2, 100, 2),
               rea30140_c = seq(2, 100, 2)),
    data.frame(ID_t = 11:100,
               rea90101s_c = 11:100,
               rea90102s_c = 11:100,
               rea901030_c = 11:100,
               rea90104s_c = 11:100)
  )
  result[[3]]$rea90101s_c[1:40] <- NA
  result[[3]]$rea90102s_c[16:65] <- NA
  result[[3]]$rea901030_c[41:90] <- NA
  test <- select_longitudinal_sc6_reading(data, SC, domain, min_valid = 1)
  expect_equivalent(test, result)

  result[[3]] <- data.frame(ID_t = c(11:25, 76:100),
                            rea90101s_c = c(11:25, 76:100),
                            rea90102s_c = c(11:25, 76:100),
                            rea901030_c = c(11:25, 76:100),
                            rea90104s_c = c(11:25, 76:100))
  result[[3]]$rea90101s_c[1:15] <- NA
  result[[3]]$rea901030_c[16:40] <- NA
  test <- select_longitudinal_sc6_reading(data, SC, domain, min_valid = 3)
  expect_equivalent(test, result)

  result[[3]] <- result[[3]][-(1:50), ]
  test <- select_longitudinal_sc6_reading(data, SC, domain, min_valid = 4)
  expect_equivalent(test, result)
})

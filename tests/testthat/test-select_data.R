context("select_data")

test_that("select_data: all cases", {
  data <- as.data.frame(
    cbind(ID_t = 1:10,
          matrix(0, ncol = 30, nrow = 10))
  )
  names(data)[-1] <- c("rea30110_c", "rea3012s_c", "rea30130_c", "rea30140_c", "rea3015s_c", "rea30210_c",
                       "rea30220_c", "rea30230_c", "rea30240_c", "rea30250_c", "rea3028s_c", "rea30310_c",
                       "rea30320_c", "rea30330_c", "rea30340_c", "rea30350_c", "rea30360_c", "rea30370_c",
                       "rea3038s_c", "rea30410_c", "rea3042s_c", "rea30430_c", "rea30440_c", "rea30450_c",
                       "rea30460_c", "rea30510_c", "rea3052s_c", "rea30530_c", "rea3054s_c", "rea30550_c")

  result <- data.frame(
    cbind(ID_t = 1:10,
          matrix(0, ncol = 30, nrow = 10))
  )
  expect_equivalent(select_data(data, "SC6", "RE", "w3", 3),
                    result)
})

test_that("select_data: remove cases", {
  data <- as.data.frame(
    cbind(ID_t = 1:10,
          matrix(0, ncol = 30, nrow = 10))
  )
  names(data)[-1] <- c("rea30110_c", "rea3012s_c", "rea30130_c", "rea30140_c", "rea3015s_c", "rea30210_c",
                       "rea30220_c", "rea30230_c", "rea30240_c", "rea30250_c", "rea3028s_c", "rea30310_c",
                       "rea30320_c", "rea30330_c", "rea30340_c", "rea30350_c", "rea30360_c", "rea30370_c",
                       "rea3038s_c", "rea30410_c", "rea3042s_c", "rea30430_c", "rea30440_c", "rea30450_c",
                       "rea30460_c", "rea30510_c", "rea3052s_c", "rea30530_c", "rea3054s_c", "rea30550_c")
  data[3, 3:31] <- NA

  result <- data.frame(
    cbind(ID_t = c(1, 2, 4:10),
          matrix(0, ncol = 30, nrow = 9))
  )
  expect_equivalent(select_data(data, "SC6", "RE", "w3", 3),
                    result)
})

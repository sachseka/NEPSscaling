context("prepare_resp_q_longitudinal")

#TODO: item names necessary for get_indicators_for_half_scoring!

test_that("prepare_resp_q_longitudinal: general case, e.g., SC5/MA", {
  
  SC <- "SC5"
  domain <- "MA"
  items <- list(
    c("mas1q02s_c", "dummy"),
    c("mas1v032_sc5s12_c", "maa9d09s_sc5s12_c", "maa9d13s_sc5s12_c",
      "maa9r03s_sc5s12_c", "mas1q02s_sc5s12_c", "maa9v27s_sc5s12_c",
      "maa9d20s_sc5s12_c", "maa9r301_sc5s12_c", "maa9r26s_sc5s12_c",
      "maa9v28s_sc5s12_c", "maa9v07s_sc5s12_c")
  )
  waves <- c("_w1", "_w12")
  resp <- list(
    data.frame(ID_t = 1:100,
               mas1q02s_c = rep(0, 100),
               dummy = rep(0, 100)),
    data.frame(ID_t = 1:100,
               mas1v032_sc5s12_c = rep(0, 100),
               maa9d09s_sc5s12_c = rep(0, 100),
               maa9d13s_sc5s12_c = rep(0, 100),
               maa9r03s_sc5s12_c = rep(0, 100),
               mas1q02s_sc5s12_c = rep(0, 100),
               maa9v27s_sc5s12_c = rep(0, 100),
               maa9d20s_sc5s12_c = rep(0, 100),
               maa9r301_sc5s12_c = rep(0, 100),
               maa9r26s_sc5s12_c = rep(0, 100),
               maa9v28s_sc5s12_c = rep(0, 100),
               maa9v07s_sc5s12_c = rep(0, 100))
  )
  
  result <- list(
    resp = resp,
    Q = list(
      matrix(c(0.5, 1), nrow = 2, ncol = 1),
      matrix(c(rep(0.5, 9), 1, 1), nrow = 11, ncol = 1)
    )
  )
  test <- prepare_resp_q_longitudinal(PCM = list(TRUE, TRUE), resp, items,
                                      waves, SC, domain)
  expect_equal(test, result)

  result$Q <- list(matrix(1, nrow = 2, ncol = 1),
                   matrix(1, nrow = 11, ncol = 1))
  test <- prepare_resp_q_longitudinal(PCM = list(FALSE, FALSE), resp, items,
                                      waves, SC, domain)
  expect_equal(test, result)
})


test_that("prepare_resp_q_longitudinal: exception for SC4 science", {
  waves <- c("_w1", "_w5")
  resp <- list(
    data.frame(ID_t = 1:100,
               scg9012s_c = rep(0, 100),
               scg9052s_c = rep(0, 100),
               scg9611s_c = rep(0, 100),
               scg9061s_c = rep(0, 100),
               scg9083s_c = rep(0, 100),
               scg9042s_c = rep(0, 100),
               scg9043s_c = rep(0, 100),
               scg9651s_c = rep(0, 100),
               scg9621s_c = rep(0, 100)),
    data.frame(ID_t = 1:100,
               scg11083s_c = rep(0, 100),
               scg11032s_c = rep(0, 100),
               scg11652s_c = rep(0, 100),
               scg11602s_c = rep(0, 100),
               scg11123s_c = rep(0, 100),
               scs5131s_sc4g11_c = rep(0, 100))
  )
  items <- list(
    c("scg9012s_c", "scg9052s_c", "scg9611s_c", "scg9061s_c", "scg9083s_c",
      "scg9042s_c", "scg9043s_c", "scg9651s_c", "scg9621s_c"),
    c("scg11083s_c", "scg11032s_c", "scg11652s_c", "scg11602s_c",
      "scg11123s_c", "scs5131s_sc4g11_c")
  )
  
  result <- list(
    resp = resp,
    Q = list(
      matrix(c(rep(2/3, 2), rep(0.5, 7)), nrow = 9, ncol = 1),
      matrix(c(rep(0.5, 6)), nrow = 6, ncol = 1)
    )
  )
  test <- prepare_resp_q_longitudinal(PCM = list(TRUE, TRUE), resp, items,
                                      waves, SC = "SC4", domain = "SC")
  expect_equal(names(test), c("resp", "Q"))
  expect_equal(test, result)

  result$Q <- list(matrix(1, nrow = 9, ncol = 1), matrix(1, nrow = 6, ncol = 1))
  test <- prepare_resp_q_longitudinal(PCM = list(FALSE, FALSE), resp, items,
                                      waves, SC = "SC4", domain = "SC")
  expect_equal(names(test), c("resp", "Q"))
  expect_equal(test, result)
})

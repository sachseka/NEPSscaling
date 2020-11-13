context("prepare_resp_b_cross")

test_that("prepare_resp_b_cross: general case, e.g., SC5/MA w1", {
  
  waves <- "_w1"
  
  resp <- data.frame(ID_t = 1:100,
                     mas1q02s_c = rep(0:4, 20),
                     dummy = rep(0:3, 25))
  items <- c("mas1q02s_c", "dummy")

  test <- prepare_resp_b_cross(resp, items, waves, SC = "SC5", domain = "MA")
  expect_equal(names(test), c("resp", "B"))

  resp[["mas1q02s_c"]][resp[["mas1q02s_c"]] %in% c(1, 2)] <- 0
  resp[["mas1q02s_c"]][resp[["mas1q02s_c"]] == 3] <- 1
  resp[["mas1q02s_c"]][resp[["mas1q02s_c"]] == 4] <- 2
  result <- list(
    resp = resp,
    B = array(data = matrix(c(0, 0.5, 1, 0,
                              0, 1, 2, 3), nrow = 2, ncol = 4, byrow = T),
              dim = c(2, 4, 1),
              dimnames = list(
                items,
                paste0("Cat", 0:3),
                "Dim01"
              )) # 2 items, 4 categories, 1 dimension
  )
  expect_equal(test, result)
})


test_that("prepare_resp_b_cross: exception for SC4 science", {
  
  waves <- "_w1"
  
  resp <- data.frame(ID_t = 1:100,
                     scg9012s_c = rep(0:4, 20),
                     scg9052s_c = rep(0:4, 20),
                     scg9611s_c = rep(0:4, 20),
                     scg9061s_c = rep(0:4, 20),
                     scg9083s_c = rep(0:4, 20),
                     scg9042s_c = rep(0:4, 20),
                     scg9043s_c = rep(0:4, 20),
                     scg9651s_c = rep(0:4, 20),
                     scg9621s_c = rep(0:4, 20))
  items <- c("scg9012s_c", "scg9052s_c", "scg9611s_c", "scg9061s_c", "scg9083s_c",
             "scg9042s_c", "scg9043s_c", "scg9651s_c", "scg9621s_c")

  test <- prepare_resp_b_cross(resp, items, waves, SC = "SC4", domain = "SC")
  expect_equal(names(test), c("resp", "B"))

  resp[["scg9012s_c"]][resp[["scg9012s_c"]] == 1] <- 0
  resp[["scg9012s_c"]][resp[["scg9012s_c"]] == 2] <- 1
  resp[["scg9012s_c"]][resp[["scg9012s_c"]] == 3] <- 2
  resp[["scg9012s_c"]][resp[["scg9012s_c"]] == 4] <- 3

  resp[["scg9052s_c"]][resp[["scg9052s_c"]] == 1] <- 0
  resp[["scg9052s_c"]][resp[["scg9052s_c"]] == 2] <- 1
  resp[["scg9052s_c"]][resp[["scg9052s_c"]] == 3] <- 2
  resp[["scg9052s_c"]][resp[["scg9052s_c"]] == 4] <- 3

  resp[["scg9042s_c"]][resp[["scg9042s_c"]] == 1] <- 0
  resp[["scg9042s_c"]][resp[["scg9042s_c"]] == 2] <- 1
  resp[["scg9042s_c"]][resp[["scg9042s_c"]] == 3] <- 2
  resp[["scg9042s_c"]][resp[["scg9042s_c"]] == 4] <- 3
  result <- list(
    resp = resp,
    B = array(data = matrix(c(c(0, 1, 2, 3, 0) * 2/3,
                              c(0, 1, 2, 3, 0) * 2/3,
                              rep(c(0, 1, 2, 3, 4) * 0.5, 3),
                              c(0, 1, 2, 3, 0) * 0.5,
                              rep(c(0, 1, 2, 3, 4) * 0.5, 3)),
                            nrow = 9, ncol = 5, byrow = T),
              dim = c(9, 5, 1),
              dimnames = list(
                items,
                paste0("Cat", 0:4),
                "Dim01"
              )) # 9 items, 5 categories, 1 dimension
  )
  expect_equal(test, result)
})

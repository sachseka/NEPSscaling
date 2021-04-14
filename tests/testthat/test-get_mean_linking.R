context("get_mean_linking")

test_that("exception: means for linking in SC6/RE", {

  eap <- list(
    data.frame(ID_t = 1:100,
               eap_w3 = 1:100,
               eap_w5 = 1:100,
               eap_w9 = 1:100),
    data.frame(ID_t = 1:100,
               eap_w3 = 1:100,
               eap_w5 = 1:100,
               eap_w9 = 1:100)
  )
  wle1 <- data.frame(ID_t = 1:100,
                     wle_w3 = 1:100,
                     wle_w5 = 1:100,
                     wle_w9 = 1:100)
  pv <- replicate(3, data.frame(ID_t = 1:100,
                                PV_w3 = 1:100,
                                PV_w5 = 1:100,
                                PV_w9 = 1:100), simplify = FALSE)
  long_IDs <- list(
    w3 = 1:50,
    w5 = 51:100
  )
  waves <- c("_w3", "_w5")
  domain <- "RE"

  test <- get_mean_linking(eap, wle1, NULL, pv, long_IDs, NULL, NULL,
                           "SC6", domain)
  expect_equal(test$eap, list(c(mean(1:50), mean(51:100), mean(1:50), mean(51:100)),
                              c(mean(1:50), mean(51:100), mean(1:50), mean(51:100))))
  expect_equal(test$wle, c(mean(1:50), mean(51:100), mean(1:50), mean(51:100)))
  expect_equal(test$pv, replicate(3,
                                  c(mean(1:50), mean(51:100), mean(1:50),
                                    mean(51:100)), simplify = FALSE))
})

test_that("regular case: means for linking", {

  eap <- list(
    data.frame(ID_t = 1:100,
               eap_w3 = 1:100,
               eap_w5 = 1:100,
               eap_w9 = 1:100),
    data.frame(ID_t = 1:100,
               eap_w3 = 1:100,
               eap_w5 = 1:100,
               eap_w9 = 1:100)
  )
  wle1 <- data.frame(ID_t = 1:100,
                     wle_w3 = 1:100,
                     wle_w5 = 1:100,
                     wle_w9 = 1:100)
  wle2 <- data.frame(ID_t = 1:100,
                     wle_w3 = 1:100)
  pv <- replicate(3, data.frame(ID_t = 1:100,
                                PV_w3 = 1:100,
                                PV_w5 = 1:100,
                                PV_w9 = 1:100), simplify = FALSE)
  long_IDs <- list(
    w3 = 1:50,
    w5 = 51:100
  )
  waves <- c("_w3", "_w5")
  w <- 2L
  SC <- "SC1"
  domain <- "RE"

  test <- get_mean_linking(eap, wle1, wle2, pv, long_IDs[[1]], waves, w,
                           SC, domain)
  expect_equal(test$eap, list(c(mean(1:50), mean(1:50)), c(mean(1:50), mean(1:50))))
  expect_equal(test$wle, c(mean(1:50), mean(1:50)))
  expect_equal(test$pv, replicate(3,
                                  c(mean(1:50), mean(1:50)),
                                  simplify = FALSE))
})

test_that("means for linking: wle is NULL", {

  eap <- list(data.frame(ID_t = 1:100,
                         eap_w3 = 1:100,
                         eap_w5 = 1:100,
                         eap_w9 = 1:100))
  pv <- replicate(3, data.frame(ID_t = 1:100,
                                PV_w3 = 1:100,
                                PV_w5 = 1:100,
                                PV_w9 = 1:100), simplify = FALSE)
  long_IDs <- list(
    w3 = 1:50,
    w5 = 51:100
  )
  waves <- c("_w3", "_w5")
  w <- 2L
  SC <- "SC1"
  domain <- "RE"

  test <- get_mean_linking(eap, wle1 = NULL, wle2 = NULL, pv, long_IDs, waves, w,
                           SC, domain)
  expect_equal(test$wle, NULL)
})

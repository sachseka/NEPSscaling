context("set_not_enough_valid_resp_NA_long")

min_valid <- 3
npv <- 3
waves <- c("_w1", "_w2")
datalist <- replicate(npv, data.frame(ID_t = 1:10,
                                      PV_w1 = rnorm(10),
                                      PV_w2 = rnorm(10)), simplify = FALSE)
eap <- data.frame(ID_t = 1:10,
                  eap_w1 = rnorm(1:10), se_w1 = rnorm(1:10, sd = 0.01),
                  eap_w2 = rnorm(1:10), se_w2 = rnorm(1:10, sd = 0.01))
wle <- data.frame(ID_t = 1:10,
                  wle_w1 = rnorm(1:10), se_w1 = rnorm(1:10, sd = 0.01),
                  wle_w2 = rnorm(1:10), se_w2 = rnorm(1:10, sd = 0.01))
valid_responses_per_person <- data.frame(ID_t = 1:10,
                                         valid_w1 = 0:9, valid_w2 = 1:10)

test_that("correct structure and selection of too few valid", {
  test <- set_not_enough_valid_resp_NA_long(npv, waves, eap, wle, min_valid,
                                            valid_responses_per_person,
                                            datalist)
  expect_equal(names(test), c("datalist", "eap", "wle"))
  expect_equal(sum(is.na(test$eap$ID_t)), 0)
  expect_equal(sum(is.na(test$eap$eap_w1)), 3)
  expect_equal(sum(is.na(test$eap$se_w1)), 3)
  expect_equal(sum(is.na(test$eap$eap_w2)), 2)
  expect_equal(sum(is.na(test$eap$se_w2)), 2)
  expect_equal(sum(is.na(test$wle$ID_t)), 0)
  expect_equal(sum(is.na(test$wle$wle_w1)), 3)
  expect_equal(sum(is.na(test$wle$se_w1)), 3)
  expect_equal(sum(is.na(test$wle$wle_w2)), 2)
  expect_equal(sum(is.na(test$wle$se_w2)), 2)
  expect_equal(sum(is.na(test$datalist[[1]]$PV_w1)), 3)
  expect_equal(sum(is.na(test$datalist[[2]]$PV_w1)), 3)
  expect_equal(sum(is.na(test$datalist[[3]]$PV_w1)), 3)
  expect_equal(sum(is.na(test$datalist[[1]]$PV_w2)), 2)
  expect_equal(sum(is.na(test$datalist[[2]]$PV_w2)), 2)
  expect_equal(sum(is.na(test$datalist[[3]]$PV_w2)), 2)
})

rm(min_valid, npv, datalist, waves, eap, wle, valid_responses_per_person)

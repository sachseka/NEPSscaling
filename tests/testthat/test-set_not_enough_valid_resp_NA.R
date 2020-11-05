context("set_not_enough_valid_resp_NA")

min_valid <- 3
npv <- 3
pv <- replicate(npv, data.frame(ID_t = 1:10, PV = rnorm(10)), simplify = FALSE)
eap <- data.frame(ID_t = 1:10, eap = rnorm(1:10), se = rnorm(1:10, sd = 0.01))
wle <- data.frame(ID_t = 1:10, wle = rnorm(1:10), se = rnorm(1:10, sd = 0.01))
valid_responses_per_person <- data.frame(ID_t = 1:10, valid = 0:9)

test_that("correct structure and selection of too few valid", {
  test <- set_not_enough_valid_resp_NA(pv, eap, wle, valid_responses_per_person,
                                       min_valid, npv)
  expect_equal(names(test), c("pv", "eap", "wle"))
  expect_equal(sum(is.na(test$eap$ID_t)), 0)
  expect_equal(sum(is.na(test$eap$eap)), 3)
  expect_equal(sum(is.na(test$eap$se)), 3)
  expect_equal(sum(is.na(test$wle$ID_t)), 0)
  expect_equal(sum(is.na(test$wle$wle)), 3)
  expect_equal(sum(is.na(test$wle$se)), 3)
  expect_equal(sum(is.na(test$pv[[1]]$PV)), 3)
  expect_equal(sum(is.na(test$pv[[2]]$PV)), 3)
  expect_equal(sum(is.na(test$pv[[3]]$PV)), 3)
})

rm(min_valid, npv, pv, eap, wle, valid_responses_per_person)

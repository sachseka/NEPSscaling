context("extract_correct_number_of_pvs")

bgdata <- NULL
control <- list(ML = list(nmi = 2))
npv <- 2
pvs <- list( # list of imputations containing data.frames for each estimated pv
  replicate(5, data.frame(ID_t = 1:10, PV = 21:30), simplify = FALSE),
  replicate(5, data.frame(ID_t = 1:10, PV = 11:20), simplify = FALSE)
)

test_that("test correct if bgdata = NULL", {
  test <- extract_correct_number_of_pvs(bgdata, control, npv, pvs)
  expect_equal(length(test), npv)
  expect_equal(test[[npv]][["PV"]], 21:30)
})

bgdata <- data.frame(ID_t = 1:10, covariate = c(rnorm(9), NA))
test_that("test correct if bgdata != NULL", {
  test <- extract_correct_number_of_pvs(bgdata, control, npv, pvs)
  expect_equal(length(test), npv)
})

rm(bgdata, control, npv, pvs)

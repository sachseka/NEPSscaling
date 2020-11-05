context("impute_missing_data")

bgdata <- data.frame(
  ID_t = 1:100,
  var1 = 1:100
)
verbose <- FALSE
control <- list(
  EAP = FALSE, WLE = FALSE,
  ML = list(
    nmi = 10L, ntheta = 2000,
    normal.approx = FALSE,
    samp.regr = FALSE,
    theta.model = FALSE,
    np.adj = 8, na.grid = 5,
    minbucket = 5,
    cp = 0.0001
  )
)

test_that("imputation of already complete bgdata", {
  test <- impute_missing_data(bgdata, verbose, control)
  expect_equal(names(test), c("imp", "frmY", "bgdata", "loggedEvents"))
  expect_equal(test$imp, NULL)
  expect_equal(test$frmY, as.formula("~ var1"))
  expect_equal(test$bgdata, bgdata)
  expect_equal(test$loggedEvents, NULL)
})

bgdata$var1[1:10] <- NA

test_that("imputation of bgdata with missings", {
  test <- impute_missing_data(bgdata, verbose, control)
  expect_equal(names(test), c("imp", "frmY", "bgdata", "loggedEvents"))
  expect_equal(length(test$imp), control$ML$nmi)
  expect_equal(test$frmY, NULL)
  expect_equal(test$bgdata, bgdata)
  expect_equal(test$loggedEvents, NULL)
  expect_equal(sum(sapply(test$imp, function(x) any(is.na(x)))), 0) # no more missings
})

test_that("imputation for bgdata = NULL", {
  test <- impute_missing_data(NULL, verbose, control)
  expect_equal(names(test), c("imp", "frmY", "bgdata", "loggedEvents"))
  expect_equal(test$imp, NULL)
  expect_equal(test$frmY, NULL)
  expect_equal(test$bgdata, NULL)
  expect_equal(test$loggedEvents, NULL)
})

rm(bgdata, verbose, control)

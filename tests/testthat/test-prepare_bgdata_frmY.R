context("prepare_bgdata_frmY")

imp <- list(
  data.frame(ID_t = 1:100,
             var1 = as.factor(rep(1:4, 25)),
             var2 = as.character(rep(1:4, 25)),
             var3 = rep(1:4, 25)),
  data.frame(ID_t = 1:100,
             var1 = as.factor(rep(5:8, 25)),
             var2 = as.character(rep(5:8, 25)),
             var3 = rep(5:8, 25))
)

test_that("with imputed bgdata", {
  test <- prepare_bgdata_frmY(imp, 1L, NULL)
  expect_equal(names(test), c("bgdatacom", "frmY"))
  expect_equal(test[[1]], data.frame(ID_t = 1:100,
                                     var1 = rep(1:4, 25),
                                     var2 = rep(1:4, 25),
                                     var3 = rep(1:4, 25)))
  expect_equal(test[[2]], as.formula("~ var1 + var2 + var3"))

  test <- prepare_bgdata_frmY(imp, 2L, NULL)
  expect_equal(names(test), c("bgdatacom", "frmY"))
  expect_equal(test[[1]], data.frame(ID_t = 1:100,
                                     var1 = rep(5:8, 25),
                                     var2 = rep(5:8, 25),
                                     var3 = rep(5:8, 25)))
  expect_equal(test[[2]], as.formula("~ var1 + var2 + var3"))
})

test_that("without imputed bgdata", {
  test <- prepare_bgdata_frmY(NULL, 1L, NULL)
  expect_equal(names(test), c("bgdatacom", "frmY"))
  expect_equal(test[[1]], NULL)
  expect_equal(test[[2]], NULL)
})

rm(imp)

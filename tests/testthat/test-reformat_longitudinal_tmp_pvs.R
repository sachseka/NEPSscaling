context("reformat_longitudinal_tmp_pvs")

npv <- 3
pvs <- replicate(1, list(), simplify = FALSE) # replicated nmi times!
tmp_pvs <- list(
  list(
    data.frame(ID_t = 1:100,
               PV_w1 = 1:100),
    data.frame(ID_t = 1:100,
               PV_w1 = 1:100),
    data.frame(ID_t = 1:100,
               PV_w1 = 1:100)
  ),
  list(
    data.frame(ID_t = 1:100,
               PV_w2 = 1:100),
    data.frame(ID_t = 1:100,
               PV_w2 = 1:100),
    data.frame(ID_t = 1:100,
               PV_w2 = 1:100)
  )
)
bgdata <- data.frame(ID_t = 1:100)

result <- list(list( # the nested list is for the imputation (# 1 in this case)
  data.frame(ID_t = 1:100,
             PV_w1 = 1:100,
             PV_w2 = 1:100),
  data.frame(ID_t = 1:100,
             PV_w1 = 1:100,
             PV_w2 = 1:100),
  data.frame(ID_t = 1:100,
             PV_w1 = 1:100,
             PV_w2 = 1:100)
))

test_that("reformat tmp_pvs with and 'without' bgdata", {
  test <- reformat_longitudinal_tmp_pvs(npv, pvs, i = 1L, tmp_pvs, bgdata)
  expect_equal(test, result)

  result[[2]] <- result[[1]]
  test[[2]] <- list() # adding another imputation!
  test <- reformat_longitudinal_tmp_pvs(npv, pvs = test, i = 2L, tmp_pvs,
                                        bgdata)
  expect_equal(test, result)
})

rm(bgdata, pvs, result, tmp_pvs, npv)

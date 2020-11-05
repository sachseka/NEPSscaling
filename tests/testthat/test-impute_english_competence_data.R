context("impute_english_competence_data")

SC <- "SC3"
wave <- "w7"

resp <- data.frame(ID_t = diffMat[[SC]][[wave]][["ind_NA"]]$ID_t)
resp <- cbind(
  resp,
  as.data.frame(lapply(1:(ncol(diffMat[[SC]][[wave]][["ind_NA"]]) - 1),
                       function(x) {
                         rep(0, nrow(resp))})
                )
)
names(resp) <- names(diffMat[[SC]][[wave]][["ind_NA"]])
resp <- tibble::as_tibble(resp)

result <- diffMat[[SC]][[wave]][["diff"]]
for (i in 2:ncol(result)) {
  result[diffMat[[SC]][[wave]][["ind_NA"]][[i]], i] <- NA
}

test_that("impute english competence, general function", {
  test <- impute_english_competence_data(resp, SC, wave)
  expect_equal(test, result)
})

rm(resp, result, i, SC, wave)

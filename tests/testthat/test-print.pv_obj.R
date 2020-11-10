context("print.pv_obj")

# TODO: richtiges expect raussuchen; kein Test?

pv_obj <- list(
  SC = 1,
  domain = "domain",
  wave = c("all", "the", "waves", "belong", "to", "us"),
  n_testtakers = c(314, 666),
  EAP_rel = list(1, 2),
  type = "cross",
  regr_coeff = matrix(0, ncol = 2, nrow = 2),
  comp_time = list(
    initial_time = Sys.time(),
    estimation_time = Sys.time() - Sys.time(),
    total_comp_time = Sys.time() - Sys.time()
  )
)
rownames(pv_obj$regr_coeff) <- c("Intercept", "aVariable")
class(pv_obj) <- "pv_obj"

result <- paste0("Plausible Values Estimation with NEPSscaling\n",
                 "\nStarting Cohort:  1 \n",
                 "Domain:  domain \n",
                 "Wave(s):  all the waves belong to us \n",
                 "Test takers per wave:  314 666 \n",
                 "EAP reliability:  1 2 \n",
                 "\nVariables in background model:  aVariable \n",
                 "\nStarting time:  ", paste(pv_obj[["comp_time"]]$initial_time), " \n",
                 "Time for estimation:  ",
                     paste(round(pv_obj[["comp_time"]]$estimation_time, 1)), " ",
                     units(pv_obj[["comp_time"]]$estimation_time), " \\n",
                 "Total computation time:  ",
                     paste(round(pv_obj[["comp_time"]]$total_comp_time, 1)), " ",
                     units(pv_obj[["comp_time"]]$total_comp_time), " \\n")

test_that("print.pv_obj: cross-sectional", {
  expect_output(print.pv_obj(pv_obj), result)
})

pv_obj$type <- "long"
pv_obj$regr_coeff <- list(matrix(0, 2, 2), matrix(0, 2, 2))
rownames(pv_obj$regr_coeff[[1]]) <- rownames(pv_obj$regr_coeff[[2]]) <-
  c("Intercept", "aVariable")

test_that("print.pv_obj: longitudinal", {
  expect_output(print.pv_obj(pv_obj), result)
})

rm(pv_obj, result)

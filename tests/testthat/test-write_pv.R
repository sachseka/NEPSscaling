context("write_pv")

# needs: pv_obj, path, ext (SPSS, Stata, Mplus)
# test: number of files correct, Mplus content file correct
# clean up after tests: remove newly created path directory and files therein

test_that("write_pv's error messages", {
  skip(message = "write_pv: skipped due to side effects")
  pv_obj <- list()
  pv_obj$domain <- "MA"
  pv_obj$SC <- 1
  pv_obj$wave <- 1
  pv_obj$type <- "long"
  pv_obj$pv <-
    replicate(3, data.frame(
      ID_t = 1:100, covariate = rbinom(100, 1, 0.5), PV = rnorm(100)),
      simplify = F)
  class(pv_obj) <- "pv_obj"
  ext <- "SPSS"
  
  path <- "tests/tests/"
  dir.create(file.path(path))
  
  expect_error(write_pv(unclass(pv_obj), path, ext),
               "pv_obj must be of class 'pv_obj'.")
  expect_error(write_pv(pv_obj, path, ""))
  expect_error(
    write_pv(pv_obj, 1, ext),
    "Path must be a character string and end in '/'.")
  expect_error(
    write_pv(pv_obj, "a_path", ext),
    "Path must be a character string and end in '/'.")
  
  unlink("tests/tests", recursive = TRUE)
})

test_that("write_pv prints npv data files for Stata/SPSS", {
  skip(message = "write_pv: skipped due to side effects")
  pv_obj <- list()
  pv_obj$domain <- "MA"
  pv_obj$SC <- 1
  pv_obj$wave <- 1
  pv_obj$type <- "long"
  pv_obj$pv <-
    replicate(3, data.frame(
      ID_t = 1:100, covariate = rbinom(100, 1, 0.5), PV = rnorm(100)),
      simplify = F)
  class(pv_obj) <- "pv_obj"
  ext <- "SPSS"
  
  path <- "tests/tests/"
  dir.create(file.path(path))
  
  write_pv(pv_obj, path, "SPSS")
  expect_equal(length(list.files(path)), 3)
  write_pv(pv_obj, path, "Stata")
  expect_equal(length(list.files(path)), 6)
  
  unlink("tests/tests", recursive = TRUE)
})


test_that("write_pv prints npv data + 1 content files for Mplus", {
  skip(message = "write_pv: skipped due to side effects")
  pv_obj <- list()
  pv_obj$domain <- "MA"
  pv_obj$SC <- 1
  pv_obj$wave <- 1
  pv_obj$type <- "long"
  pv_obj$pv <-
    replicate(3, data.frame(
      ID_t = 1:100, covariate = rbinom(100, 1, 0.5), PV = rnorm(100)),
      simplify = F)
  class(pv_obj) <- "pv_obj"
  ext <- "SPSS"
  
  path <- "tests/tests/"
  dir.create(file.path(path))
  
  content_file <- "SC1_MA_w1_long_plausible_values_1.dat\nSC1_MA_w1_long_plausible_values_2.dat\nSC1_MA_w1_long_plausible_values_3.dat"
  write_pv(pv_obj, path, "Mplus")
  expect_equal(length(list.files(path)), 4)
  expect_equal(
    paste(readLines(paste0(path, "SC1_MA_w1_long_content_file.dat")),
          collapse = "\n"),
    content_file)
  
  unlink("tests/tests", recursive = TRUE)
})

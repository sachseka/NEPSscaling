context("import_data")

test_that("import_data: SPSS", {
  skip(message = "import_data: skipped because of side effects")
  data <- result <- data.frame(ID_t = 1:100,
                               var1 = c(1:90, rep(-94, 10)))
  data$var1 <- haven::labelled_spss(
    data$var1,
    label = "Label",
    labels = c("omitted" = -97, "not valid" = -95,
               "not reached" = -94, "test aborted" = -91,
               "unspecific missing" = -90, "not administered" = -54,
               "Angabe zurückgesetzt" = -21)
  )

  path <- "tests/tests/"
  dir.create(file.path(path))

  haven::write_sav(data, "tests/tests/test_data.sav")

  test <- import_data(filetype = "sav", filepath = "tests/tests/test_data.sav",
                      error_msg = "message", school = FALSE)
  expect_equivalent(test, result)

  result[result < 0] <- NA
  test <- import_data(filetype = "sav", filepath = "tests/tests/test_data.sav",
                      error_msg = "message", school = TRUE)
  expect_equivalent(test, result)

  unlink("tests/tests", recursive = TRUE)

  expect_error(import_data(filetype = "sav", filepath = "no_path",
                           error_msg = "message", school = TRUE),
               "message")

  expect_error(import_data(filetype = "savt", filepath = "no_path",
                           error_msg = "message", school = TRUE),
               "message")
})


test_that("import_data: Stata", {
  skip(message = "import_data: skipped because of side effects")
  data <- result <- data.frame(ID_t = 1:100,
                               var1 = c(1:90, rep(-94, 10)))
  data$var1 <- haven::labelled_spss(
    data$var1,
    label = "Label",
    labels = c("omitted" = -97, "not valid" = -95,
               "not reached" = -94, "test aborted" = -91,
               "unspecific missing" = -90, "not administered" = -54,
               "Angabe zurückgesetzt" = -21)
  )

  path <- "tests/tests/"
  dir.create(file.path(path))

  haven::write_dta(data, "tests/tests/test_data.dta")

  test <- import_data(filetype = "dta", filepath = "tests/tests/test_data.dta",
                      error_msg = "message", school = FALSE)
  expect_equivalent(test, result)

  result[result < 0] <- NA
  test <- import_data(filetype = "dta", filepath = "tests/tests/test_data.dta",
                      error_msg = "message", school = TRUE)
  expect_equivalent(test, result)

  unlink("tests/tests", recursive = TRUE)

  expect_error(import_data(filetype = "dta", filepath = "no_path",
                           error_msg = "message", school = TRUE), "message")

  expect_error(import_data(filetype = "savt", filepath = "no_path",
                           error_msg = "message", school = TRUE), "message")
})

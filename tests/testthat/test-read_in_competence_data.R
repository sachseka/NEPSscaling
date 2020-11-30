context("read_in_competence_data")

test_that("read_in_competence_data: SC1 - Bayley Scales", {
  skip(message = "read_in_competence_data: skipped because of side effects")
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

  SC <- "SC3"
  domain <- "RE"

  path <- "tests/tests/"
  dir.create(file.path(path))

  haven::write_sav(data, "tests/tests/xDirectMeasures.sav")

  test <- read_in_competence_data(path, SC = "SC1", domain = "CD")
  expect_equal(test, result)

  unlink("tests/tests", recursive = TRUE)
})


test_that("read_in_competence_data: SC5 - Business Administration", {
  skip(message = "read_in_competence_data: skipped because of side effects")
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

  SC <- "SC3"
  domain <- "RE"

  path <- "tests/tests/"
  dir.create(file.path(path))

  haven::write_sav(data, "tests/tests/xEcoCAPI.sav")

  test <- read_in_competence_data(path, SC = "SC5", domain = "BA")
  expect_equal(test, result)

  unlink("tests/tests", recursive = TRUE)
})


test_that("read_in_competence_data: general case (SPSS)", {
  skip(message = "read_in_competence_data: skipped because of side effects")
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

  SC <- "SC3"
  domain <- "RE"

  path <- "tests/tests/"
  dir.create(file.path(path))

  haven::write_sav(data, "tests/tests/xTargetCompetencies.sav")

  test <- read_in_competence_data(path, SC, domain)
  expect_equal(test, result)

  unlink("tests/tests", recursive = TRUE)
})


test_that("read_in_competence_data: general case (SPSS) w/ wrong path", {
  error_msg <- paste0(
    "* Path 'bla' may not lead to competence files.\n",
    "* File format: '' might be wrong"
  )
  expect_error(read_in_competence_data(path = "bla", SC, domain),
               cat(error_msg))
})


test_that("read_in_competence_data: general case (Stata)", {
  skip(message = "read_in_competence_data: skipped because of side effects")
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

  SC <- "SC3"
  domain <- "RE"

  path <- "tests/tests/"
  dir.create(file.path(path))

  haven::write_dta(data, "tests/tests/xTargetCompetencies.dta")

  test <- read_in_competence_data(path, SC, domain)
  expect_equal(test, result)

  unlink("tests/tests", recursive = TRUE)
})


test_that("read_in_competence_data: general case (Stata) w/ wrong path", {
  error_msg <- paste0(
    "* Path 'bla' may not lead to competence files.\n",
    "* File format: '' might be wrong"
  )
  expect_error(read_in_competence_data(path = "bla", SC, domain),
               cat(error_msg))
})


test_that("read_in_competence_data: wrong file format", {
  skip(message = "read_in_competence_data: skipped because of side effects")
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

  SC <- "SC3"
  domain <- "RE"

  path <- "tests/tests/"
  dir.create(file.path(path))

  write.csv(data, file = "tests/tests/xTargetCompetencies.csv")

  error_msg <- paste0(
    "* Path 'tests/testsxTargetCompetencies.csv' may not lead to competence files.\n",
    "* File format: 'csv' might be wrong"
  )
  expect_error(read_in_competence_data(path = "bla", SC, domain),
               cat(error_msg))

  unlink("tests/tests", recursive = TRUE)
})

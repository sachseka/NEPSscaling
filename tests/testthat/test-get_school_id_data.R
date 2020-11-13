context("get_school_id_data")

test_that("get_school_id_data: sav", {
  skip("get_school_id_data: skipped due to side effects")

  data <- data.frame(ID_t = rep(1:25, each = 4),
                     ID_i = sample(1:10, 100, replace = TRUE),
                     wave = rep(1:4, 25))

  result <- data.frame(ID_t = 1:25,
                       school_w1 = data$ID_i[data$wave == 1],
                       school_w2 = data$ID_i[data$wave == 2],
                       school_w3 = data$ID_i[data$wave == 3],
                       school_w4 = data$ID_i[data$wave == 4])

  path <- "tests/tests/"
  dir.create(file.path(path))

  haven::write_sav(data, "tests/tests/CohortProfile.sav")

  test <- get_school_id_data(path)
  expect_equivalent(test, result)

  unlink("tests/tests", recursive = TRUE)
})


test_that("get_school_id_data: dta", {
  skip("get_school_id_data: skipped due to side effects")

  data <- data.frame(ID_t = rep(1:25, each = 4),
                     ID_i = sample(1:10, 100, replace = TRUE),
                     wave = rep(1:4, 25))

  result <- data.frame(ID_t = 1:25,
                       school_w1 = data$ID_i[data$wave == 1],
                       school_w2 = data$ID_i[data$wave == 2],
                       school_w3 = data$ID_i[data$wave == 3],
                       school_w4 = data$ID_i[data$wave == 4])

  path <- "tests/tests/"
  dir.create(file.path(path))

  haven::write_dta(data, "tests/tests/CohortProfile.dta")

  test <- get_school_id_data(path)
  expect_equivalent(test, result)

  unlink("tests/tests", recursive = TRUE)
})


test_that("get_school_id_data: error", {
  skip("get_school_id_data: skipped due to side effects")
  result <- paste0(
    "* Path '.' may not contain CohortProfile.\n",
    "* File format: '' might be wrong"
  )
  expect_error(get_school_id_data(path = "."), result)
})

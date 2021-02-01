context("determine_file_path")

test_that("determine_file_path: school = FALSE", {
  skip(message = "determine_file_path: skipped because of side effects")
  path <- "tests/tests/"
  dir.create(file.path(path))
  sink(file = "xEcoCAPI.txt")
  "test"
  sink()
  sink(file = "xDirectMeasures.txt")
  "test"
  sink()
  sink(file = "xTargetCompetencies.txt")
  "test"
  sink()


  expect_equal(
    determine_file_path(path = path, SC = "SC5", domain = "BA", school = FALSE),
    "tests/tests/xEcoCAPI.txt"
  )

  expect_equal(
    determine_file_path(path = path, SC = "SC1", domain = "CD", school = FALSE),
    "tests/tests/xDirectMeasures.txt"
  )

  expect_equal(
    determine_file_path(path = path, SC = "SC3", domain = "BA", school = FALSE),
    "tests/tests/xTargetCompetencies.txt"
  )

  expect_equal(
    determine_file_path(path = path, SC = "SC3", domain = "CD", school = FALSE),
    "tests/tests/xTargetCompetencies.txt"
  )

  expect_equal(
    determine_file_path(path = path, SC = "SC5", domain = "RE", school = FALSE),
    "tests/tests/xTargetCompetencies.txt"
  )

  expect_equal(
    determine_file_path(path = path, SC = "SC1", domain = "RE", school = FALSE),
    tests/tests/"xTargetCompetencies.txt"
  )
  unlink("tests/tests", recursive = TRUE)
})

test_that("determine_file_path: school = TRUE", {
  skip(message = "determine_file_path: skipped because of side effects")
  path <- "tests/tests/"
  dir.create(file.path(path))
  sink(file = "CohortProfile.txt")
  "test"
  sink()
  expect_equal(
    determine_file_path(path = path, SC = NULL, domain = NULL, school = TRUE),
    "tests/tests/CohortProfile.txt"
  )
  unlink("tests/tests", recursive = TRUE)
})

context("add_contextual_info")


test_that("add_contextual_info: cross-sectional", {
  skip(message = "add_contextual_info: skipped due to side effects")

  cp <- data.frame(ID_t = rep(1:20, each = 4),
                   ID_i = rep(1:5, each = 4),
                   wave = rep(1:4, 20))

  path <- "tests/tests/"
  dir.create(file.path(path))

  haven::write_sav(cp, "tests/tests/CohortProfile.sav")

  SC <- "SC4"
  domain <- "MA"
  waves <- c("_w1", "_w7") # cross and long cases!
  bgdata <- data.frame(ID_t = 1:20)
  data <- data.frame(ID_t = 1:20,
                     mag9_sc1u = rep(1:5, 4), mag12_sc1u = rep(1:5, 4),
                     mag9_sc1 = rep(1:5, 4))

  result <- data.frame(
    ID_t = 1:20,
    mag9_sc1_schavg = rep(1:5, 4)
  )
  test <- add_contextual_info(path, SC, domain, waves = "_w1", bgdata, data)
  expect_equal(test, result)

  unlink("tests/tests", recursive = TRUE)
})



test_that("add_contextual_info: longitudinal", {
  skip(message = "add_contextual_info: skipped due to side effects")

  cp <- data.frame(ID_t = rep(1:20, each = 4),
                   ID_i = rep(1:5, each = 4),
                   wave = rep(1:4, 20))

  path <- "tests/tests/"
  dir.create(file.path(path))

  haven::write_sav(cp, "tests/tests/CohortProfile.sav")

  SC <- "SC4"
  domain <- "MA"
  waves <- c("_w1", "_w7") # cross and long cases!
  bgdata <- data.frame(ID_t = 1:20)
  data <- data.frame(ID_t = 1:20,
                     mag9_sc1u = rep(1:5, 4), mag12_sc1u = rep(1:5, 4),
                     mag9_sc1 = rep(1:5, 4))

  result <- data.frame(
    ID_t = 1:20,
    mag9_sc1u_schavg = rep(1:5, 4),
    mag12_sc1u_schavg = rep(1:5, 4)
  )
  test <- add_contextual_info(path, SC, domain, waves, bgdata, data)
  expect_equal(test, result)

  unlink("tests/tests", recursive = TRUE)
})

context("pre_process_background_data")

test_that("pre_process_background_data: bgdata = NULL", {
  data <- data.frame(ID_t = 100:1)
  items_not_reached <- data.frame(ID_t = 1:100, not_reached = 1)
  min_valid <- 3
  include_nr <- TRUE
  
  # bgdata = NULL
  result <- list(
    data = data,
    bgdata = NULL,
    ID_t = data.frame(ID_t = 100:1)
  )
  test <- pre_process_background_data(bgdata = NULL, data, include_nr = FALSE,
                                      items_not_reached, min_valid)
  expect_equal(names(test), c("data", "bgdata", "ID_t"))
  expect_equal(test, result)

  result <- list(
    data = data,
    bgdata = data.frame(ID_t = 100:1, not_reached = 1),
    ID_t = data.frame(ID_t = 100:1)
  )
  test <- pre_process_background_data(bgdata = NULL, data, include_nr,
                                      items_not_reached, min_valid)
  expect_equal(names(test), c("data", "bgdata", "ID_t"))
  expect_equal(test, result)
})


test_that("pre_process_background_data: min_valid > 0", {
  bgdata <- data.frame(ID_t = 1:100, var = 1)
  data <- data.frame(ID_t = 100:1)
  include_nr <- TRUE
  items_not_reached <- data.frame(ID_t = 1:100, not_reached = 1)
  min_valid <- 3
  
  # bgdata = data
  result <- list(
    data = data,
    bgdata = bgdata,
    ID_t = data.frame(ID_t = 100:1)
  )
  test <- pre_process_background_data(bgdata, data, include_nr = FALSE,
                                      items_not_reached, min_valid)
  expect_equal(names(test), c("data", "bgdata", "ID_t"))
  expect_equal(test, result)

  result <- list(
    data = data,
    bgdata = data.frame(ID_t = 1:100, var = 1, not_reached = 1),
    ID_t = data.frame(ID_t = 100:1)
  )
  test <- pre_process_background_data(bgdata, data, include_nr,
                                      items_not_reached, min_valid)
  expect_equal(names(test), c("data", "bgdata", "ID_t"))
  expect_equal(test, result)

  # data > bgdata
  data <- data.frame(ID_t = 110:1, dummy = 1)
  items_not_reached <- data.frame(ID_t = 1:110, not_reached = 1)
  result <- list(
    data = data,
    bgdata = data.frame(ID_t = 1:110,
                        var = c(rep(1, 100), rep(NA, 10)),
                        not_reached = 1),
    ID_t = data.frame(ID_t = 110:1)
  )
  test <- pre_process_background_data(bgdata, data, include_nr,
                                      items_not_reached, min_valid)
  expect_equal(names(test), c("data", "bgdata", "ID_t"))
  expect_equal(test, result)

  # data < bgdata
  data <- data.frame(ID_t = 90:1, dummy = 1)
  items_not_reached <- data.frame(ID_t = 1:90, not_reached = 1)
  result <- list(
    data = data,
    bgdata = data.frame(ID_t = 1:90,
                        var = 1,
                        not_reached = 1),
    ID_t = data.frame(ID_t = 90:1)
  )
  test <- pre_process_background_data(bgdata, data, include_nr,
                                      items_not_reached, min_valid)
  expect_equal(names(test), c("data", "bgdata", "ID_t"))
  expect_equal(test, result)
})

test_that("pre_process_background_data: min_valid  = 0", {
  include_nr <- TRUE
  items_not_reached <- data.frame(ID_t = 1:100, not_reached = 1)
  min_valid <- 3
  bgdata <- data.frame(ID_t = 1:100, var = 1)
  data <- data.frame(ID_t = 90:1, dummy = 1)
  
  # data < bgdata
  result <- list(
    data = data.frame(ID_t = 1:100, dummy = c(rep(1, 90), rep(NA, 10))),
    bgdata = data.frame(ID_t = 1:100, var = 1, not_reached = 1),
    ID_t = data.frame(ID_t = 1:100)
  )
  test <- pre_process_background_data(bgdata, data, include_nr,
                                      items_not_reached, min_valid = 0)
  expect_equal(names(test), c("data", "bgdata", "ID_t"))
  expect_equivalent(test, result)

  # data = bgdata
  data <- data.frame(ID_t = 100:1, dummy = 1)
  result <- list(
    data = data.frame(ID_t = 1:100, dummy = 1),
    bgdata = data.frame(ID_t = 1:100, var = 1, not_reached = 1),
    ID_t = data.frame(ID_t = 1:100)
  )
  test <- pre_process_background_data(bgdata, data, include_nr,
                                      items_not_reached, min_valid = 0)
  expect_equal(names(test), c("data", "bgdata", "ID_t"))
  expect_equivalent(test, result)

  # data > bgdata
  data <- data.frame(ID_t = 110:1, dummy = 1)
  items_not_reached <- data.frame(ID_t = 1:110, not_reached = 1)
  result <- list(
    data = data.frame(ID_t = 1:110, dummy = 1),
    bgdata = data.frame(ID_t = 1:110,
                        var = c(rep(1, 100), rep(NA, 10)),
                        not_reached = 1),
    ID_t = data.frame(ID_t = 1:110)
  )
  test <- pre_process_background_data(bgdata, data, include_nr,
                                      items_not_reached, min_valid = 0)
  expect_equal(names(test), c("data", "bgdata", "ID_t"))
  expect_equivalent(test, result)
})

test_that("pre_process_background_data: exclude completely missing variables", {
  bgdata <- data.frame(ID_t = 1:100, var1 = 1, var2 = NA)
  data <- data.frame(ID_t = 100:1)
  include_nr <- TRUE
  items_not_reached <- data.frame(ID_t = 1:100, not_reached = 1)
  min_valid <- 3
  
  # bgdata = data
  result <- list(
    data = data,
    bgdata = bgdata[, -3],
    ID_t = data.frame(ID_t = 100:1)
  )
  test <- pre_process_background_data(bgdata, data, include_nr = FALSE,
                                      items_not_reached, min_valid)
  expect_equal(names(test), c("data", "bgdata", "ID_t"))
  expect_equivalent(test, result)
})

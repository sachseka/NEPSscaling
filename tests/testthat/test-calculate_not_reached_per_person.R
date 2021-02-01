context("calculate_not_reached_per_person")

test_that("calculate_not_reached_per_person: cross-sectional", {
  data <- data.frame(
    ID_t = 1:10,
    item1 = 1:10,
    item2 = 1:10
  )
  data[2:5, 2:3] <- -94
  sel <- list(c("item1", "item2"))
  waves <- c("_w1", "_w2")
  SC <- "SC6"
  domain <- "RE"

  result <- data.frame(
    ID_t = 1:10,
    items_not_reached_w1 = c(0, 2, 2, 2, 2, 0, 0, 0, 0, 0)
  )
  expect_equivalent(calculate_not_reached_per_person(data, sel, waves, SC, domain, longitudinal = FALSE),
                    result)
})

test_that("calculate_not_reached_per_person: longitudinal", {
  data <- data.frame(
    ID_t = 1:10,
    item1 = 1:10,
    item2 = 1:10,
    rea3_sc1u = 1:10,
    rea5_sc1u = 1:10
  )
  data[2:5, 2:3] <- -94
  data[1:3, 4] <- NA
  data[4:10, 5] <- NA
  sel <- list(c("item1", "item2"), c("item1"))
  waves <- c("_w1", "_w2")
  SC <- "SC5"
  domain <- "RE"

  result <- data.frame(
    ID_t = 1:10,
    items_not_reached_w1 = c(0, 2, 2, 2, 2, 0, 0, 0, 0, 0),
    items_not_reached_w2 = c(0, 1, 1, 1, 1, 0, 0, 0, 0, 0)
  )
  expect_equivalent(calculate_not_reached_per_person(data, sel, waves, SC, domain, longitudinal = TRUE),
                    result)

  waves <- c("_w3", "_w5")
  SC <- "SC6"

  result <- data.frame(
    ID_t = 1:10,
    items_not_reached_w3 = c(NA, NA, NA, 2, 2, 0, 0, 0, 0, 0),
    items_not_reached_w5 = c(0, 1, 1, NA, NA, NA, NA, NA, NA, NA)
  )
  expect_equivalent(calculate_not_reached_per_person(data, sel, waves, SC, domain, longitudinal = TRUE),
                    result)
})

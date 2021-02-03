context("create_facet")

test_that("create_facet: no rotation", {
  data <- data.frame(ID_t = 1:10)

  result <- data.frame(ID_t = 1:10, position = 1)
  expect_equivalent(create_facet(data, SC = "SC1", wave = "w1", domain = NULL),
                    result)
  expect_equivalent(create_facet(data, SC = "SC1", wave = "w7", domain = NULL),
                    result)
  expect_equivalent(create_facet(data, SC = "SC4", wave = "w3", domain = NULL),
                    result)
  expect_equivalent(create_facet(data, SC = "SC5", wave = "w7", domain = NULL),
                    result)
  expect_equivalent(create_facet(data, SC = "SC2", wave = "w6", domain = NULL),
                    result)
})

test_that("create_facet: rotation", {
  SC <- "SC4"
  wave <- "w1"
  domain <- "RE"
  data <- data.frame(ID_t = 1:10, tx80211_w1 = rep(1:2, 5))

  result <- data.frame(ID_t = 1:10, position = rep(1:2, 5))
  expect_equivalent(create_facet(data, SC, wave, domain),
                    result)

  data <- data.frame(ID_t = 1:11, tx80211_w1 = c(rep(1:2, 5), 3))

  result <- data.frame(ID_t = 1:10, position = rep(1:2, 5))
  expect_equivalent(create_facet(data, SC, wave, domain),
                    result)
})

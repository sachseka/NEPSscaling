context("estimate_wle")

data(data.sim.rasch, package = "TAM")
longitudinal <- TRUE
waves <- c("_w1", "_w2", "_w3")
mod <- lapply(1:3, function(x) TAM::tam.mml(data.sim.rasch, verbose = FALSE)) # cross-sec also in form of list, but with only 1 entry!

test_that("estimate wles (longitudinal)", {
  test <- estimate_wles(longitudinal, waves, mod)
  expect_equal(length(test), 2)
  expect_equal(names(test), c("wle", "WLE.rel"))
  expect_equal(names(test$wle), c("ID_t", "wle_w1", "se_w1", "wle_w2", "se_w2",
                                  "wle_w3", "se_w3"))
})

longitudinal <- FALSE
waves <- ""
test_that("estimate wles (cross-sectional)", {
  test <- estimate_wles(longitudinal, waves, mod)
  expect_equal(length(test), 2)
  expect_equal(names(test), c("wle", "WLE.rel"))
  expect_equal(names(test$wle), c("ID_t", "wle", "se"))
})

rm(data.sim.rasch, mod, longitudinal, waves)

context("extract_bgdata_variables")

test_that("extract_bgdata_variables: No excluded variables", {
  # exclude_for_wave == NULL filtered out before applying function
  bgdatacom <- data.frame(ID_t = 1:10, var1 = 1, var2 = 2, var3 = 3)
  exclude_for_wave <- list(
    w1 = "var1",
    w3 = c("var2", "var3")
  )
  waves <- c("_w1", "_w2", "_w3")

  expect_equivalent(extract_bgdata_variables(bgdatacom, exclude_for_wave,
                                             waves, j = 2),
                    bgdatacom)
})

test_that("extract_bgdata_variables: With excluded variables", {
  bgdatacom <- data.frame(ID_t = 1:10, var1 = 1, var2 = 2, var3 = 3)
  exclude_for_wave <- list(
    w1 = "var1",
    w3 = c("var2", "var3")
  )
  waves <- c("_w1", "_w2", "_w3")

  expect_equivalent(extract_bgdata_variables(bgdatacom, exclude_for_wave,
                                             waves, j = 1),
                    bgdatacom[, -2])

  expect_equivalent(extract_bgdata_variables(bgdatacom, exclude_for_wave,
                                             waves, j = 3),
                    bgdatacom[, -c(3, 4)])
})


context("add_not_reached_to_bgdata")

test_that("add_not_reached_to_bgdata: include_nr = FALSE",{
  bgdata <- data.frame(ID_t = 1:10, var = 1)

  expect_equivalent(add_not_reached_to_bgdata(bgdata, ID_t = NULL,
                                              items_not_reached = NULL,
                                              include_nr =  FALSE),
                    bgdata)
})

test_that("add_not_reached_to_bgdata: include_nr = FALSE",{
  bgdata <- data.frame(ID_t = 1:10, var = 1)
  ID_t <- data.frame(ID_t = 1:10)
  items_not_reached <- data.frame(ID_t = 1:10, nr = 2)

  expect_equivalent(add_not_reached_to_bgdata(bgdata, ID_t, items_not_reached,
                                              include_nr =  TRUE),
                    cbind(bgdata, nr = 2))

  expect_equivalent(add_not_reached_to_bgdata(bgdata = NULL, ID_t,
                                              items_not_reached,
                                              include_nr =  TRUE),
                    cbind(ID_t, nr = 2))
})

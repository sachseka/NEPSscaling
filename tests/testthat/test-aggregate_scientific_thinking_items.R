context("aggregate_scientific_thinking_items")

test_that("aggregate_scientific_thinking_items", {
  resp <- data.frame(
    ID_t = 1:10,
    "stg12nh1" = 1,
    "stg12nh2" = 1, "stg12eg1" = 1, "stg12eg2" = 1,
    "stg12mt1" = 0, "stg12mt2" = 0, "stg12cw1" = 0, "stg12cw2" = 1,
    "stg12pd1" = 1, "stg12pd2" = 2
  )
  result <- data.frame(
    ID_t = 1:10,
    "stg12nhs_c" = 2, "stg12egs_c" = 2, "stg12mts_c" = 0, "stg12cws_c" = 1, "stg12pds_c" = 3
  )
  expect_equivalent(aggregate_scientific_thinking_items(resp), result)
})

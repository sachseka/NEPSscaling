context("prepare_resp_q_longitudinal")

#TODO: item names necessary for get_indicators_for_half_scoring!

SC <- "SC5"
domain <- "MA"
items <- list(
  c("mas1q02s_c", "dummy"),
  c("mas1v032_sc5s12_c", "maa9d09s_sc5s12_c", "maa9d13s_sc5s12_c",
    "maa9r03s_sc5s12_c", "mas1q02s_sc5s12_c", "maa9v27s_sc5s12_c",
    "maa9d20s_sc5s12_c", "maa9r301_sc5s12_c", "maa9r26s_sc5s12_c",
    "maa9v28s_sc5s12_c", "maa9v07s_sc5s12_c")
)
waves <- c("_w1", "_w12")
resp <- list(
  data.frame(ID_t = 1:100,
             mas1q02s_c = rep(0, 100),
             dummy = rep(0)),
  data.frame(ID_t = 1:100,
             mas1v032_sc5s12_c = rep(0, 100),
             maa9d09s_sc5s12_c = rep(0, 100),
             maa9d13s_sc5s12_c = rep(0, 100),
             maa9r03s_sc5s12_c = rep(0, 100),
             mas1q02s_sc5s12_c = rep(0, 100),
             maa9v27s_sc5s12_c = rep(0, 100),
             maa9d20s_sc5s12_c = rep(0, 100),
             maa9r301_sc5s12_c = rep(0, 100),
             maa9r26s_sc5s12_c = rep(0, 100),
             maa9v28s_sc5s12_c = rep(0, 100),
             maa9v07s_sc5s12_c = rep(0, 100))
)

test_that("prepare_resp_q_longitudinal: general case, e.g., SC5/MA", {
  result <- list(
    resp = resp,
    Q = list(
      matrix(c(0.5, 1), nrow = 2, ncol = 1),
      matrix(c(1, 0.5, 0.5, 0.5, 1, 0.5, 1, 1, 0.5, 0.5, 0.5), nrow = 11, ncol = 1)
    )
  )
  test <- prepare_resp_q_longitudinal(PCM = list(TRUE, TRUE), resp, items,
                                      waves, SC, domain)
  expect_equal(test, result)

  result$Q <- list(matrix(1, nrow = 2, ncol = 1),
                   matrix(1, nrow = 11, ncol = 1))
  test <- prepare_resp_q_longitudinal(PCM = list(FALSE, FALSE), resp, items,
                                      waves, SC, domain)
  expect_equal(test, result)
})

test_that("prepare_resp_q_longitudinal: exception for SC4 science", {
  test <- prepare_resp_q_longitudinal(PCM = list(TRUE, TRUE), resp, items,
                                      waves, SC = "SC4", domain = "SC")
  expect_equal(names(test), c("resp", "Q"))
  expect_equal(test, result)

  test <- prepare_resp_q_longitudinal(PCM = list(FALSE, FALSE), resp, items,
                                      waves, SC = "SC4", domain = "SC")
  expect_equal(names(test), c("resp", "Q"))
  expect_equal(test, result)
})

# prepare_resp_q_longitudinal <- function(PCM, resp, items, waves, SC, domain) {
  Q <- create_loading_matrix_q_longitudinal(SC, domain, items)
  for (i in seq(length(PCM))) {
    if (PCM[[i]]) {
      resp[[i]][, items[[i]]] <- collapse_categories_pcm(
        resp[[i]][, items[[i]]], SC, gsub("_", "", waves)[i], domain
      )
      ind <- get_indicators_for_half_scoring(
        SC, domain, gsub("_", "", waves[i])
      )
      if (SC == "SC4" & domain == "SC" & i == 1) {
        Q[[i]][which(items[[i]] %in% ind[[1]]), ] <- 2 / 3
        Q[[i]][which(items[[i]] %in% ind[[2]]), ] <- 0.5
      } else {
        Q[[i]][which(items[[i]] %in% ind), ] <- 0.5
      }
    }
  }
  list(resp = resp, Q = Q)
# }

#' prepare competence data and scoring matrix
#'
#' @param PCM logical; whether there are partial credit items in the response
#' data
#' @param resp list of data.frames containing containing the responses per wave
#' @param items list of item names per wave
#' @param waves character; waves of longitudinal assessment ("_wx", "_wy")
#' @param SC character; starting cohort ("SCx")
#' @param domain character; competence domain (e.g., "RE", "MA")
#'
#' @noRd
prepare_resp_q_longitudinal <- function(PCM, resp, items, waves, SC, domain) {
  Q <- create_loading_matrix_q_longitudinal(SC, domain, items)
  for (i in seq(length(PCM))) {
    if (PCM[[i]]) {
      resp[[i]] <- collapse_categories_pcm(
        resp[[i]], SC, gsub("_", "", waves)[i], domain
      )
      ind <- get_indicators_for_half_scoring(
        SC, domain, gsub("_", "", waves[i])
      )
      Q <- half_score_q_matrix(SC, domain, i, Q, items, ind)
    }
  }
  list(resp = resp, Q = Q)
}

half_score_q_matrix <- function(SC, domain, i, Q, items, ind) {
  if (SC == "SC4" & domain == "SC" & i == 1) {
    Q[[i]][which(items[[i]] %in% ind[[1]]), ] <- 2 / 3
    Q[[i]][which(items[[i]] %in% ind[[2]]), ] <- 0.5
  } else {
    Q[[i]][which(items[[i]] %in% ind), ] <- 0.5
  }
  Q
}

#' prepare competence data and scoring matrix
#'
#' @param resp data.frame containing containing the responses per wave
#' @param items character vector of item names per wave
#' @param waves character; waves of longitudinal assessment ("_wx", "_wy")
#' @param SC character; starting cohort ("SCx")
#' @param domain character; competence domain (e.g., "RE", "MA")
#'
#' @noRd
prepare_resp_b_cross <- function(resp, items, waves, SC, domain) {
  resp <- collapse_categories_pcm(
    resp, SC, gsub("_", "", waves), domain
  )
  ind <- get_indicators_for_half_scoring(SC, domain, gsub("_", "", waves))
  B <- TAM::designMatrices(modeltype = "PCM", resp = resp[, items])$B
  B <- half_score_b_array(SC, domain, waves, B, items, ind)
  list(resp = resp, B = B)
}

half_score_b_array <- function(SC, domain, waves, B, items, ind) {
  if (SC == "SC4" & domain == "SC" & waves == "_w1") {
    B[which(items %in% ind[[1]]), , ] <-
      (2/3) * B[which(items %in% ind[[1]]), , ]
    B[which(items %in% ind[[2]]), , ] <- 0.5 * B[which(items %in% ind[[2]]), , ]
  } else {
    B[which(items %in% ind), , ] <- 0.5 * B[which(items %in% ind), , ]
  }
  B
}

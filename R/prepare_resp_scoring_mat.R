#' prepare competence data and scoring matrix
#'
#' @param PCM logical; whether there are partial credit items in the response 
#' data
#' @param resp list of data.frames containing containing the responses per wave
#' @param items list of item names per wave
#' @param waves character; waves of longitudinal assessment ("_wx", "_wy")
#' @param SC character; starting cohort ("SCx")
#' @param domain character; competence domain (e.g., "RE", "MA")
#' @param Q list of design / scoring matrices
#'
#' @noRd
prepare_resp_q_longitudinal <- function(PCM, resp, items, waves, SC, domain,
                                        Q) {
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
  resp[, items] <- collapse_categories_pcm(
    resp[, items], SC, gsub("_", "", waves), domain
  )
  ind <- get_indicators_for_half_scoring(SC, domain, gsub("_", "", waves))
  B <- TAM::designMatrices(modeltype = "PCM", resp = resp[, items])$B
  if (SC == "SC4" & domain == "SC" & waves == "_w1") {
    B[which(items %in% ind[[1]]), , ] <-
      (2/3) * B[which(items %in% ind[[1]]), , ]
    B[which(items %in% ind[[2]]), , ] <- 0.5 * B[which(items %in% ind[[2]]), , ]
  } else {
    B[which(items %in% ind), , ] <- 0.5 * B[which(items %in% ind), , ]
  }
  list(resp = resp, B = B)
}

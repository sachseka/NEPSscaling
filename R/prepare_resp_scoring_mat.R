#' prepare competence data and scoring matrix
#'
#' @param PCM ...
#' @param resp ...
#' @param items ...
#' @param waves ...
#' @param SC ...
#' @param domain ...
#' @param Q ...
#'
#' @noRd

prepare_resp_q_longitudinal <- function(PCM, resp, items, waves, SC, domain,
                                        Q) {
  for (i in seq(length(PCM))) {
    if (PCM[[i]]) {
      res <- collapse_categories_pcm(
        resp[[i]][, items[[i]]], SC, gsub("_", "", waves)[i], domain
      )
      resp[[i]][, items[[i]]] <- res$resp[, items[[i]]]
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
#' @param resp ...
#' @param items ...
#' @param waves ...
#' @param SC ...
#' @param domain ...
#'
#' @noRd

prepare_resp_b_cross <- function(resp, items, waves, SC, domain) {
  res <- collapse_categories_pcm(
    resp[, items], SC, gsub("_", "", waves), domain
  )
  resp[, items] <- res$resp[, items]
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

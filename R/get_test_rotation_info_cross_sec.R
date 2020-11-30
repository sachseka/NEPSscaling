#' test rotation / testlet position for cross-sectional estimation
#'
#' @param rotation logical; whether test rotation is to be considered
#' @param data data.frame; xTargetCompetencies etc.
#' @param SC character; starting cohort ("SCx")
#' @param wave character; wave of assessment ("wx")
#' @param domain character; abbr. of competence domain (e.g. "MA")
#' @param resp data.frame; response data + ID_t
#' @param bgdata data.frame or NULL; background data
#' @param ID_t data.frame; ID_t of test takers (only column)
#'
#' @noRd

get_test_rotation_info_cross_sec <- function(rotation, data, SC, wave,
                                             domain, resp, bgdata, ID_t) {
  position <- data[, "ID_t", drop = FALSE]
  # construct facet to correct for rotation design
  if ((SC == "SC1" & wave %in% c("w1", "w7")) |
      (SC == "SC4" & wave %in% c("w3")) |
      (SC == "SC5" & wave %in% c("w7")) |
      (SC == "SC2" & wave %in% c("w6", "w9"))) {
    # no test rotation or position variable available
    position[["position"]] <- 1
  } else {
    position[["position"]] <- data[[paste0("tx80211_", wave)]]
    position[["position"]][
      !is.na(position[["position"]]) &
        (position[["position"]] %in%
           testlet_position[[SC]][[domain]][[wave]][, 1])] <- 1
    position[["position"]][
      !is.na(position[["position"]]) &
        (position[["position"]] %in%
           testlet_position[[SC]][[domain]][[wave]][, 2])] <- 2
  }
  # possible NAs lead to further data selection
  position <- position[!is.na(position[["position"]]), ]
  rotation <- rotation & length(unique(position[["position"]])) > 1
  if (rotation) {
    data <- data[data[["ID_t"]] %in% position[["ID_t"]], ]
    resp <- resp[resp[["ID_t"]] %in% position[["ID_t"]], ]
    ID_t <- ID_t[ID_t[["ID_t"]] %in% position[["ID_t"]], , drop = FALSE]
    if (!is.null(bgdata)) {
      bgdata <- bgdata[bgdata[["ID_t"]] %in% position[["ID_t"]], ]
    }
    # # possible NAs in position variable treated as third group
    # position[is.na(position[["position"]]), "position"] <- 3
    # format position effect information
    position <- position[, "position", drop = FALSE]
  }
  list(position = position, rotation = rotation, data = data,
       resp = resp, ID_t = ID_t, bgdata = bgdata)
}

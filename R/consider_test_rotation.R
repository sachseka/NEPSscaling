#' if tests have been rotated, calculate position variable
#'
#' @param longitudinal ...
#' @param rotation ...
#' @param data ...
#' @param SC ...
#' @param wave ...
#' @param domain ...
#' @param resp ...
#' @param bgdata ...
#' @param ID_t ...
#'
#' @noRd

consider_test_rotation <- function(longitudinal, rotation, data, SC, wave,
                                   domain, resp, bgdata, ID_t) {
  if (longitudinal) {
    rotation <- FALSE
    Q <- create_loading_matrix_q_longitudinal(SC, domain)
    testletSetting <- position <- NULL
    if ((SC == "SC4" & domain %in% c("RE", "MA")) |
        (SC == "SC3" & domain == "RE") |
        (SC == "SC2" & domain %in% c("VO", "MA"))) {
      wave <- ifelse(SC == "SC4", "w7",
                     ifelse(SC == "SC3", "w6",
                            ifelse(SC == "SC2" & domain == "VO", "w3", "w4")))
      position <- data.frame(ID_t = data[["ID_t"]],
                             position = data[[paste0("tx80211_", wave)]])
      position[["position"]][
        !is.na(position[["position"]]) &
          (position[["position"]] %in%
             testlet_position[[SC]][[domain]][[wave]][, 1])] <- 1
      position[["position"]][
        !is.na(position[["position"]]) &
          (position[["position"]] %in%
             testlet_position[[SC]][[domain]][[wave]][, 2])] <- 2
    }
    if (SC == "SC4" & domain == "MA") {
      testletSetting <- data.frame(
        ID_t = data[["ID_t"]],
        difficultTestlet = ifelse(
          data[["tx80211_w7"]] %in% c(285, 288, 291:293, 296:303), TRUE,
          ifelse(is.na(data[["tx80211_w7"]]), NA, FALSE)),
        atHome = ifelse(data[["tx80211_w7"]] %in% c(281:295), TRUE,
                        ifelse(is.na(data[["tx80211_w7"]]), NA, FALSE))
      )
    }
    if (SC == "SC2" & domain == "RE") {
      testletSetting <- data.frame(
        ID_t = data[["ID_t"]],
        easy = ifelse(
          data[["tx80220_w9"]] %in% c(751, 753, 757), TRUE,
          ifelse(is.na(data[["tx80220_w9"]]), NA, FALSE))
      )
    }
    return(list(rotation = rotation, Q = Q, testletSetting = testletSetting,
                position = position))
  } else {
    if (rotation || (SC == "SC4" & domain %in% c("RE", "MA") & wave == "w7") ||
        (SC == "SC2" & domain == "RE" & wave == "w9")) {
      position <- data[, "ID_t", drop = FALSE]
      # construct facet to correct for rotation design
      if ((SC == "SC1" & wave %in% c("w1", "w7")) |
          (SC == "SC4" & wave %in% c("w3")) |
          (SC == "SC5" & wave %in% c("w7"))) {
        # no test rotation or position variable available
        position[["position"]] <- 1
      } else if (SC == "SC2" & domain == "RE" & wave == "w9") {
        # no test rotation or position variable available
        position[["position"]] <- 1
        testletSetting <- data.frame(
          ID_t = data[["ID_t"]],
          easy = ifelse(
            data[["tx80220_w9"]] %in% c(751, 753, 757), TRUE,
            ifelse(is.na(data[["tx80220_w9"]]), NA, FALSE))
        )
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
        if (SC == "SC4" & wave == "w7" & domain == "MA") {
          position[["difficultTestlet"]] <- ifelse(
            data[["tx80211_w7"]] %in% c(285, 288, 291:293, 296:303), TRUE,
            ifelse(is.na(data[["tx80211_w7"]]), NA, FALSE))
          position[["atHome"]] <-
            ifelse(data[["tx80211_w7"]] %in% c(281:295), TRUE,
                   ifelse(is.na(data[["tx80211_w7"]]), NA, FALSE))
        }
      }
      # possible NAs lead to further data selection
      position <- position[!is.na(position[["position"]]), ]
      rotation <- rotation & length(unique(position[["position"]])) > 1
      testletSetting <- position
      if (SC == "SC4" & domain == "MA" & wave == "w7") {
        testletSetting <-
          testletSetting[, c("ID_t", "difficultTestlet", "atHome")]
      }
      if (rotation) {
        data <- data[data[["ID_t"]] %in% position[["ID_t"]], ]
        resp <- resp[resp[["ID_t"]] %in% position[["ID_t"]], ]
        ID_t <- ID_t[ID_t[["ID_t"]] %in% position[["ID_t"]], , drop = FALSE]
        testletSetting <-
          testletSetting[testletSetting[["ID_t"]] %in% position[["ID_t"]], ]
        if (!is.null(bgdata)) {
          bgdata <- bgdata[bgdata[["ID_t"]] %in% position[["ID_t"]], ]
        }
        # # possible NAs in position variable treated as third group
        # position[is.na(position[["position"]]), "position"] <- 3
        # format position effect information
        position <- position[, "position", drop = FALSE]
      }
      return(list(position = position, rotation = rotation, data = data,
                  resp = resp, ID_t = ID_t, bgdata = bgdata,
                  testletSetting = testletSetting))
    }
  }
}

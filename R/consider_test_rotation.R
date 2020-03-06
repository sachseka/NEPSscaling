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
    if (SC == "SC4") { 
      position[, "position"] <- data[, "tx80211_w7"]
      if (domain == "RE") {
        position[
          !is.na(position$position) &
            (position$position %in% c(283, 284, 285, 286, 287, 288, 300,
                                      301, 302, 303)),
          "position"
          ] <- 1
        position[
          !is.na(position$position) &
            (position$position %in% c(281, 282, 289, 290, 291, 292, 296,
                                      297, 298, 299)),
          "position"
          ] <- 2
        position$position <-
          haven::labelled(
            position$position,
            c(
              "Reading first" = 1,
              "Reading second" = 2
            )
        )
      }
      if (domain == "MA") {
        position[
          !is.na(position$position) &
            (position$position %in% c(289, 290, 291, 292, 293, 294)),
          "position"
          ] <- 1
        position[
          !is.na(position$position) &
            (position$position %in% c(284, 285, 287, 288, 296, 297, 298,
                                      299, 300, 301, 302, 303)),
          "position"
          ] <- 2
        position$position <-
          haven::labelled(
            position$position,
            c(
              "Math first" = 1,
              "Math second" = 2
            )
          )
        testletSetting <- data.frame(
          ID_t = data$ID_t,
          difficultTestlet = ifelse(
                               data$tx80211_w7 %in% c(285, 288, 
                                                      291:293, 296:303), 
                               TRUE,
                               ifelse(is.na(data$tx80211_w7), NA, 
                                      FALSE)),
          atHome = ifelse(data$tx80211_w7 %in% c(281:295), TRUE,
                          ifelse(is.na(data$tx80211_w7), NA, FALSE))
        )
      }
    }
    return(list(rotation = rotation, Q = Q, 
                testletSetting = testletSetting,
                position = position))
  } else {
    if (rotation || (SC == "SC4" & domain == "MA" & wave == "w7")) {
      position <- data.frame(
        ID_t = data$ID_t,
        position = rep(NA, nrow(data))
      )
      # construct facet to correct for rotation design
      if (SC == "SC1") {
        stop("Sorry, not yet implemented.")
      } else if (SC == "SC2") {
        stop("Sorry, not yet implemented.")
      } else if (SC == "SC3") {
        stop("Sorry, not yet implemented.")
      } else if (SC == "SC4") {
        if (wave == "w1") { # grade 9
          position[, "position"] <- data[, "tx80211_w1"]
          # if (domain == "VO") {
          #   # no rotation
          # }
          # if (domain == "MA") {
          #   # no rotation
          # }
          if (domain == "SC") {
            position[
              !is.na(position$position) &
                (position$position == 129),
              "position"
              ] <- 1
            position[
              !is.na(position$position) &
                (position$position == 128),
              "position"
              ] <- 2
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "Science first" = 1,
                  "Science second" = 2
                )
              )
          }
          if (domain == "IC") {
            position[
              !is.na(position$position) &
                (position$position == 128),
              "position"
              ] <- 1
            position[
              !is.na(position$position) &
                (position$position == 129),
              "position"
              ] <- 2
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "ICT first" = 1,
                  "ICT second" = 2
                )
              )
          }
        }
        # if (wave == "w2") { # grade 9
        #   # no rotation!
        # }
        # if (wave == "w3") {# grade 10
        #   # no rotation!
        # }
        # if (wave == "w5") { # grade 11
        #   # no rotation!
        # }
        if (wave == "w7") { # grade 12
          position[, "position"] <- data[, "tx80211_w7"]
          if (domain == "RE") {
            position[
              !is.na(position$position) &
                (position$position %in% c(283, 284, 285, 286, 287, 288, 300,
                                          301, 302, 303)),
              "position"
              ] <- 1
            position[
              !is.na(position$position) &
                (position$position %in% c(281, 282, 289, 290, 291, 292, 296,
                                          297, 298, 299)),
              "position"
              ] <- 2
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "Reading first" = 1,
                  "Reading second" = 2
                )
              )
          }
          if (domain == "MA") {
            position[
              !is.na(position$position) &
                (position$position %in% c(289, 290, 291, 292, 293, 294)),
              "position"
              ] <- 1
            position[
              !is.na(position$position) &
                (position$position %in% c(284, 285, 287, 288, 296, 297, 298,
                                          299, 300, 301, 302, 303)),
              "position"
              ] <- 2
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "Math first" = 1,
                  "Math second" = 2
                )
              )
            position$difficultTestlet <- ifelse(
                                           data$tx80211_w7 %in% c(285, 288, 
                                                          291:293, 296:303), 
                                           TRUE,
                                           ifelse(is.na(data$tx80211_w7), NA, 
                                                  FALSE))
            position$atHome <- ifelse(data$tx80211_w7 %in% c(281:295), TRUE,
                                      ifelse(is.na(data$tx80211_w7), NA, FALSE))
          }
          if (domain == "IC") {
            position[
              !is.na(position$position) &
                (position$position %in% c(281, 282, 295, 296, 297, 298, 299)),
              "position"
              ] <- 1
            position[
              !is.na(position$position) &
                (position$position %in% c(283, 286, 300, 301, 302, 303)),
              "position"
              ] <- 2
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "ICT first" = 1,
                  "ICT second" = 2
                )
              )
          }
          if (domain == "EF") {
            position[
              !is.na(position$position) &
                (position$position %in% c(298, 299, 302, 303)),
              "position"
              ] <- 1
            position[
              !is.na(position$position) &
                (position$position %in% c(296, 297, 300, 301)),
              "position"
              ] <- 2
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "English 4th" = 1,
                  "English 5th" = 2
                )
              )
          }
          if (domain == "ST") {
            position[
              !is.na(position$position) &
                (position$position %in% c(296, 297, 300, 301)),
              "position"
              ] <- 1
            position[
              !is.na(position$position) &
                (position$position %in% c(298, 299, 302, 303)),
              "position"
              ] <- 2
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "Scientific thinking 4th" = 1,
                  "Scientific thinking 5th" = 2
                )
              )
          }
        }
        if (wave == "w10") { # 21 years
          position[, "position"] <- data[, "tx80211_w10"]
          if (domain == "RE") {
            position[
              !is.na(position$position) &
                (position$position %in% c(470, 471, 474, 475)),
              "position"
              ] <- 1
            position[
              !is.na(position$position) &
                (position$position %in% c(472, 473)),
              "position"
              ] <- 2
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "Reading first" = 1,
                  "Reading second" = 2
                )
              )
          }
          if (domain == "MA") {
            position[
              !is.na(position$position) &
                (position$position %in% c(472, 473, 476)),
              "position"
              ] <- 1
            position[
              !is.na(position$position) &
                (position$position %in% c(470, 471)),
              "position"
              ] <- 2
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "Math first" = 1,
                  "Math second" = 2
                )
              )
          }
        }
        # if (wave == "w13") { # 25 years
        #   # position[, "position"] <- data[, "tx80211_w13"]
        #   if (domain == "SC") {
        #     #
        #   }
        #   if (domain == "IC") {
        #     #
        #   }
        # }
      } else if (SC == "SC5") { # w7 does not have rotation and can be ignored here
        if (wave == "w1") {
          position[, "position"] <- data[, "tx80211_w1"]
          if (domain == "RE") {
            position[
              !is.na(position$position) &
                (position$position == 126),
              "position"
            ] <- 1 # reading first
            position[
              !is.na(position$position) &
                (position$position == 127),
              "position"
            ] <- 2 # reading second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "reading first" = 1,
                  "reading second" = 2
                )
              )
          } else if (domain == "MA") {
            position[
              !is.na(position$position) &
                (position$position == 127),
              "position"
            ] <- 1 # maths first
            position[
              !is.na(position$position) &
                (position$position == 126),
              "position"
            ] <- 2 # math second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "math first" = 1,
                  "math second" = 2
                )
              )
          }
        } else if (wave == "w5") {
          position[, "position"] <- data[, "tx80211_w5"]
          if (domain == "IC") {
            position[
              !is.na(position$position) &
                (position$position %in% c(331, 333, 335)),
              "position"
            ] <- 1 # ict first
            position[
              !is.na(position$position) &
                (position$position %in% c(330, 332, 334, 336)),
              "position"
            ] <- 2 # ict second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "ICT first" = 1,
                  "ICT second" = 2
                )
              )
          } else if (domain == "SC") {
            position[
              !is.na(position$position) &
                (position$position %in% c(330, 332, 334, 336)),
              "position"
            ] <- 1 # sc first
            position[
              !is.na(position$position) &
                (position$position %in% c(331, 333, 335)),
              "position"
            ] <- 2 # science second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "science first" = 1,
                  "science second" = 2
                )
              )
          }
        } else if (wave == "w12") {
          position[, "position"] <- data[, "tx80211_w12"]
          if (domain == "RE") {
            position[
              !is.na(position$position) &
                (position$position %in% c(459, 462, 465, 468)),
              "position"
            ] <- 1 # reading first
            position[
              !is.na(position$position) &
                (position$position %in% c(458, 463, 464, 469)),
              "position"
            ] <- 2 # reading second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "reading first" = 1,
                  "reading second" = 2
                )
              )
          } else if (domain == "MA") {
            position[
              !is.na(position$position) &
                (position$position %in% c(458, 460, 464, 466)),
              "position"
            ] <- 1 # math first
            position[
              !is.na(position$position) &
                (position$position %in% c(459, 461, 465, 467)),
              "position"
            ] <- 2 # math second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "math first" = 1,
                  "math second" = 2
                )
              )
          } else if (domain == "EF") {
            position[
              !is.na(position$position) &
                (position$position %in% c(461, 463, 467, 469)),
              "position"
            ] <- 1 # English first
            position[
              !is.na(position$position) &
                (position$position %in% c(460, 462, 466, 468)),
              "position"
            ] <- 2 # English second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "English first" = 1,
                  "English second" = 2
                )
              )
          }
        }
      } else if (SC == "SC6") {
        if (wave == "w3") {
          position[, "position"] <- data[, "tx80211_w3"]
          if (domain == "RE") {
            position[
              !is.na(position$position) &
                (position$position %in% c(123, 125)),
              "position"
            ] <- 1 # reading first
            position[
              !is.na(position$position) &
                (position$position %in% c(122, 124)),
              "position"
            ] <- 2 # reading second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "reading first" = 1,
                  "reading second" = 2
                )
              )
          } else if (domain == "MA") {
            position[
              !is.na(position$position) &
                (position$position %in% c(122, 124)),
              "position"
            ] <- 1 # maths first
            position[
              !is.na(position$position) &
                (position$position %in% c(123, 125)),
              "position"
            ] <- 2 # math second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "math first" = 1,
                  "math second" = 2
                )
              )
          }
        } else if (wave == "w5") {
          position[, "position"] <- data[, "tx80211_w5"]
          if (domain == "IC") {
            position[
              !is.na(position$position) &
                position$position == 248,
              "position"
            ] <- 1 # ict first
            position[
              !is.na(position$position) &
                position$position == 247,
              "position"
            ] <- 2 # ict second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "ict first" = 1,
                  "ict second" = 2
                )
              )
          } else if (domain == "SC") {
            position[
              !is.na(position$position) &
                position$position == 247,
              "position"
            ] <- 1 # science first
            position[
              !is.na(position$position) &
                position$position == 248,
              "position"
            ] <- 2 # science second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "science first" = 1,
                  "science second" = 2
                )
              )
          } else if (domain == "RE") {
            position[
              !is.na(position$position) &
                position$position == 249,
              "position"
            ] <- 1 # no rotation should be found
          }
        } else if (wave == "w9") {
          position[, "position"] <- data[, "tx80211_w9"]
          if (domain == "RE") {
            position[
              !is.na(position$position) &
                position$position %in%
                  c(444, 445, 448, 449, 452:455),
              "position"
            ] <- 1 # reading first
            position[
              !is.na(position$position) &
                position$position %in%
                  c(446, 447, 450, 451, 456, 457),
              "position"
            ] <- 2 # reading second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "reading first" = 1,
                  "reading second" = 2
                )
              )
          } else if (domain == "MA") {
            position[
              !is.na(position$position) &
                position$position %in%
                  c(446, 447, 450, 451, 456, 457),
              "position"
            ] <- 1 # math first
            position[
              !is.na(position$position) &
                position$position %in%
                  c(444, 445, 448, 449, 452:455),
              "position"
            ] <- 2 # math second
            position$position <-
              haven::labelled(
                position$position,
                c(
                  "math first" = 1,
                  "math math" = 2
                )
              )
          }
        }
      }
      # possible NAs lead to further data selection
      position <- position[!is.na(position$position), ]
      rotation <- rotation & length(unique(position$position)) > 1 # T: with rotation, F: without rotation
      testletSetting <- position
      if (SC == "SC4" & domain == "MA" & wave == "w7") {
        testletSetting <- testletSetting[, c("ID_t", "difficultTestlet", "atHome")]
      }
      if (rotation) {
        data <- data[data$ID_t %in% position$ID_t, ]
        resp <- resp[resp$ID_t %in% position$ID_t, ]
        ID_t <- ID_t[ID_t$ID_t %in% position$ID_t, , drop = FALSE]
        testletSetting <- testletSetting[testletSetting$ID_t %in% position$ID_t, ]
        if (!is.null(bgdata)) {
          bgdata <- bgdata[bgdata$ID_t %in% position$ID_t, ]
        }
        # # possible NAs in position variable treated as third group
        # position[is.na(position$position), "position"] <- 3
        # format position effect information
        position <- position[, 2, drop = FALSE]
      }
      return(
        list(
          position = position, rotation = rotation, data = data,
          resp = resp, ID_t = ID_t, bgdata = bgdata, 
          testletSetting = testletSetting
        )
      )
    }
  }
}

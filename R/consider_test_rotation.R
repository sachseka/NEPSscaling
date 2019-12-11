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
    return(list(rotation = rotation, Q = Q))
  } else {
    if (rotation) {
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
        stop("Sorry, not yet implemented.")
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
      rotation <- length(unique(position$position)) > 1 # T: with rotation, F: without rotation
      if (rotation) {
        data <- data[data$ID_t %in% position$ID_t, ]
        resp <- resp[resp$ID_t %in% position$ID_t, ]
        ID_t <- ID_t[ID_t$ID_t %in% position$ID_t, , drop = FALSE]
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
          resp = resp, ID_t = ID_t, bgdata = bgdata
        )
      )
    }
  }
}

#' process background data
#'
#' @param bgdata ...
#' @param data ...
#' @param include_nr ...
#' @param nr ...
#' @param min_valid ...
#'
#' @noRd

pre_process_background_data <- function(bgdata, data, include_nr, nr, min_valid) {
  if (is.null(bgdata)) {
    ID_t <- data[, "ID_t", drop = FALSE]
    if (include_nr) {
      bgdata <- dplyr::left_join(ID_t, nr, by = "ID_t")
    }
    return(list(ID_t = ID_t, bgdata = bgdata))
  } else {
    if (!is.data.frame(bgdata)) {
      stop("bgdata must be a data.frame.")
    }
    if (is.null(bgdata$ID_t)) {
      stop("ID_t must be included in bgdata.")
    }
    bgdata <- bgdata[order(bgdata$ID_t), ]

    if (min_valid > 0) {
      bgdata <- bgdata[bgdata$ID_t %in% data$ID_t, ]
      if (nrow(bgdata) < nrow(data)) {
        bgdata <- suppressWarnings(
          dplyr::bind_rows(
            bgdata,
            data[!(data$ID_t %in% bgdata$ID_t),
              "ID_t",
              drop = FALSE
            ]
          )
        )
      }
      bgdata <- bgdata[order(bgdata$ID_t), ]
    } else {
      # append subjects in background data that did not take the competence tests
      data <- suppressWarnings(
        dplyr::bind_rows(data, bgdata[!(bgdata$ID_t %in% data$ID_t),
          "ID_t",
          drop = FALSE
        ])
      )
      data <- data[order(data$ID_t), ]
      bgdata <- suppressWarnings(
        dplyr::bind_rows(
          bgdata,
          data[!(data$ID_t %in% bgdata$ID_t),
            "ID_t",
            drop = FALSE
          ]
        )
      )
      bgdata <- bgdata[order(bgdata$ID_t), ]
    }
    if (include_nr) {
      bgdata <- dplyr::left_join(bgdata, nr, by = "ID_t")
    }
    ID_t <- bgdata[, "ID_t", drop = FALSE]
  }
  list(data = data, bgdata = bgdata, ID_t = ID_t)
}

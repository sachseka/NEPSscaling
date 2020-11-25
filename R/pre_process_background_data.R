#' process background data: combine processing time proxy, add ID_ts if
#' existing in data (-> value of min_valid!), but not in bgdata
#'
#' @param bgdata data.frame or NULL; contains background data and ID_t
#' @param data data.frame; xTargetCompetencies
#' @param include_nr logical; whether not-reached missings are to be used
#' as processing time proxies
#' @param items_not_reached data.frame; number of items not reached per person
#' @param min_valid numeric; minimum number of required responses per person
#'
#' @noRd

pre_process_background_data <- function(bgdata, data, include_nr,
                                        items_not_reached, min_valid) {
  if (!is.null(bgdata)) {
    if (min_valid > 0) {
    # keep only those with competence data in background data
      bgdata <- bgdata[bgdata[["ID_t"]] %in% data[["ID_t"]], ]
    } else {
      # append subjects in background data that did not take the competence tests
      data <- suppressWarnings(
        dplyr::bind_rows(data, bgdata[!(bgdata[["ID_t"]] %in% data[["ID_t"]]),
          "ID_t",
          drop = FALSE
        ])
      )
      data <- data[order(data[["ID_t"]]), ]
    }
    # append subjects in response data who are not present in background data
    if (nrow(bgdata) < nrow(data)) {
      bgdata <- suppressWarnings(
        dplyr::bind_rows(
          bgdata,
          data[!(data[["ID_t"]] %in% bgdata[["ID_t"]]), "ID_t", drop = FALSE]
        )
      )
    }
    bgdata <- bgdata[order(bgdata[["ID_t"]]), ]
    if (any(colSums(!is.na(bgdata)) == 0)) {
      vars <- paste(names(bgdata)[which(colSums(!is.na(bgdata)) == 0)],
                    collapse = ", ")
      message(paste0("The variables ", vars, " in the background data did not",
                     " contain values for the sample. They were dropped."))
      bgdata <- bgdata[, colSums(!is.na(bgdata)) != 0, drop = FALSE]
    }
  }
  ID_t <- data[, "ID_t", drop = FALSE]
  if (include_nr) {
    bgdata <- suppressWarnings(
      dplyr::left_join(if(is.null(bgdata)) {ID_t} else {bgdata},
                       items_not_reached, by = "ID_t"))
  }
  list(data = data, bgdata = bgdata, ID_t = ID_t)
}

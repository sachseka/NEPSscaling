#' not reached missing values as proxy for processing time
#' @noRd

not_reached_as_proxy <- function(include_nr, longitudinal, data, SC, domain,
                                 wave, waves) {
    if (include_nr) {
        if (longitudinal) {
            sel <- lapply(item_labels[[SC]][[domain]],
                          function(it) {names(data) %in% it})
            # in the longitudinal case, missing test taking for later time points
            # causes problems in imputation, if include_nr = TRUE, bgdata = NULL,
            # thus, remove NAs from data
            nr <- data.frame(ID_t = data[["ID_t"]])
            for (s in seq_len(sel)) {
                nr[[paste0("nr", waves[s])]] <-
                    rowSums(data[, sel[s]] == -94, na.rm = TRUE)
            }
            # TODO: SC6 RE w3/w5
            if (SC == "SC6" & domain == "RE") {
                nr[["nr_w3"]][
                    is.na(data[[wle_names[["SC6"]][["RE"]][["w3"]]]])] <- NA
                nr[["nr_w5"]][
                    is.na(data[[wle_names[["SC6"]][["RE"]][["w5"]]]])] <- NA
            }
        } else {
            sel <- names(data) %in% item_labels[[SC]][[domain]][[wave]]
            nr <- data.frame(ID_t = data[["ID_t"]],
                             nr = rowSums(data[, sel] == -94, na.rm = TRUE))
            if (length(unique(nr[["nr"]])) == 1) {
                include_nr <- FALSE
                nr <- NULL
                cat(
                    "The number of not-reached missing values is constant.",
                    "Thus, it is not considered in the background model.\n"
                )
            }
        }
      # set user-defined missings to NA
      data[data < -15] <- NA # assumption: WLEs this low are not to be expected
      return(list(nr = nr, data = data))
    }# set user-defined missings to NA
    data[data < -15] <- NA
    list(nr = NULL, data = data)
}

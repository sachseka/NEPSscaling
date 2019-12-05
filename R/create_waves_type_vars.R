#' Create waves and type variables
#'
#' @param longitudinal logical indicating type of estimation
#' @param SC starting cohort; string
#' @param domain competence domain; string
#'
#' @noRd

create_waves_type_vars <- function(longitudinal, SC, domain) {
    # create auxiliary waves variable for longitudinal estimation
    if (longitudinal) {
        type <- "long"
        if (SC == "SC6") {
            if (domain == "RE") {
                waves <- c("_w3", "_w5", "_w9")
            }
            if (domain == "MA") {
                waves <- c("_w3", "_w9")
            }
        }
        if (SC == "SC5" && domain %in% c("RE", "MA")) {
            waves <- c("_w1", "_w12")
        }
        if (SC == "SC4") {
            if (domain == "RE") {
                waves <- c("_w2", "_w7", "_w10")
            }
            if (domain == "MA") {
                waves <- c("_w1", "_w7", "_w10")
            }
            if (domain == "IC") {
                waves <- c("_w1", "_w7")
            }
            if (domain == "SC") {
                waves <- c("_w1", "_w5")
            }
            if (domain == "EF") {
                waves <- c("_w3", "_w7")
            }
        }
    } else {
        type <- "cross"
        waves <- paste0("_", wave)
    }
    list(type = type, waves = waves)
}

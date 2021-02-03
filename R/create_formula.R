#' create formula for latent regression
#'
#' @param dat data.frame with independent variables
#'
#' @noRd

create_formula <- function(dat) {
  as.formula(
    paste("~", paste(colnames(dat[, -which(colnames(dat) == "ID_t"), drop = F]),
                     collapse = "+"))
  )
}

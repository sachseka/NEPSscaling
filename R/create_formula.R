#' create formula for latent regression
#'
#' @param dat data.frame with independent variables
#'
#' @return R formula adding all variables in the data.frame (except ID_t)
#' @noRd

create_formula <- function(dat) {
  as.formula(
    paste("~", paste(colnames(dat[, -which(colnames(dat) == "ID_t"), drop = F]),
                     collapse = "+"))
  )
}

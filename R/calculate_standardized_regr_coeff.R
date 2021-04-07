#' add standardized regression coefficients
#'
#' @param regr.coeff (list of) data.frame(s) with regression coefficients;
#' cross: Variable impX_coeff impX_se impY_coeff impY_se
#' long: [[1]] Variable coeff_wX se_wX coeff_wY se_wY
#' @param datalist list of imputed data frames + PVs
#' @param longitudinal logical
#' @param waves character vector c("_wX", "_wY")
#'
#' @noRd
#'
calculate_standardized_regr_coeff <- function(regr.coeff, datalist,
                                              longitudinal, waves) {
  used_imp <- determine_used_imputations(datalist)

  sdX <- lapply(datalist, function(dat) {
    apply(
      model.matrix(~., dat[, !grepl("ID_t|PV", names(dat)), drop = FALSE])[, -1],
      2, sd, na.rm = TRUE)
  })
  sdY <- lapply(datalist, function(dat) {
    apply(dat[, grepl("PV", names(dat)), drop = FALSE], 2, sd, na.rm = TRUE)
  })

  if (longitudinal) {
    regr_coeff_std <- replicate(length(datalist),
                                data.frame(Variable = regr.coeff[[1]]$Variable),
                                simplify = FALSE)
    names(regr_coeff_std) <- names(datalist)

    for (u in used_imp[[2]]) {
      tmp_datalist <- datalist[grepl(u, names(datalist))]
      for (d in 1:length(tmp_datalist)) {
        nms <- names(tmp_datalist)[d]
        tmp_coeff <- regr.coeff[[u]]
        for (w in waves) {
          tmp_coeff <- tibble::add_column(
            tmp_coeff,
            x = c(NA, standardized_coeff(tmp_coeff[-1, paste0("coeff", w)], sdX[[nms]], sdY[[nms]])),
            .after = paste0("coeff", w)
          )
        }
        names(tmp_coeff) <-
          c("Variable", paste0(c("coeff", "coeff_std", "se"), rep(waves, each = 3)))
        regr_coeff_std[[nms]] <- dplyr::left_join(regr_coeff_std[[nms]],
                                                  tmp_coeff, by = "Variable")
      }
    }

    return(regr_coeff_std)
  }

  regr_coeff_std <- regr.coeff[, "Variable", drop = FALSE]
  for (u in used_imp[[2]]) {
    tmp_datalist <- datalist[grepl(u, names(datalist))]
    for (d in 1:length(tmp_datalist)) {
      nms <- names(tmp_datalist)[d]
      tmp_coeff <- regr.coeff[, grepl(u, names(regr.coeff))]
      tmp_coeff <- tibble::add_column(
        tmp_coeff,
        x = c(NA, standardized_coeff(tmp_coeff[-1, 1], sdX[[nms]], sdY[[nms]])),
        .after = paste0(u, "_coeff")
      )
      names(tmp_coeff) <- paste0(nms, c("_coeff", "_coeff_std", "_se"))
      regr_coeff_std <- dplyr::left_join(regr_coeff_std,
                                         tmp_coeff %>% tibble::rownames_to_column(),
                                         by = c("Variable" = "rowname"))
    }
  }
  regr_coeff_std
}

standardized_coeff <- function(b, sdX, sdY) {
  b * sdX / sdY
}

# --cross:
# Variable impX_coeff impX_coeff_std impX_se impY_coeff impY_coeff_std impY_se

# --long:
# impA
# Variable coeff_wX coeff_std_wX se_wX coeff_wY coeff_std_wY se_wY
# impB
# Variable coeff_wX coeff_std_wX se_wX coeff_wY coeff_std_wY se_wY




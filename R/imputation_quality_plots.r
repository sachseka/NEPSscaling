#' Plots for multiple imputation
#'
#' The plots show the distribution of the original and imputed data sets per
#' variable. The plots are analogous to the xyplot, densityplot, stripplot and
#' bwplot of the mice package.
#'
#' @param pv_obj pv_obj by plausible_values() containing at least the sublists
#'   "pv" with the imputed data sets and "indmis", the indicator matrix for
#'   missing variables
#' @param var character value denoting the variable against which to plot the
#'   distribution of the imputed variables
#'
#' @return ggplot2 objects
#' @export
xyplot <- function(pv_obj, var) {
  dat <- reshape_data(pv_obj)
  ggplot2::ggplot(
    data = dat %>% dplyr::filter(dat$variable != var),
    mapping = ggplot2::aes(x = .data$value,
                           y = rep(dat[dat$variable == var, "variable"],
                                   length(unique(dat$variable)) - 1),
                           color = .data$missing,
                           shape = .data$imputation)
  ) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(.~.data$variable)
}

#' @export
densityplot <- function(pv_obj) {
  dat <- reshape_data(pv_obj)
  ggplot2::ggplot(
    data = dat,
    mapping = ggplot2::aes(x = .data$value,
                           color = .data$missing,
                           shape = .data$imputation)
  ) +
  ggplot2::geom_density() +
  ggplot2::facet_wrap(.~.data$variable)
}

#' @export
stripplot <- function(pv_obj) {
  dat <- reshape_data(pv_obj)
  ggplot2::ggplot(
    data = dat,
    mapping = ggplot2::aes(x = .data$imputation,
                           y = .data$value,
                           color = .data$missing)
  ) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(.~.data$variable)
}

#' @export
bwplot <- function(pv_obj) {
  dat <- reshape_data(pv_obj)
  ggplot2::ggplot(
    data = dat,
    mapping = ggplot2::aes(x = .data$missing,
                           y = .data$value,
                           color = .data$imputation)
  ) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(.~.data$variable)
}

#' @noRd
reshape_data <- function(pv_obj) {
  dat1 <- lapply(1:length(pv_obj[["pv"]]), function(x) {
    pv_obj[["pv"]][[x]] %>%
      dplyr::select(!dplyr::contains("PV")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
      tidyr::pivot_longer(
        cols = !dplyr::contains("ID_t"),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::mutate(imputation = as.character(x))
  })
  dat1 <- dplyr::bind_rows(dat1)
  dat2 <- pv_obj[["indmis"]] %>%
    dplyr::select(!dplyr::contains("PV")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric),
                  ID_t = pv_obj[["pv"]][[1]][["ID_t"]]) %>%
    tidyr::pivot_longer(
      cols = !dplyr::contains("ID_t"),
      names_to = "variable",
      values_to = "missing"
    )
  dat2$missing <- ifelse(dat2$missing == 1, "imputed", "observed")
  dat <- dplyr::left_join(dat2, dat1, by = c("ID_t", "variable"))
  as.data.frame(dat)
}

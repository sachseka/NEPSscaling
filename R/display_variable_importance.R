#' Display variable importance for specific imputation of a variable
#'
#' @param pv_obj pv_obj by plausible_values()
#' @param imputation name or index of imputation (e.g., 1, or "imp3")
#' @param variable specific imputed variable for which the predictor variable
#' importance shall be displayed
#'
#' @return barplot of feature importance for imputed variable
#' @export

display_variable_importance <- function(pv_obj, imputation, variable) {
  pv_obj[["variable_importance"]][[imputation]][[variable]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename("Variable" = "rowname", "Importance" = ".") %>%
    dplyr::arrange(.data[["Importance"]]) %>%
    dplyr::mutate(Variable = forcats::fct_inorder(.data[["Variable"]])) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data[["Variable"]], 
                                           y = .data[["Importance"]])) +
    ggplot2::geom_col(col = "black", show.legend = F) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_grey()

}

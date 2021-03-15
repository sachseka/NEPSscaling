#' Display variable importance for specific imputation of a variable
#'
#' @param pv_obj pv_obj by plausible_values()
#' @param imputation name or index of imputation (e.g., 1, or "imp3")
#' @param variable specific imputed variable for which the predictor variable
#' importance shall be displayed
#'
#' @return ggplot2 plot

display_variable_importance <- function(pv_obj, imputation, variable) {
  pv_obj[["variable_importance"]][[imputation]][[variable]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename("variable" = "rowname", "importance" = ".") %>%
    dplyr::arrange(importance) %>%
    dplyr::mutate(variable = forcats::fct_inorder(variable)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = variable, y = importance)) +
    ggplot2::geom_col(col = "black", show.legend = F) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_grey()

}

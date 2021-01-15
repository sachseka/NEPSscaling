#' Create plot of tree structure of impuation with CART
#'
#' @param tree rpart object
#' @param variable string; name of imputed variable
#' @return ggplot2 plot object
#' @noRd

create_tree_plot <- function(tree, variable) {
  rn <- rownames(tree$splits)
  tmp <- as.data.frame(tree$splits)
  tmp$var <- rn
  tmp$split <- paste0(ifelse(tmp$ncat == -1, "<", ">="), tmp$index)

  dat <- data.frame(nodeID = as.numeric(rownames(tree$frame)))
  dat$parent <- floor(dat$nodeID / 2)
  dat$parent[dat$parent == 0] <- 1
  dat$value <- tree$frame$yval
  dat$level <- floor(log(dat$nodeID, base = 2))
  dat$position <- dat$nodeID - 2^dat$level + 1
  dat$y <- 1 - (1 / (floor(log(max(dat$nodeID), base = 2)) + 1)) * dat$level
  dat$x <- (1 / (2^dat$level + 1)) * dat$position
  dat$var <- tree$frame$var
  dat$n <- tree$frame$n
  dat$split <- NA

  for (i in 1:nrow(dat)) {
    if (length(tmp$split[tmp$var == dat$var[i] & tmp$count == dat$n[i]]) > 0) {
      dat$split[i] <-
        paste0(dat$var[i],
               tmp$split[tmp$var == dat$var[i] & tmp$count == dat$n[i]])
    }
  }

  dat$split[is.na(dat$split)] <- dat$value[is.na(dat$split)]
  dat$xend <- dat$yend <- 0
  for (i in 1:nrow(dat)) {
    dat$xend[i] <- dat$x[which(dat$parent[i] == dat$nodeID)]
    dat$yend[i] <- dat$y[which(dat$parent[i] == dat$nodeID)]
  }

  p <- ggplot2::ggplot(data = dat, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_segment(ggplot2::aes(xend = xend, yend = yend)) +
    ggplot2::geom_label(ggplot2::aes(label = split)) +
    ggplot2::theme_void() +
    ggplot2::ggtitle(variable)

  p
}

#' Display the tree structure returned by the CART algorithm built to impute
#' one missing variable
#'
#' @param pv_obj An object of class "pv_obj" containing at least the sublist
#' "treeplot"
#' @param imputation Integer or character string value. If it is supplied as an
#' integer it signifies the index of the imputation (ranging from 1 to max.
#' control$ML$nmi) to choose the imputed variable from. As a character string it
#' supplies the name of the imputation (e.g., "imp2").
#' @param variable A character string. Name of the variable to be displayed.
#'
#' @return a ggplot2 plot object representing the tree structure
#'
#' @export

display_tree <- function(pv_obj, imputation, variable) {
  tree <- pv_obj[["treeplot"]][[imputation]][[variable]]

  tree <- strsplit(x = tree, split = "\n")[[1]][-c(1, 2, 4, 5)]
  tree <- lapply(tree, function(x) {strsplit(x, " ")[[1]]})
  tree <- lapply(tree, function(x) {x[x != ""]})
  tree <- lapply(tree, function(x) {
    if (length(which(grepl("<", x))) > 0) {
      x[which(grepl("<", x))] <-
        paste(x[which(grepl("<", x))], x[which(grepl("<", x)) + 1])
      x[which(grepl("<", x)) + 1] <- NA

      x <- x[!is.na(x)]
    }
    x
  })
  tree[[1]] <- c(tree[[1]], "leaf")
  tree <- lapply(tree, function(x) {gsub(")|,", "", x)})
  names(tree) <- sapply(tree, function(x) {x[1]})
  cn <- tree[[1]]
  tree[[1]] <- NULL
  tree <- lapply(tree, function(x) {
    if (length(x) == 5) {
      c(x, "")
    } else {
      x
    }
  })

  dat <- as.data.frame(do.call(rbind, tree))
  names(dat) <- cn
  dat$label <- paste0(dat$split, "\n(n = ", dat$n, ")")

  dat$parent <- floor(as.numeric(dat$node) / 2)
  dat$parent[dat$parent == 0] <- 1
  dat$level <- floor(log(as.numeric(dat$node), base = 2))
  dat$position <- as.numeric(dat$node) - 2^dat$level + 1
  dat$y <- 1 - (1 / (floor(log(max(as.numeric(dat$node)), base = 2)) + 1)) * dat$level
  dat$x <- (1 / (2^dat$level + 1)) * dat$position
  dat$xend <- dat$yend <- 0
  for (i in 1:nrow(dat)) {
    dat$xend[i] <- dat$x[which(dat$parent[i] == dat$node)]
    dat$yend[i] <- dat$y[which(dat$parent[i] == dat$node)]
  }

  p <- ggplot2::ggplot(data = dat, mapping = ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point() +
    ggplot2::geom_segment(ggplot2::aes(xend = .data$xend, yend = .data$yend)) +
    ggplot2::geom_label(ggplot2::aes(label = .data$label)) +
    ggplot2::theme_void() +
    ggplot2::ggtitle(variable)

  p
}

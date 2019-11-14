# Loading patterns for longitudinal item response models (ML)

qmat <- function(SC, domain) {
    Q <- list()
    for (l in seq(length(item_labels[[SC]][[domain]]))) {
        Q[[l]] <- matrix(data = 1,
                         nrow = length(item_labels[[SC]][[domain]][[l]]),
                         ncol = 1)
    }
    return(Q)
}

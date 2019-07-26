# Loading patterns for longitudinal item response models (ML)

qmat <- function(SC, domain) {
    Q <- list()
    if (SC == "SC6" && domain == "RE") {
        len <- vector("numeric", 2)
        len[1] <- length(item_labels[[SC]][[domain]][[1]])
        len[2] <- length(item_labels[[SC]][[domain]][[3]])
    } else {
        len <- vector("numeric", length(item_labels[[SC]][[domain]]))
        for (l in seq(length(len))) {
            len[l] <- length(item_labels[[SC]][[domain]][[l]])
        }
    }
    for (i in seq(length(len))) {
        Q[[i]] <- matrix(data = 1, nrow = len[i], ncol = 1)
    }
    return(Q)
}

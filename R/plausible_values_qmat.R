# Loading patterns for longitudinal
# item response models (ML)

qmat <- function(SC, domain) {
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
    Q <- matrix(data = 0, nrow = sum(len), ncol = length(len))
    Q[1:len[1], 1] <- 1
    Q[(sum(len[-length(len)])+1):sum(len), length(len)] <- 1
    if (SC == "SC4" && domain %in% c("RE", "MA")) { # three test points
        Q[(len[1]+1):sum(len[-length(len)]), 2] <- 1
    }
    return(Q)
}

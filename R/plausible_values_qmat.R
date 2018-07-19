# Loading patterns for longitudinal
# item response models (ML)

qmat <- function(SC, domain) {
    if (SC == "SC6") {
        if (domain == "RE") {
            Q <- matrix(data = 0, nrow = (length(item_labels[[SC]][[domain]][[1]])+length(item_labels[[SC]][[domain]][[3]])), ncol = 2)
            Q[1:length(item_labels[[SC]][[domain]][[1]]), 1] <- 1
            Q[(length(item_labels[[SC]][[domain]][[1]])+1):nrow(Q), 2] <- 1
        } else if (domain == "MA") {
            Q <- matrix(data = 0, nrow = (length(item_labels[[SC]][[domain]][[1]])+length(item_labels[[SC]][[domain]][[2]])), ncol = 2)
            Q[1:length(item_labels[[SC]][[domain]][[1]]), 1] <- 1
            Q[(length(item_labels[[SC]][[domain]][[1]])+1):nrow(Q), 2] <- 1
        } else if (domain %in% c("SC","IC")) {
            Q <- matrix(data = 1, nrow = length(item_labels[[SC]][[domain]][[1]]), ncol = 1)
        }
    } else if (SC == "SC5") {
        if (domain == "RE") {
            Q <- matrix(data = 0, nrow = (length(item_labels[[SC]][[domain]][[1]])+length(item_labels[[SC]][[domain]][[2]])), ncol = 2)
            Q[1:length(item_labels[[SC]][[domain]][[1]]), 1] <- 1
            Q[(length(item_labels[[SC]][[domain]][[1]])+1):nrow(Q), 2] <- 1
        } else if (domain == "MA") {
            Q <- matrix(data = 0, nrow = (length(item_labels[[SC]][[domain]][[1]])+length(item_labels[[SC]][[domain]][[2]])), ncol = 2)
            Q[1:length(item_labels[[SC]][[domain]][[1]]), 1] <- 1
            Q[(length(item_labels[[SC]][[domain]][[1]])+1):nrow(Q), 2] <- 1
        } else if (domain %in% c("SC","IC","BA","EF")) {
            Q <- matrix(data = 1, nrow = length(item_labels[[SC]][[domain]][[1]]), ncol = 1)
        }
    } else if (SC == "SC4") {
        if (domain == "RE") {
            Q <- matrix(data = 0, nrow = (length(item_labels[[SC]][[domain]][[1]])+length(item_labels[[SC]][[domain]][[2]])), ncol = 2)
            Q[1:length(item_labels[[SC]][[domain]][[1]]), 1] <- 1
            Q[(length(item_labels[[SC]][[domain]][[1]])+1):nrow(Q), 2] <- 1
        }
    }
    return(Q)
}

## Dummy coding of factor variables in conditioning
## model.

dummy_coding <- function (Y, categorical, ID_t) {
  domain <- NULL

  # dummy coding
  cate <- ID_t
  for (i in categorical) {
    bar <- Y[[i]]
    tab <- table(bar, useNA = 'always')
    if (sum(is.na(bar)) > 0) {
      a <- matrix(0, nrow = length(bar), ncol = length(tab))
      for (j in 1:(length(tab)-1)) {
        for (k in 1:length(bar)) {
          if (!is.na(bar[k]) & bar[k] == as.numeric(names(tab)[j])) {
            a[k, j] <- 1
          }
        }
      }
      for (k in 1:length(bar)) {
        if (is.na(bar[k])) {
          a[k, length(tab)] <- 1
        }
      }
    } else {
      a <- matrix(0, nrow = length(bar), ncol = length(tab)-1)
      for (j in 1:(length(tab)-1)) {
        for (k in 1:length(bar)) {
          if (!is.na(bar[k]) & bar[k] == as.numeric(names(tab)[j])) {
            a[k, j] <- 1
          }
        }
      }
    }
    colnames(a) <- paste0(i, '.', 0:(ncol(a)-1))
    a <- a[, -1, drop = FALSE]
    cate <- cbind(cate, a)
  }

  return(cate)
}


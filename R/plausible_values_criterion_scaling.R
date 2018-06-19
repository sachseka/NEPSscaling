## Criterion scaling (Beaton, 1969) of numeric
## variables in conditioning model.

criterion_scaling <- function (Y, wle, SC, wave, wle.data, categorical) {
  domain <- NULL

  # criterion scaling
  cont <- as.matrix(merge(Y[, !names(Y) %in% categorical], wle.data))

  for (i in colnames(cont)[!colnames(cont) %in% c('ID_t', wle)]) {
    bar <- cont[, i]
    tab <- table(round(bar, 1))
    mNA <- mean(cont[, wle][is.na(bar)], na.rm = T)
    cont[is.na(bar), i] <- mNA
    for (j in as.numeric(names(tab))) {
      m <- mean(cont[, wle][bar == j], na.rm = T)
      for (k in 1:length(bar)) {
        if (!is.na(bar[k]) & bar[k] == j) {
          if (!is.na(bar[k]) & !is.nan(m)) {
            cont[k, i] <- m
          } else if (!is.na(bar[k]) & is.nan(m)) {
            cont[k, i] <- 0
          }
        }
      }
    }
  }
  cont <- as.data.frame(cont)

  return(cont)
}


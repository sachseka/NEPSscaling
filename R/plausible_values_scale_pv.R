# Re-scale plausible values to
# fit linked distributions

scale_pv <- function(pv, SC, domain, type, MEAN, VAR, wave) {
    if (SC == "SC6") {
        if (domain == "RE" & type == "long")
            waves <- c("w3", "w12")
    }
    if (SC == "SC4") {
        if (domain == "RE" & type == "long")
            waves <- c("w2", "w7")
    }
    if (length(MEAN) == 1) {
        m <- meanvar[[SC]][[domain]][[wave]][[type]][1]
        s <- sqrt(meanvar[[SC]][[domain]][[wave]][[type]][2])
        for(i in seq(length(pv))) {
            pv[[i]]$PV <- (pv[[i]]$PV + m - MEAN) / sqrt(VAR) * s
        }
        return(pv)
    }
    for(i in seq(length(pv))) {
        for (w in seq(length(waves))) {
            m <- meanvar[[SC]][[domain]][[waves[w]]][[type]][1]
            s <- sqrt(meanvar[[SC]][[domain]][[waves[w]]][[type]][2])
            pv[[i]][[paste0("PV_",waves[w])]] <- (pv[[i]][[paste0("PV_",waves[w])]] + m - MEAN[[waves[w]]]) / sqrt(VAR[[waves[w]]]) * s
        }
    }
    return(pv)
}

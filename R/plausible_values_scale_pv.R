# Re-scale plausible values to
# fit linked distributions

scale_pv <- function(pv, SC, domain, type, MEAN, VAR, wave, ID) {
    # if (SC == "SC6") {
    #     if (domain == "RE" & type == "long")
    #         waves <- c("w3", "w12")
    # }
    # if (SC == "SC4") {
    #     if (domain == "RE" & type == "long")
    #         waves <- c("w2", "w7")
    # }
    # if (length(MEAN) == 1) {
    #     m <- meanvar[[SC]][[domain]][[wave]][[type]][1]
    #     # s <- sqrt(meanvar[[SC]][[domain]][[wave]][[type]][2])
    #     for(i in seq(length(pv))) {
    #         pv[[i]]$PV <- (pv[[i]]$PV + m - MEAN)# / sqrt(VAR) * s
    #     }
    #     return(pv)
    # }
    for(i in seq(length(pv))) {
        for (w in seq(length(wave))) {
            m <- meanvar[[SC]][[domain]][[wave[w]]][1]#[[type]][1]
            # s <- sqrt(meanvar[[SC]][[domain]][[waves[w]]][[type]][2])
            pv[[i]][pv[[i]]$ID_t %in% ID, paste0("PV_",wave[w])] <- pv[[i]][pv[[i]]$ID_t %in% ID, paste0("PV_",wave[w])] - MEAN[[wave[w]]]
            pv[[i]][[paste0("PV_",wave[w])]] <- pv[[i]][[paste0("PV_",wave[w])]] + m + correction[[SC]][[domain]][[wave[w]]] #- MEAN[[wave[w]]])# / sqrt(VAR[[waves[w]]]) * s
        }
    }
    return(pv)
}

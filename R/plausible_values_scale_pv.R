# Re-scale plausible values to
# fit linked distributions

scale_pv <- function(pv, SC, domain, type, MEAN, VAR, wave, ID) {
    for(i in seq(length(pv))) {
        for (w in seq(length(wave))) {
            m <- meanvar[[SC]][[domain]][[wave[w]]][1]#[[type]][1]
            # s <- sqrt(meanvar[[SC]][[domain]][[waves[w]]][[type]][2])
            pv[[i]][pv[[i]]$ID_t %in% ID, paste0("PV_",wave[w])] <-
                pv[[i]][pv[[i]]$ID_t %in% ID, paste0("PV_",wave[w])] -
                MEAN[[wave[w]]]
            pv[[i]][[paste0("PV_",wave[w])]] <-
                pv[[i]][[paste0("PV_",wave[w])]] + m +
                correction[[SC]][[domain]][[wave[w]]] #- MEAN[[wave[w]]])# / sqrt(VAR[[waves[w]]]) * s
        }
    }
    return(pv)
}

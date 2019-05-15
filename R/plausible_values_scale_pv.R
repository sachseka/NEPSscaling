# Re-scale plausible values to fit linked distributions:
# The mean difference of the longitudinal sample must be the linking constant

scale_pv <- function(pv, SC, domain, type, MEAN, VAR, wave, ID) {
    for(w in seq(2,length(wave))) {
        if (SC == "SC6" && domain == "RE") {
            term1 <- MEAN[[wave[w]]] -
                MEAN[[wave[w-1]]] -
                meanvar[[SC]][[domain]][[wave[w]]][["B67"]][1]
            term2 <- MEAN[[wave[w]]] -
                MEAN[[wave[w-1]]] -
                meanvar[[SC]][[domain]][[wave[w]]][["B69"]][1]
            for (i in seq(length(pv))) {
                pv[[i]][pv[[i]]$ID_t %in% ID[["w3"]], paste0("PV_",wave[w])] <-
                    pv[[i]][pv[[i]]$ID_t %in% ID[["w3"]], paste0("PV_",wave[w])] - term1
                pv[[i]][pv[[i]]$ID_t %in% ID[["w5"]], paste0("PV_",wave[w])] <-
                    pv[[i]][pv[[i]]$ID_t %in% ID[["w5"]], paste0("PV_",wave[w])] - term2
            }
        } else {
            term <- MEAN[[wave[w]]] -
                    MEAN[[wave[w-1]]] -
                    meanvar[[SC]][[domain]][[wave[w]]][1]
            for (i in seq(length(pv))) {
                pv[[i]][[paste0("PV_",wave[w])]] <-
                    pv[[i]][[paste0("PV_",wave[w])]] - term
            }
        }
    }
    return(pv)
}

# Re-scale plausible values to
# fit linked distributions

scale_pv <- function(pv, method, SC, domain, type, EAP, wave) {
    if (SC == "SC6") {
        if (domain == "RE" & type == "long")
            waves <- c("w3", "w12")
        else if (domain == "RE" & type == "cross")
            waves <- wave
    }
    for(i in seq(length(pv))) {
        # TODO: subtract mean of plausible values distribution:
        # does not work for multi-dim case yet!
        m <- 0
        for (w in waves)
            m <- m + meanvar[[SC]][[domain]][[w]][[type]][1]
        pv[[i]]$PV <- (pv[[i]]$PV + m - EAP)/
            sqrt(meanvar[[SC]][[domain]][[wave]][[type]][2])
    }
    return(pv)
}


# in PVPIAACL
# finalweight12 <- Xid
# finalweight12 <- merge(finalweight12,
#                        ZA5845[, c("seqid", "SPFWT0")], by = "seqid")[, -1]
# PVsLit12_SUF <- Xid
# PVsLit12_SUF <- merge(PVsLit12_SUF,
#                       ZA5845[, c("seqid", grep("PVLIT", names(ZA5845), value = TRUE))],
#                       by = "seqid")[, -1]
# PVsNum12_SUF <- Xid
# PVsNum12_SUF <- merge(PVsNum12_SUF,
#                       ZA5845[, c("seqid", grep("PVNUM", names(ZA5845), value = TRUE))],
#                       by = "seqid")[, -1]
# GMLit12_SUF <- mean(apply(PVsLit12_SUF, 2, wtd.mean, weights = finalweight12))
# GSdLit12_SUF <- mean(sqrt(apply(PVsLit12_SUF, 2, wtd.var,
#                                 weights = finalweight12)))
# GMNum12_SUF <- mean(apply(PVsNum12_SUF, 2, wtd.mean,
#                           weights = finalweight12))
# GSdNum12_SUF <- mean(sqrt(apply(PVsNum12_SUF, 2, wtd.var,
#                                 weights = finalweight12)))
# PVsLit12_est <- sapply(PVs, `[[`, 2)
# PVsNum12_est <- sapply(PVs, `[[`, 3)
# PVsLit15_est <- sapply(PVs, `[[`, 4)
# PVsNum15_est <- sapply(PVs, `[[`, 5)
# GMLit12_est <- mean(apply(PVsLit12_est, 2, wtd.mean, weights = finalweight12))
# GSdLit12_est <- mean(sqrt(apply(PVsLit12_est, 2, wtd.var, weights = finalweight12)))
# GMNum12_est <- mean(apply(PVsNum12_est, 2, wtd.mean, weights = finalweight12))
# GSdNum12_est <- mean(sqrt(apply(PVsNum12_est, 2, wtd.var, weights = finalweight12)))
# ALit <- GSdLit12_SUF/GSdLit12_est
# BLit <- -GMLit12_est*ALit + GMLit12_SUF
# ANum <- GSdNum12_SUF/GSdNum12_est
# BNum <- -GMNum12_est*ANum + GMNum12_SUF
# PVsLit12_est_t <- apply(PVsLit12_est, 2, function(x) x*ALit + BLit)
# PVsNum12_est_t <- apply(PVsNum12_est, 2, function(x) x*ANum + BNum)
# PVsLit15_est_t <- apply(PVsLit15_est, 2, function(x) x*ALit + BLit)
# PVsNum15_est_t <- apply(PVsNum15_est, 2, function(x) x*ANum + BNum)
# for(pv in 1:nopvs){
#     PVs[[pv]][, 2] <- PVsLit12_est_t[, pv]
#     PVs[[pv]][, 3] <- PVsNum12_est_t[, pv]
#     PVs[[pv]][, 4] <- PVsLit15_est_t[, pv]
#     PVs[[pv]][, 5] <- PVsNum15_est_t[, pv]
#     save.dta13(data = PVs[[pv]], file = paste0(path, "litnum1215_", pv, ".dta"))
# }

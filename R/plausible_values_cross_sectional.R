# estimation of cross-sectional plausible values (polytomous data)


cross_sectional <- function (bgdata, imp, resp, waves, frmY = NULL,
                                 ID_t, type, domain, SC, control, npv) {
    pvs <- list(NULL)
    EAP.rel <- NULL
    eap <-
        replicate(
            ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1, control$ML$nmi),
            data.frame(ID_t = ID_t$ID_t), simplify = FALSE)
    for (i in 1:ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1,
                       control$ML$nmi)) {
        if (!is.null(imp)) {
            bgdatacom <- imp[[i]]
            bgdatacom <- as.data.frame(apply(bgdatacom, 2, as.numeric))
            frmY <-
                as.formula(paste("~",
                paste(colnames(bgdatacom)[-which(names(bgdatacom) == "ID_t")],
                       collapse = "+")))
        }
        # estimate IRT model
        mod <- list()

        mod[[1]] <- TAM::tam.mml(
            resp = resp[, item_labels[[SC]][[domain]][[gsub("_", "", waves)]]],
            dataY = if (is.null(bgdata)) {
                NULL
            } else if (is.null(imp)) {
                bgdata[
                    bgdata$ID_t %in% resp$ID_t,
                    -which(names(bgdata) == "ID_t"), drop = FALSE]
            } else {
                bgdatacom[
                    bgdatacom$ID_t %in% resp$ID_t,
                    -which(names(bgdatacom) == "ID_t"), drop = FALSE]
            },
            formulaY = frmY,
            pid = resp$ID_t,
            irtmodel = "1PL",
            xsi.fixed = xsi.fixed[[type]][[domain]][[SC]][[gsub("_", "",
                                                                waves) ]],
            verbose = FALSE)
        # post-processing of model
        res <- cross_sectional_post_proc(mod[[1]], npv, control,
            Y = if (is.null(bgdata)) {NULL} else if (is.null(imp)) {bgdata} else {bgdatacom},
            Y.pid = if (is.null(bgdata)) {NULL} else {"ID_t"},
            eap, i, EAP.rel, regr.coeff, pvs, bgdata)
        eap <- res$eap
        regr.coeff <- res$regr.coeff
        pvs <- res$pvs
        mod[[1]] <- res$mod
        EAP.rel <- res$EAP.rel
    }
    res <- list(eap = eap, pvs = pvs, mod = mod, EAP.rel = EAP.rel,
                regr.coeff = regr.coeff)
    return(res)
}

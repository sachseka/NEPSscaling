context("estimate_cross_rasch_corrected_for_rotation")


# estimate_cross_rasch_corrected_for_rotation <- function(bgdata, imp,
#                                                         frmY = NULL, resp,
#                                                         position, waves,
#                                                         ID_t, type, domain,
#                                                         SC, control, npv) {
#   times <- ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1, control$ML$nmi)
#   pvs <- list(NULL)
#   EAP.rel <- NULL
#   eap <- replicate(times, data.frame(ID_t = ID_t$ID_t), simplify = FALSE)
#   for (i in 1:times) {
#     res <- prepare_bgdata_frmY(imp, i, frmY)
#     bgdatacom <- res[["bgdatacom"]]
#     frmY <- res[["frmY"]]
#     # estimate IRT model
#     mod <- list()
#
#     items <- rownames(xsi.fixed$cross[[domain]][[SC]][[gsub("_", "", waves)]])
#     mod[[1]] <- TAM::tam.mml.mfr(
#       resp = resp[, items],
#       facets = position,
#       formulaA = ~ 0 + item + position,
#       dataY = if (is.null(bgdata)) {
#         NULL
#       } else if (is.null(imp)) {
#         bgdata[
#           bgdata$ID_t %in% resp$ID_t,
#           -which(names(bgdata) == "ID_t"),
#           drop = FALSE
#         ]
#       } else {
#         bgdatacom[
#           bgdatacom$ID_t %in% resp$ID_t,
#           -which(names(bgdatacom) == "ID_t"),
#           drop = FALSE
#         ]
#       },
#       formulaY = frmY,
#       pid = resp$ID_t,
#       irtmodel = "1PL",
#       xsi.fixed = xsi.fixed[[type]][[domain]][[SC]][[gsub(
#         "_", "",
#         waves
#       ) ]],
#       verbose = FALSE
#     )
#     # post-processing of model
#     res <- post_process_cross_tam_results(mod[[1]], npv, control,
#                                           imp, bgdatacom, eap, i, EAP.rel, regr.coeff, pvs, bgdata
#     )
#     eap <- res$eap
#     regr.coeff <- res$regr.coeff
#     pvs <- res$pvs
#     EAP.rel <- res$EAP.rel
#   }
#   res <- list(
#     eap = eap, pvs = pvs, mod = mod, EAP.rel = EAP.rel,
#     regr.coeff = regr.coeff
#   )
#   res
# }

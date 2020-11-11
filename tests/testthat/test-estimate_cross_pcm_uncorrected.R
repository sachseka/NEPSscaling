context("estimate_cross_pcm_uncorrected")



# estimate_cross_pcm_uncorrected <- function(bgdata, imp, resp,
#                                            waves, frmY = NULL,
#                                            ID_t, type, domain,
#                                            SC, control, npv) {
#   items <- rownames(xsi.fixed$cross[[domain]][[SC]][[gsub("_", "", waves)]])
#   res <- prepare_resp_b_cross(resp, items, waves, SC, domain)
#   resp <- res[["resp"]]
#   B <- res[["B"]]
#
#   times <- ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1, control$ML$nmi)
#   pvs <- list(NULL)
#   EAP.rel <- NULL
#   eap <- replicate(times, data.frame(ID_t = ID_t$ID_t), simplify = FALSE)
#   for (i in 1:times) {
#     res <- prepare_bgdata_frmY(imp, i, frmY)
#     bgdatacom <- res[["bgdatacom"]]
#     frmY <- res[["frmY"]]
#
#     # estimate IRT model
#     mod <- list()
#     mod[[1]] <- TAM::tam.mml(
#       resp = resp[, items],
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
#       irtmodel = "PCM2",
#       xsi.fixed = xsi.fixed[[type]][[domain]][[SC]][[gsub(
#         "_", "",
#         waves
#       ) ]],
#       B = B, verbose = FALSE
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

#' info for splitting items for both cross-sectional and longitudinal estimation
#'
#' @param data data.frame; xTargetCompetencies etc.
#' @param SC character; starting cohort ("SCx")
#' @param domain character; abbr. of competence domain (e.g. "MA")
#'
#' @return data.frame with columns denoting the DIF group membership of the
#' participants
#' @noRd

get_item_split_info <- function(SC, domain, data) {
  #testletSetting <- NULL
  #if (SC == "SC6" & domain == "SC"){
  #  testletSetting <- data.frame(
  #    ID_t = data[["ID_t"]],
  #    female = ifelse(data[["tx80501"]] %in% 2, TRUE,
  #                    ifelse(is.na(data[["tx80501"]]), NA, FALSE))
  #  )
  #}
  if (SC == "SC4" & domain == "MA") {
    testletSetting <- data.frame(
      ID_t = data[["ID_t"]],
      difficultTestlet = ifelse(
        data[["tx80211_w7"]] %in% c(285, 288, 291:293, 296:303), TRUE,
        ifelse(is.na(data[["tx80211_w7"]]), NA, FALSE)),
      atHome = ifelse(data[["tx80211_w7"]] %in% c(281:295), TRUE,
                      ifelse(is.na(data[["tx80211_w7"]]), NA, FALSE))
    )
  }
  #if (SC == "SC4" & domain == "SC"){
  #  #testletSetting<- NEPSscaling:::gender_info_SC4_SC_w14
  #  testletSetting<-gender_info_SC4_SC_w14
  #  }
  if (SC == "SC2" & domain == "RE") {
    testletSetting <- data.frame(
      ID_t = data[["ID_t"]],
      easy = ifelse(
        data[["tx80211_w9"]] %in% c(751, 753, 757), TRUE,
        ifelse(is.na(data[["tx80211_w9"]]), NA, FALSE))
    )
  }
  testletSetting
}

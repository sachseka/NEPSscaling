#' info for splitting items for both cross-sectional and longitudinal estimation
#'
#' @param data data.frame; xTargetCompetencies etc.
#' @param SC character; starting cohort ("SCx")
#' @param domain character; abbr. of competence domain (e.g. "MA")
#'
#' @noRd

get_item_split_info <- function(SC, domain, data) {
  testletSetting <- NULL
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

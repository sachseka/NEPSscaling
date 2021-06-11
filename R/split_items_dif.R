#' If large DIF occurred during the original scaling procedure, items may have
#' been split accordingly
#' @param testletSetting data.frame; contains info about group memberships
#' @param resp data.frame in the cross-sectional case; list of data.frames in the
#' longitudinal case; response data
#' @param longitudinal logical; whether estimation is to be longitudinal
#' @param wave character of form "wx" where x denotes the assessment wave;
#' indicates wave with split in cross-sectional case because there is no
#' selection prior to here
#'
#' @return (in the longitudinal case, a list of) data.frame of responses with
#' a fixed number of items split by a DIF criterion
#' @noRd
split_SC4_math_items <- function(testletSetting, resp, longitudinal, wave) {
  if (longitudinal) {
    resp[[2]] <- tibble::add_column(resp[[2]],
      mag9d201_sc4g12_c_g = ifelse(
        !testletSetting[["atHome"]][
          testletSetting[["ID_t"]] %in% resp[[2]][["ID_t"]]
        ], resp[[2]][["mag9d201_sc4g12_c"]], NA
      ),
      .after = "mag9d201_sc4g12_c"
    )
    resp[[2]] <- tibble::add_column(resp[[2]],
      mag9d201_sc4g12_c_i = ifelse(
        testletSetting[["atHome"]][
          testletSetting[["ID_t"]] %in% resp[[2]][["ID_t"]]
        ], resp[[2]][["mag9d201_sc4g12_c"]], NA
      ),
      .after = "mag9d201_sc4g12_c_g"
    )
    resp[[2]][["mag9d201_sc4g12_c"]] <- NULL
    resp[[2]] <- tibble::add_column(resp[[2]],
      mag9r051_sc4g12_c_d = ifelse(
        testletSetting[["difficultTestlet"]][
          testletSetting[["ID_t"]] %in% resp[[2]][["ID_t"]]
        ], resp[[2]][["mag9r051_sc4g12_c"]], NA
      ),
      .after = "mag9r051_sc4g12_c"
    )
    resp[[2]] <- tibble::add_column(resp[[2]],
      mag9r051_sc4g12_c_e = ifelse(
        !testletSetting[["difficultTestlet"]][
          testletSetting[["ID_t"]] %in% resp[[2]][["ID_t"]]
        ], resp[[2]][["mag9r051_sc4g12_c"]], NA
      ),
      .after = "mag9r051_sc4g12_c_d"
    )
    resp[[2]][["mag9r051_sc4g12_c"]] <- NULL
  } else {
    if (wave == "w7") {
      resp <- tibble::add_column(resp,
        mag9d201_sc4g12_c_g = ifelse(
          !testletSetting[["atHome"]][
            testletSetting[["ID_t"]] %in% resp[["ID_t"]]
          ], resp[["mag9d201_sc4g12_c"]], NA
        ),
        .after = "mag9d201_sc4g12_c"
      )
      resp <- tibble::add_column(resp,
        mag9d201_sc4g12_c_i = ifelse(
          testletSetting[["atHome"]][
            testletSetting[["ID_t"]] %in% resp[["ID_t"]]
          ], resp[["mag9d201_sc4g12_c"]], NA
        ),
        .after = "mag9d201_sc4g12_c_g"
      )
      resp[["mag9d201_sc4g12_c"]] <- NULL
      resp <- tibble::add_column(resp,
        mag9r051_sc4g12_c_d = ifelse(
          testletSetting[["difficultTestlet"]][
            testletSetting[["ID_t"]] %in% resp[["ID_t"]]
          ], resp[["mag9r051_sc4g12_c"]], NA
        ),
        .after = "mag9r051_sc4g12_c"
      )
      resp <- tibble::add_column(resp,
        mag9r051_sc4g12_c_e = ifelse(
          !testletSetting[["difficultTestlet"]][
            testletSetting[["ID_t"]] %in% resp[["ID_t"]]
          ], resp[["mag9r051_sc4g12_c"]], NA
        ),
        .after = "mag9r051_sc4g12_c_d"
      )
      resp[["mag9r051_sc4g12_c"]] <- NULL
    }
  }
  resp
}

#' If large DIF occurred during the original scaling procedure, items may have
#' been split accordingly
#' @param testletSetting data.frame; contains info about group memberships
#' @param resp data.frame in the cross-sectional case; list of data.frames in the
#' longitudinal case; response data
#' @param longitudinal logical; whether estimation is to be longitudinal
#' @param wave character of form "wx" where x denotes the assessment wave;
#' indicates wave with split in cross-sectional case because there is no
#' selection prior to here
#'
#' @return (in the longitudinal case, a list of) data.frame of responses with
#' a fixed number of items split by a DIF criterion
#' @noRd
split_SC2_reading_items <- function(testletSetting, resp, longitudinal, wave) {
  if (longitudinal) {
    resp[[2]] <- tibble::add_column(resp[[2]],
                                    reg7024s_sc2g7_c_d = ifelse(
                                      !testletSetting[["easy"]][
                                        testletSetting[["ID_t"]] %in% resp[[2]][["ID_t"]]
                                      ], resp[[2]][["reg7024s_sc2g7_c"]], NA
                                    ),
                                    .after = "reg7024s_sc2g7_c"
    )
    resp[[2]][["reg7024s_sc2g7_c"]] <- ifelse(
      testletSetting[["easy"]][
        testletSetting[["ID_t"]] %in% resp[[2]][["ID_t"]]
      ], resp[[2]][["reg7024s_sc2g7_c"]], NA
    )
    resp[[2]] <- tibble::add_column(resp[[2]],
                                    reg7033s_sc2g7_c_d = ifelse(
                                      !testletSetting[["easy"]][
                                        testletSetting[["ID_t"]] %in% resp[[2]][["ID_t"]]
                                      ], resp[[2]][["reg7033s_sc2g7_c"]], NA
                                    ),
                                    .after = "reg7033s_sc2g7_c"
    )
    resp[[2]][["reg7033s_sc2g7_c"]] <- ifelse(
      testletSetting[["easy"]][
        testletSetting[["ID_t"]] %in% resp[[2]][["ID_t"]]
      ], resp[[2]][["reg7033s_sc2g7_c"]], NA
    )
    resp[[2]] <- tibble::add_column(resp[[2]],
                                    reg7045s_sc2g7_c_d = ifelse(
                                      !testletSetting[["easy"]][
                                        testletSetting[["ID_t"]] %in% resp[[2]][["ID_t"]]
                                      ], resp[[2]][["reg7045s_sc2g7_c"]], NA
                                    ),
                                    .after = "reg7045s_sc2g7_c"
    )
    resp[[2]][["reg7045s_sc2g7_c"]] <- ifelse(
      testletSetting[["easy"]][
        testletSetting[["ID_t"]] %in% resp[[2]][["ID_t"]]
      ], resp[[2]][["reg7045s_sc2g7_c"]], NA
    )
  } else {
    if (wave == "w9") {
      resp <- tibble::add_column(resp,
                                 reg7024s_sc2g7_c_d = ifelse(
                                   !testletSetting[["easy"]][
                                     testletSetting[["ID_t"]] %in% resp[["ID_t"]]
                                   ], resp[["reg7024s_sc2g7_c"]], NA
                                 ),
                                 .after = "reg7024s_sc2g7_c"
      )
      resp[["reg7024s_sc2g7_c"]] <- ifelse(
        testletSetting[["easy"]][
          testletSetting[["ID_t"]] %in% resp[["ID_t"]]
        ], resp[["reg7024s_sc2g7_c"]], NA
      )
      resp <- tibble::add_column(resp,
                                 reg7033s_sc2g7_c_d = ifelse(
                                   !testletSetting[["easy"]][
                                     testletSetting[["ID_t"]] %in% resp[["ID_t"]]
                                   ], resp[["reg7033s_sc2g7_c"]], NA
                                 ),
                                 .after = "reg7033s_sc2g7_c"
      )
      resp[["reg7033s_sc2g7_c"]] <- ifelse(
        testletSetting[["easy"]][
          testletSetting[["ID_t"]] %in% resp[["ID_t"]]
        ], resp[["reg7033s_sc2g7_c"]], NA
      )
      resp <- tibble::add_column(resp,
                                 reg7045s_sc2g7_c_d = ifelse(
                                   !testletSetting[["easy"]][
                                     testletSetting[["ID_t"]] %in% resp[["ID_t"]]
                                   ], resp[["reg7045s_sc2g7_c"]], NA
                                 ),
                                 .after = "reg7045s_sc2g7_c"
      )
      resp[["reg7045s_sc2g7_c"]] <- ifelse(
        testletSetting[["easy"]][
          testletSetting[["ID_t"]] %in% resp[["ID_t"]]
        ], resp[["reg7045s_sc2g7_c"]], NA
      )
    }
  }
  resp
}

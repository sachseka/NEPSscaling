
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
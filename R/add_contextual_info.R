#' add contextual information (school average competences)
#'
#' @param path file path leading to NEPS competence data (String)
#' @param SC starting cohort; String
#' @param domain competence domain; String
#' @param bgdata data.frame
#' @param data data.frame
#' @param waves String vector
#'
#' @return bgdata augmented by school competence averages
#' @noRd

add_contextual_info <- function(path, SC, domain, waves, bgdata, data) {
  wle_vnames <- list(
    list( # longitudinal
      SC3 = list(
          MA = c("mag5_sc1u", "mag7_sc1u", "mag9_sc1u"),
          RE = c("reg5_sc1u", "reg7_sc1u", "reg9_sc1u"),
          ORA = c("org5_sc1a", "org7_sc1a", "org9_sc1a"),
          ORB = c("org5_sc1b", "org7_sc1b", "org9_sc1b"),
          SC = c("scg6_sc1", "scg9_sc1u", "scg11_sc1"),
          IC = c("icg6_sc1u", "icg9_sc1u", "icg12_sc1u"),
          NT = c("ntg7_sc1", "ntg9_sc3g9_sc1"),
          NR = c("nrg7_sc1", "nrg9_sc3g9_sc1"),
          LI = c("lig9_sc1u"),
          EF = c("efg10_sc1u", "efg12_sc1u"),
          ST = c("stg12_sc1u")
      ),
      SC4 = list(
          MA = c("mag9_sc1u", "mag12_sc1u"),
          RE = c("reg9_sc1u", "reg12_sc1u"),
          SC = c("scg9_sc1u", "scg11_sc1u"),
          IC = c("icg9_sc1u", "icg12_sc1u"),
          NT = c("ntg9_sc1u"),
          NR = c("nrg9_sc1u"),
          EF = c("efg10_sc1u", "efg12_sc1u"),
          ST = c("stg12_sc1u")
      )
    ),
    list( # cross-sectional
      SC3 = list(
          MA = c(w1 = "mag5_sc1", w3 = "mag7_sc1", w5 = "mag9_sc1"),
          RE = c(w1 = "reg5_sc1", w3 = "reg7_sc1", w6 = "reg9_sc1"),
          ORA = c(w1 = "org5_sc1a", w3 = "org7_sc1a", w5 = "org9_sc1a"),
          ORB = c(w1 = "org5_sc1b", w3 = "org7_sc1b", w5 = "org9_sc1b"),
          SC = c(w2 = "scg6_sc1", w5 = "scg9_sc1", w8 = "scg11_sc1"),
          IC = c(w2 = "icg6_sc1", w5 = "icg9_sc1", w9 = "icg12_sc1"),
          NT = c(w3 = "ntg7_sc1", w6 = "ntg9_sc3g9_sc1"),
          NR = c(w3 = "nrg7_sc1", w6 = "nrg9_sc3g9_sc1"),
          LI = c(w6 = "lig9_sc1u"),
          EF = c(w7 = "efg10_sc1", w9 = "efg12_sc1"),
          ST = c(w9 = "stg12_sc1")
      ),
      SC4 = list(
          MA = c(w1 = "mag9_sc1", w7 = "mag12_sc1"),
          RE = c(w2 = "reg9_sc1", w7 = "reg12_sc1"),
          SC = c(w1 = "scg9_sc1", w5 = "scg11_sc1"),
          IC = c(w1 = "icg9_sc1", w7 = "icg12_sc1"),
          NT = c(w2 = "ntg9_sc1"),
          NR = c(w2 = "nrg9_sc1"),
          EF = c(w3 = "efg10_sc1", w7 = "efg12_sc1"),
          ST = c(w7 = "stg12_sc1u")
      )
    )
  )
  # extract wles from data
  wle_vnames <- if (length(waves) > 1) {
    wle_vnames[[1]][[SC]][[domain]]
  } else {
    wle_vnames[[2]][[SC]][[domain]][[gsub("_", "", waves)]]
  }
  data <- data[, c("ID_t", wle_vnames)]
  waves <- waves[1:length(wle_vnames)]

  # get school id data for SC and domain
  files <- list.files(path = path)
  filepath <- paste0(path, files[grep("CohortProfile", files)])
  filetype <- tools::file_ext(filepath)
  error_msg <- paste0(
      "* Path '", filepath, "' may not contain CohortProfile.\n",
      "* File format: '", filetype, "' might be wrong"
  )
  if (filetype == "sav") {
    school_data <-
      tryCatch(
        haven::read_spss(file = filepath, user_na = FALSE),
        error = function(cnd) {
          stop(cat(error_msg))
        }
      )
  } else {
    school_data <-
      tryCatch(
        haven::read_dta(file = filepath, user_na = FALSE),
        error = function(cnd) {
          stop(cat(error_msg))
        }
      )
  }
  school_data <- dplyr::select(school_data, .data$ID_t, .data$wave, .data$ID_i)
  # missing school id: students did not participate in wave/test
  school_data$ID_i[school_data$ID_i < 0] <- NA

  # match wle to school id, get group average per school
  school_data <- school_data %>%
    tidyr::pivot_wider(names_from = "wave",
                       names_prefix = "school_w",
                       values_from = "ID_i") %>%
    dplyr::mutate_all(as.numeric)
  school_data <- suppressWarnings(
      haven::zap_labels(dplyr::left_join(data, school_data, by = "ID_t"))
  )
  school_waves <-
    names(school_data)[names(school_data) %in% paste0("school", waves)]
  for (i in seq(length(waves))) {
    w <- school_waves[i]
    vn <- wle_vnames[i]
    # NAs: correspond to missing WLEs and are ignored
    for (j in unique(na.omit(school_data[[w]]))) {
      school_data[which(school_data[[w]] == j), vn] <-
        mean(school_data[[vn]][which(school_data[[w]] == j)], na.rm = TRUE)
    }
    names(school_data)[which(names(school_data) == vn)] <- paste0(vn, "_schavg")
  }
  bgdata <- suppressWarnings(
    dplyr::left_join(bgdata,
                     school_data %>%
                       dplyr::select(dplyr::matches("ID_t|_schavg")),
                     by = "ID_t") %>%
    dplyr::arrange(.data$ID_t)
  )

  bgdata
}


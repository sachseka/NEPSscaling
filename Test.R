path<- "Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Plausible Values/SUFs/SC1"

plausible_values(1,"MA",9, path)


path<- "Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Plausible Values/SUFs/SC2/"

plausible_values(2,"MA",9, path,bgdata=NULL)

library(haven)
data <- read_sav("Z:/Projektgruppen_(08)/Kompetenzen_BA_Hiwi_(p000012)/Methoden/Plausible Values/SUFs/SC5/SC5_xTargetCompetencies_D_18-0-0.sav")

resp <- data[, c("ID_t", item_labels[["RE"]][["SC6"]][["w9"]])]
resp <- resp[rowSums(!is.na(resp[, -1])) >= 3, ]
resp <- resp[order(resp[["ID_t"]]), ]
resp

impute_english_competence_data <- function(resp, SC, wave) {
  resp[is.na(resp)] <- 0
  dm <- diffMat[["SC4"]][["w3"]][["diff"]][
    diffMat[["SC4"]][["w3"]][["diff"]][["ID_t"]] %in% resp[["ID_t"]], ]
  setNA <- diffMat[["SC4"]][["w3"]][["ind_NA"]][
    diffMat[["SC4"]][["w3"]][["ind_NA"]][["ID_t"]] %in% resp[["ID_t"]], ]
  resp[, -1] <- resp[, -1] + dm[order(dm[["ID_t"]]), -1]
  for (i in 2:ncol(resp)) {
    resp[setNA[[i]], i] <- NA
  }
  resp
}


SC="2"
wave="9"
domain="RE"
plausible_values <- function(SC,
                             domain,
                             wave,
                             path,
                             bgdata = NULL,
                             npv = 10L,
                             nmi = 10L,
                             longitudinal = FALSE,
                             rotation = TRUE,
                             min_valid = 3L,
                             include_nr = TRUE,
                             verbose = TRUE,
                             adjust_school_context = FALSE,
                             exclude = NULL,
                             seed = NULL,
                             returnAll = FALSE,
                             control = list(
                               EAP = FALSE, WLE = FALSE,
                               ML = list(
                                 ntheta = 2000,
                                 normal.approx = FALSE,
                                 samp.regr = FALSE,
                                 theta.model = FALSE,
                                 np.adj = 8, na.grid = 5,
                                 minbucket = 5,
                                 cp = 0.0001
                               )
                             )) {
  t0 <- Sys.time()

  # check argument validity and apply necessary changes to arguments ----------

  if (missing(SC)) {
    stop("Starting cohort is missing, but must be provided.", call. = FALSE)
  }
  if (!is.numeric(SC)) {
    stop("Starting cohort must be numeric.", call. = FALSE)
  }
  if (missing(wave)) {
    stop(paste0(
      "Wave is missing, but must be provided.\n",
      "Please note that, for longitudinal estimation, you can use any of the ",
      "waves in which the competence was assessed.\n",
      "For example: if the competence was assessed in waves 1, 3, and 5, it ",
      "is fine to use either 1 or 3 or 5."
    ),
    call. = FALSE
    )
  }
  if (!is.numeric(wave)) {
    stop("Wave must be numeric.", call. = FALSE)
  }

  # convert SC and wave to character strings to index internal lists more
  # comfortably
  SC <- paste0("SC", 4)
  wave <- paste0("w", 14)
  domain <- "SC"
  domain <- toupper(domain)
  domain <- match.arg(domain)

  if (missing(path)) {
    stop("Path is missing, but must be provided.", call. = FALSE)
  }
  if (!is.character(path)) {
    stop("Path must be a character string.", call. = FALSE)
  }
  path <- gsub("\\\\", "/", path)
  if (!grepl("/$", path)) {
    path <- paste0(path, "/")
  }
  if (is.null(item_labels[[domain]][[SC]][[wave]])) {
    stop(paste0(
      "There were no competence tests for ", SC, " ", domain, " ",
      wave, ". Please check the NEPS documentation at https://neps-data.de."
    ), call. = FALSE)
  }
  if (longitudinal &&
      (
        #(SC == "SC6" & domain %in% c("IC", "SC")) ||
        (SC == "SC5" & domain %in% c("IC", "SC", "BA", "EF")) ||
        (SC == "SC3" & domain %in% c("ST", "LI")) ||
        (SC == "SC1" & domain %in% c("CD", "SC"))
      )
  ) {
    stop(paste0(
      SC, " ", domain, " was not tested longitudinally.\n",
      "If you wanted to estimate cross-sectional plausible values, ",
      "set longitudinal = FALSE."
    ), call. = FALSE)
  }
  if (longitudinal && SC == "SC1" && domain == "MA" && wave == "w9") {
    stop(paste0("Please note that Starting Cohort 1's science tests can only ",
                "be linked longitudinally up to wave 7. Wave 9 test data does ",
                "not share any common test information and can, thus, not be ",
                "connected to previous assessment waves. It can only be ",
                "analysed cross-sectionally."), call. = FALSE)
  }
  if (longitudinal && SC == "SC2" && domain == "SC" && wave == "w9") {
    stop(paste0("Please note that Starting Cohort 2's science tests can only ",
                "be linked longitudinally up to wave 5. Wave 9 test data does ",
                "not share any common test information and can, thus, not be ",
                "connected to previous assessment waves. It can only be ",
                "analysed cross-sectionally."), call. = FALSE)
  }
  if (min_valid < 0) {
    stop("min_valid must be greater than or equal to 0.", call. = FALSE)
  }
  if (min_valid >= length(item_labels[[domain]][[SC]][[wave]])) {
    stop("min_valid is too high. It excludes all possible test takers.",
         call. = FALSE)
  }
  if (!is.null(bgdata)) {
    if (inherits(bgdata, "list") ) {
      if (length(bgdata) != nmi) {
        stop(paste0(
          "The number of imputed data sets does not equal the specified ",
          "number of imputations 'nmi'."
        ), call. = FALSE)
      }
      if (!all(sapply(bgdata, is.data.frame))) {
        stop(paste0(
          "All data sets in bgdata must be formatted as data.frames"
        ), call. = FALSE)
      }
      if (!(sum(unlist(lapply(bgdata, names)) %in% "ID_t") == nmi)) {
        stop(paste0(
          "ID_t must be included in all imputed bgdata sets."
        ), call. = FALSE)
      }
      if (any(sapply(bgdata, ncol)) < 2) {
        stop(paste0(
          "The list of imputed bgdata sets must include at least one variable ",
          "apart from ID_t."
        ), call. = FALSE)
      }
      if (any(sapply(bgdata, is.na))) {
        stop(paste0(
          "Pre-imputed bgdata must not contain any NA."
        ), call. = FALSE)
      }
    } else {
      if (!is.data.frame(bgdata)) {
        stop("bgdata must be a data.frame.")
      }
      if (is.null(bgdata[["ID_t"]])) {
        stop("ID_t must be included in bgdata.")
      }
    }
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed)) {
      stop("'seed' must be a positive integer.")
    }
    set.seed(seed)
  }

  # complement control lists
  res <- complement_control_lists(
    control[["EAP"]], control[["WLE"]], control[["ML"]]
  )
  # control$Bayes <- res$Bayes
  control[["ML"]] <- res[["ML"]]
  control[["EAP"]] <- res[["EAP"]]
  control[["WLE"]] <- res[["WLE"]]

  # create auxiliary waves variable for longitudinal estimation
  waves <- create_waves_vars(longitudinal, SC, domain, wave)
  type <- ifelse(longitudinal, "long", "cross")

  # school context only for SC2-4 while in school
  if (adjust_school_context) {
    adjust_school_context <- was_assessed_in_school(longitudinal, SC, wave)
  }

  # Begin data pre-processing -------------------------------------------------

  t1 <- Sys.time()
  if (verbose) {
    message("Begin pre-processing of data... ", paste(t1))
    flush.console()
  }

  # read in competence data specified by user
  data <- read_in_competence_data(path, SC, domain)

  # number of not-reached items as processing time proxy
  res <- not_reached_as_proxy(include_nr, longitudinal, data, SC, domain, wave,waves
                              )
  items_not_reached <- res[["nr"]]
  data <- res[["data"]]
  include_nr <- res[["include_nr"]]

  # test data and test taker selection
  res <- select_test_responses_and_test_takers(
    longitudinal, SC, domain, data, wave, waves_ = gsub("_", "", waves), min_valid
  )
  data <- res[["data"]]
  resp <- res[["resp"]]

  # check for Partial Credit Items
  PCM <- is_PCM(longitudinal, resp)

  # process background data ---------------------------------------------------

  res <- pre_process_background_data(bgdata, data, include_nr,
                                     items_not_reached, min_valid)
  if (!is.null(bgdata)) {
    data <- res[["data"]]
  }
  bgdata <- res[["bgdata"]]
  ID_t <- res[["ID_t"]]

  # number of valid responses per person (and wave)
  valid_responses_per_person <-
    calculate_number_of_valid_responses(longitudinal, resp, waves)

  # multi-level proxy for school starting cohorts (while still in school)
  if (adjust_school_context) {
    if (is.null(bgdata)) {
      bgdata <- ID_t
    }
    bgdata <- add_contextual_info(path, SC, domain, waves, bgdata, data)
  }

  # test rotation and changes thereof
  if (longitudinal) {
    rotation <- FALSE
    position <- get_rotation_change_info_longitudinal(SC, domain, data)
  } else {
    res <- get_test_rotation_info_cross_sec(rotation, data, SC, wave, domain,
                                            resp, bgdata, ID_t)
    position <- res[["position"]]
    rotation <- res[["rotation"]]
    data <- res[["data"]]
    resp <- res[["resp"]]
    ID_t <- res[["ID_t"]]
    bgdata <- res[["bgdata"]]
  }

  # split items if DIF was detected during original scaling procedure
  if (SC == "SC4" && domain == "MA") {
    resp <-
      split_SC4_math_items(
        get_item_split_info(SC, domain, data), resp, longitudinal, wave)
  }else if (SC == "SC4" && domain == "SC") {
    resp <-
      split_SC4_science_items(
        get_item_split_info(SC, domain, data), resp, longitudinal, wave)
  }  else if (SC == "SC2" && domain == "RE") {
    resp <-
      split_SC2_reading_items(
        get_item_split_info(SC, domain, data), resp, longitudinal, wave)
  }else if (SC == "SC6" && domain == "SC") {
    resp <-
      split_SC6_science_items(
        get_item_split_info(SC, domain, data), resp, longitudinal, wave)
  }

  # multiple imputation of missing covariate data -----------------------------

  t2 <- Sys.time()
  res <- impute_missing_data(NULL, verbose, control, nmi)
  imp <- res[["imp"]]
  frmY <- res[["frmY"]]
  bgdata <- res[["bgdata"]]
  variable_importance <- res[["variable_importance"]]
  treeplot <- res[["treeplot"]]
  indmis <- res[["indmis"]]

  # begin estimation of plausible values --------------------------------------

  t3 <- Sys.time()
  if (verbose) {
    message("\nBegin estimation... ", paste(t3), "\nThis might take some time.")
    flush.console()
  }

  if (longitudinal) {
    res <- estimate_longitudinal(
      bgdata, imp, frmY = frmY, resp, PCM, ID_t, waves, "cross", domain, SC,
      control, npv, nmi, exclude
    )
  } else {
    if (rotation) {
      if (PCM) {
        res <- estimate_cross_pcm_corrected_for_rotation(
          bgdata, imp, frmY = frmY, waves, ID_t, resp, "cross", domain, SC,
          control, npv, nmi, position, exclude
        )
      } else {
        res <- estimate_cross_rasch_corrected_for_rotation(
          bgdata, imp, frmY = frmY, resp, position, waves, ID_t, "cross", domain,
          SC, control, npv, nmi, exclude
        )
      }
    } else {
      if (PCM) {
        res <- estimate_cross_pcm_uncorrected(
          bgdata, imp, resp, waves, frmY = frmY, ID_t, type, domain, SC,
          control, npv, nmi, exclude
        )
      } else {
        res <- estimate_cross_rasch_uncorrected(
          bgdata, imp, resp, waves, frmY = frmY, ID_t, "cross", domain, SC,
          control, npv, nmi, exclude
        )
      }
    }
  }
  eap <- res[["eap"]]
  pvs <- res[["pvs"]]
  EAP.rel <- res[["EAP.rel"]]
  regr.coeff <- res[["regr.coeff"]]
  mod <- res[["mod"]]
  info_crit <- res[["info_crit"]]
  variance <- res[["variance"]]

  # Begin post-processing of estimated data -----------------------------------

  t4 <- Sys.time()
  if (verbose) {
    message("Finished estimation. Begin post-processing... ", paste(t4))
    flush.console()
  }

  # # assumption: eaps are equivalent over all estimations
  # eap <- eap[[1]]

  # estimate WLEs
  if (control[["WLE"]]) {
    res <- estimate_wles(longitudinal, waves, mod)
    wle <- res[["wle"]]
    WLE.rel <- res[["WLE.rel"]]
  }

  # extract correct number of plausible values from pvs object
  res <- extract_correct_number_of_pvs(bgdata, nmi, npv, pvs, returnAll)
  datalist <- res[["datalist"]]
  npv <- res[["npv"]]

  # keep only those regr. coefficients / EAP reliabilities of kept imputations
  res <- discard_not_used_imputations(datalist, regr.coeff, EAP.rel,
                                      longitudinal, info_crit, treeplot,
                                      variable_importance, variance, eap)
  regr.coeff <- res[["regr.coeff"]]
  EAP.rel <- res[["EAP.rel"]]
  eap <- res[["eap"]]
  info_crit <- res[["info_crit"]]
  treeplot <- res[["treeplot"]]
  variable_importance <- res[["variable_importance"]]
  variance <- res[["variance"]]

  # add standardized regression coefficients
  if (!is.null(bgdata)) {
    regr.coeff <- calculate_standardized_regr_coeff(regr.coeff, datalist,
                                                    longitudinal, waves, variance)
  }

  # linking of longitudinal plausible values ----------------------------------

  # linear transformation of longitudinal PVs to pre-defined scale
  if (longitudinal) {
    res <- link_longitudinal_plausible_values(
      datalist, npv, min_valid, valid_responses_per_person, waves, eap,
      wle = if (control[["WLE"]]) {wle} else {NULL},
      data, SC, domain
    )
    pv <- res[["pv"]]
    wle <- res[["wle"]]
    eap <- res[["eap"]]
    if (SC == "SC4" & domain %in% c("MA", "RE") ||
        (SC == "SC3" & domain == "RE") ||
        (SC == "SC2" & domain %in% c("VO", "MA"))) {
      res <- correct_for_changed_test_rotation(SC, domain, position,
                                               wle = if (control[["WLE"]]) {wle} else {NULL},
                                               eap, pv)
      pv <- res[["pv"]]
      wle <- res[["wle"]]
      eap <- res[["eap"]]
    }
  } else {
    res <- set_not_enough_valid_resp_NA(
      datalist, eap, wle = if (control[["WLE"]]) {wle} else {NULL},
      valid_responses_per_person, min_valid, npv
    )
    pv <- res[["pv"]]
    wle <- res[["wle"]]
    eap <- res[["eap"]]
  }

  # calculate posterior moments of estimated eaps/plausible values
  MEAN <- calculate_posterior_means(eap,
                                    wle = if (control[["WLE"]]) {wle} else {NULL},
                                    pv, waves, npv)
  # VAR <- calculate_posterior_variances(eap,
  #                                   wle = if (control[["WLE"]]) {wle} else {NULL},
  #                                   pv, waves, npv)

  t5 <- Sys.time()

  if (verbose) {
    message("Done!")
    flush.console()
  }

  # collect output object -----------------------------------------------------

  res <- list()
  res[["SC"]] <- as.numeric(gsub(pattern = "SC", replacement = "", x = SC))
  res[["domain"]] <- domain
  res[["wave"]] <- as.numeric(gsub("_w", "", waves))
  res[["type"]] <- type
  res[["rotation"]] <- ifelse(rotation, "Corrected For Test Position",
                              "No Correction For Test Position"
  )
  res[["min_valid"]] <- min_valid
  res[["include_nr"]] <- include_nr
  res[["adjust_school_context"]] <- adjust_school_context
  res[["path"]] <- path
  res[["valid_responses_per_person"]] <- valid_responses_per_person
  res[["n_testtakers"]] <-
    colSums(!is.na(valid_responses_per_person[, -1, drop = FALSE]))
  res[["npv"]] <- npv
  res[["nmi"]] <- nmi
  if (!is.null(seed)) {
    res[["seed"]] <- seed
  }
  res[["control"]] <- control
  if (rotation) {
    res[["position"]] <- data.frame(ID_t, position)
  }
  res[["information_criteria"]] <- info_crit
  res[["posterior_means"]] <- MEAN
  res[["posterior_variances"]] <- variance #VAR
  res[["pv"]] <- pv
  if (control[["EAP"]]) {
    res[["eap"]] <- eap
  }
  if (control[["WLE"]]) {
    res[["wle"]] <- wle
    res[["WLE_rel"]] <- WLE.rel
  }
  res[["EAP_rel"]] <- EAP.rel
  res[["regr_coeff"]] <- regr.coeff
  res[["items"]] <- if (longitudinal) {
    xsi.fixed[["long"]][[domain]][[SC]][gsub("_", "", waves)]
  } else {
    mod[[1]][["xsi"]]
  }
  if (!is.null(treeplot)) {
    res[["treeplot"]] <- treeplot
  }
  if (!is.null(variable_importance)) {
    res[["variable_importance"]] <- variable_importance
  }
  if (!is.null(exclude)) {
    res[["exclude"]] <- exclude
  }
  res[["indmis"]] <- indmis
  res[["comp_time"]] <- list(
    initial_time = t0,
    input_check = t1 - t0,
    data_processing = t2 - t1,
    imputation = t3 - t2,
    estimation_time = t4 - t3,
    estimator_postprocessing = t5 - t4,
    total_comp_time = t5 - t0
  )
  class(res) <- "pv_obj"
  res
}




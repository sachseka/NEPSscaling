

correct_for_changed_test_rotation <- function(SC, domain, position, wle, eap, pv) {

  if (SC == "SC4") {
    # correct longitudinal values for change in rotation design
    # MA: add -0.060 to all participants who took math first (position == 1)
    # RE: add 0.164 to all participants who took reading second (position == 2)
    correction <- ifelse(domain == "MA", -0.060, 0.164)
    pos <- ifelse(domain == "MA", 1, 2)
    wave <- "w7"
  } else if (SC == "SC3") {
    # correct longitudinal values for change in rotation design
    # RE: add 0.174 to all participants who took reading second (position == 2)
    # NOTE: might not be necessary because this was done to calculate the link
    # constant!!! TODO
    correction <- 0.174
    pos <- 2
    wave <- "w6"
  } else if (SC == "SC2") {
    # correct longitudinal values for change in rotation design
    # VO: add 0.03 to all participants who took vocab test --> all children (wave 1)
    # VO: add 0.03 to all participants who took vocab test in (position == 2) (wave 3)
    if (domain == "VO") {
      correction <- matrix(0.03, nrow(wle), 2)
      correction[position$position == 1, 2] <- 0
      wave <- c("w1", "w3")
      pos <- c(1, 2)
    } else if (domain == "MA") {
      correction <- 0# TODO: add half the position effect of grade 1 to persons who received the test in second position in grade 1, but: not recorded in grade 1 TR...
      wave <- "w4"
      pos <- 2
    }
  }
  wle[position$ID_t %in% wle$ID_t && position$position %in% pos, 
      paste0("wle_", wave)] <-
    wle[position$ID_t %in% wle$ID_t && position$position %in% pos, 
        paste0("wle_", wave)] + correction
  eap[position$ID_t %in% wle$ID_t && position$position %in% pos, 
    paste0("eap_", wave)] <-
    eap[position$ID_t %in% wle$ID_t && position$position %in% pos, 
        paste0("eap_", 
        wave)] + correction
  for (i in seq(length(pv))) {
    pv[[i]][position$ID_t %in% wle$ID_t && position$position %in% pos, 
            paste0("PV_", wave)] <-
      pv[[i]][position$ID_t %in% wle$ID_t && position$position %in% pos, 
              paste0("PV_", wave)] + correction
  }
  
  list(wle = wle, eap = eap, pv = pv)

}

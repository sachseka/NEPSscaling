#' Collapse categories with too low cell frequencies (< 200) for stable PC
#'   estimation (guidelines laid out in Pohl & Carstensen (2012))
#'
#' @param resp Data.frame containing the item responses of the test takers
#' @param SC String representation of starting cohort
#' @param wave String representation of current wave of test assessment
#' @param domain String representation of current competency domain
#'
#' @noRd

collapse_categories_pcm <- function(resp, SC, wave, domain) {
  if (SC == "SC6") {
    if (domain == "RE") {
      # if (wave == "w3") { # SC6 RE w3 and w5 already collapsed in SUF
      #   resp$rea3012s_c[resp$rea3012s_c %in% c(0, 1)] <- 0
      #   resp$rea3012s_c[resp$rea3012s_c == 2] <- 1
      #   resp$rea3012s_c[resp$rea3012s_c == 3] <- 2
      #   resp$rea3015s_c[resp$rea3015s_c == 3] <- 2
      #   resp$rea3028s_c[resp$rea3028s_c %in% c(0, 1)] <- 0
      #   resp$rea3028s_c[resp$rea3028s_c == 2] <- 1
      #   resp$rea3028s_c[resp$rea3028s_c == 3] <- 2
      #   resp$rea3028s_c[resp$rea3028s_c == 4] <- 3
      #   resp$rea3028s_c[resp$rea3028s_c == 5] <- 4
      #   resp$rea3028s_c[resp$rea3028s_c == 6] <- 5
      #   resp$rea3038s_c[resp$rea3038s_c %in% c(0, 1)] <- 0
      #   resp$rea3038s_c[resp$rea3038s_c == 2] <- 1
      #   resp$rea3038s_c[resp$rea3038s_c == 3] <- 2
      #   resp$rea3052s_c[resp$rea3052s_c %in% c(0, 1, 2)] <- 0
      #   resp$rea3052s_c[resp$rea3052s_c == 3] <- 1
      #   resp$rea3054s_c[resp$rea3054s_c %in% c(0, 1)] <-  0
      #   resp$rea3054s_c[resp$rea3054s_c %in% c(2, 3)] <-  1
      #   resp$rea3054s_c[resp$rea3054s_c %in% c(4, 5)] <-  2
      # }
      if (wave == "w9") {
        # collapse categories
        resp$rea90101s_c[resp$rea90101s_c == 1] <- 0
        resp$rea90101s_c[resp$rea90101s_c == 2] <- 1
        resp$rea90101s_c[resp$rea90101s_c == 3] <- 2
        resp$rea90101s_c[resp$rea90101s_c == 4] <- 3
        resp$rea90206s_c[resp$rea90206s_c == 4] <- 3
        resp$rea90403s_c[resp$rea90403s_c == 1] <- 0
        resp$rea90403s_c[resp$rea90403s_c == 2] <- 1
        resp$rea90403s_c[resp$rea90403s_c == 3] <- 2
        resp$rea90403s_c[resp$rea90403s_c == 4] <- 3
        resp$rea90403s_c[resp$rea90403s_c == 5] <- 4
      }
    }
    if (domain == "MA") {
      if (wave == "w9") {
        # collapse categories
        resp$maa9v28s_c[resp$maa9v28s_c %in% c(0, 1, 2, 3)] <- 0
        resp$maa9v28s_c[resp$maa9v28s_c %in% c(4, 5, 6)] <- 1
        resp$maa9v27s_c[resp$maa9v27s_c == 1] <- 0
        resp$maa9v27s_c[resp$maa9v27s_c == 2] <- 1
        resp$maa9v27s_c[resp$maa9v27s_c == 3] <- 2
        resp$maa9r26s_c[resp$maa9r26s_c == 1] <- 0
        resp$maa9r26s_c[resp$maa9r26s_c == 2] <- 1
        resp$maa9r26s_c[resp$maa9r26s_c == 3] <- 2
        resp$maa9r26s_c[resp$maa9r26s_c == 4] <- 3
        # left out because of extreme amount of missing values (almost 100%!)
        # resp$maa9d23s_c[resp$maa9d23s_c == 1] <- 0
        # resp$maa9d23s_c[resp$maa9d23s_c == 2] <- 1
        # resp$maa9r18s_c[resp$maa9d18s_c == 2] <- 1
        resp$maa9d13s_c[resp$maa9d13s_c %in% c(1, 2, 3)] <- 0
        resp$maa9d13s_c[resp$maa9d13s_c == 4] <- 1
        resp$maa9d13s_c[resp$maa9d13s_c %in% c(5, 6)] <- 2
        resp$maa9d09s_c[resp$maa9d09s_c == 1] <- 0
        resp$maa9d09s_c[resp$maa9d09s_c == 2] <- 1
        resp$maa9d09s_c[resp$maa9d09s_c == 3] <- 2
        resp$maa9d09s_c[resp$maa9d09s_c == 4] <- 3
        resp$maa9d09s_c[resp$maa9d09s_c == 5] <- 4
        resp$maa9v07s_c[resp$maa9v07s_c == 2] <- 1
        resp$maa9r03s_c[resp$maa9r03s_c == 1] <- 0
        resp$maa9r03s_c[resp$maa9r03s_c == 2] <- 1
        resp$maa9r03s_c[resp$maa9r03s_c == 3] <- 2
      }
    }
    if (domain == "IC") {
      resp$ica5018s_c[resp$ica5018s_c %in% c(2, 3)] <- 1
      resp$ica5018s_c[resp$ica5018s_c %in% c(4, 5, 6, 7)] <- 2
      resp$ica5018s_c[resp$ica5018s_c == 8] <- 3
      resp$ica5015s_c[resp$ica5015s_c %in% c(0, 1)] <- 0
      resp$ica5015s_c[resp$ica5015s_c == 2] <- 1
      resp$ica5015s_c[resp$ica5015s_c == 3] <- 2
      resp$ica5020s_c[resp$ica5020s_c %in% c(0, 1)] <- 0
      resp$ica5020s_c[resp$ica5020s_c == 2] <- 1
      resp$ica5020s_c[resp$ica5020s_c == 3] <- 2
      resp$ica5020s_c[resp$ica5020s_c == 4] <- 3
      resp$ica5050s_c[resp$ica5050s_c %in% c(0, 1)] <- 0
      resp$ica5050s_c[resp$ica5050s_c == 2] <- 1
      resp$ica5050s_c[resp$ica5050s_c == 3] <- 2
      resp$ica5050s_c[resp$ica5050s_c == 4] <- 3
      resp$ica5004s_c[resp$ica5004s_c %in% c(0, 1, 2)] <- 0
      resp$ica5004s_c[resp$ica5004s_c == 3] <- 1
      resp$ica5004s_c[resp$ica5004s_c == 4] <- 2
      resp$ica5004s_c[resp$ica5004s_c == 5] <- 3
      resp$ica5017s_c[resp$ica5017s_c %in% c(0, 1, 2)] <- 0
      resp$ica5017s_c[resp$ica5017s_c == 3] <- 1
      resp$ica5017s_c[resp$ica5017s_c == 4] <- 2
      resp$ica5017s_c[resp$ica5017s_c == 5] <- 3
      resp$ica5016s_c[resp$ica5016s_c %in% c(0, 1, 2)] <- 0
      resp$ica5016s_c[resp$ica5016s_c == 3] <- 1
      resp$ica5016s_c[resp$ica5016s_c == 4] <- 2
      resp$ica5016s_c[resp$ica5016s_c == 5] <- 3
      resp$ica5047s_c[resp$ica5047s_c %in% c(0, 1, 2)] <- 0
      resp$ica5047s_c[resp$ica5047s_c == 3] <- 1
      resp$ica5047s_c[resp$ica5047s_c == 4] <- 2
      resp$ica5047s_c[resp$ica5047s_c == 5] <- 3
      resp$ica5021s_c[resp$ica5021s_c %in% c(0, 1, 2)] <- 0
      resp$ica5021s_c[resp$ica5021s_c == 3] <- 1
      resp$ica5021s_c[resp$ica5021s_c == 4] <- 2
      resp$ica5021s_c[resp$ica5021s_c == 5] <- 3
      resp$ica5052s_c[resp$ica5052s_c %in% c(0, 1, 2)] <- 0
      resp$ica5052s_c[resp$ica5052s_c == 3] <- 1
      resp$ica5052s_c[resp$ica5052s_c == 4] <- 2
      resp$ica5052s_c[resp$ica5052s_c == 5] <- 3
    } else if (domain == "SC") {
      resp$sca5652s_c[resp$sca5652s_c %in% c(0, 1)] <- 0
      resp$sca5652s_c[resp$sca5652s_c == 2] <- 1
      resp$sca5652s_c[resp$sca5652s_c == 3] <- 2
      resp$sca5652s_c[resp$sca5652s_c == 4] <- 3
      resp$sca5091s_c[resp$sca5091s_c %in% c(0, 1)] <- 0
      resp$sca5091s_c[resp$sca5091s_c == 2] <- 1
      resp$sca5091s_c[resp$sca5091s_c == 3] <- 2
      resp$sca5091s_c[resp$sca5091s_c == 4] <- 3
    }
  } else if (SC == "SC5") {
    # EF (w12) already collapsed in SUF
    if (domain == "RE") {
      # reading w1 already collapsed in SUF
      if (wave == "w12") {
        # collapse categories
        resp$rea90201s_sc5s12_c[resp$rea90201s_sc5s12_c == 1] <- 0
        resp$rea90201s_sc5s12_c[resp$rea90201s_sc5s12_c == 2] <- 1
        resp$rea90201s_sc5s12_c[resp$rea90201s_sc5s12_c == 3] <- 2
        resp$rea90201s_sc5s12_c[resp$rea90201s_sc5s12_c == 4] <- 3
        resp$rea90206s_sc5s12_c[resp$rea90206s_sc5s12_c == 4] <- 3
        resp$rea90402s_sc5s12_c[resp$rea90402s_sc5s12_c == 1] <- 0
        resp$rea90402s_sc5s12_c[resp$rea90402s_sc5s12_c == 2] <- 1
        resp$rea90402s_sc5s12_c[resp$rea90402s_sc5s12_c == 3] <- 2
        resp$rea90402s_sc5s12_c[resp$rea90402s_sc5s12_c == 4] <- 3
        resp$rea90402s_sc5s12_c[resp$rea90402s_sc5s12_c == 5] <- 4
      }
    }
    if (domain == "MA") {
      if (wave == "w1") {
        resp$mas1q02s_c[resp$mas1q02s_c %in% c(1, 2)] <- 0
        resp$mas1q02s_c[resp$mas1q02s_c == 3] <- 1
        resp$mas1q02s_c[resp$mas1q02s_c == 4] <- 2
      }
      if (wave == "w12") {
        # collapse categories
        resp$maa9v28s_sc5s12_c[resp$maa9v28s_sc5s12_c %in% c(0, 1, 2, 3)] <- 0
        resp$maa9v28s_sc5s12_c[resp$maa9v28s_sc5s12_c %in% c(4, 5, 6)] <- 1
        resp$maa9v27s_sc5s12_c[resp$maa9v27s_sc5s12_c == 1] <- 0
        resp$maa9v27s_sc5s12_c[resp$maa9v27s_sc5s12_c == 2] <- 1
        resp$maa9v27s_sc5s12_c[resp$maa9v27s_sc5s12_c == 3] <- 2
        resp$maa9r26s_sc5s12_c[resp$maa9r26s_sc5s12_c == 1] <- 0
        resp$maa9r26s_sc5s12_c[resp$maa9r26s_sc5s12_c == 2] <- 1
        resp$maa9r26s_sc5s12_c[resp$maa9r26s_sc5s12_c == 3] <- 2
        resp$maa9r26s_sc5s12_c[resp$maa9r26s_sc5s12_c == 4] <- 3
        # left out because of extreme amount of missing values (almost 100%!)
        # resp$maa9d23s_sc5s12_c[resp$maa9d23s_sc5s12_c == 1] <- 0
        # resp$maa9d23s_sc5s12_c[resp$maa9d23s_sc5s12_c == 2] <- 1
        # resp$maa9r18s_sc5s12_c[resp$maa9r18s_sc5s12_c == 2] <- 1
        resp$maa9d13s_sc5s12_c[resp$maa9d13s_sc5s12_c %in% c(1, 2, 3)] <- 0
        resp$maa9d13s_sc5s12_c[resp$maa9d13s_sc5s12_c == 4] <- 1
        resp$maa9d13s_sc5s12_c[resp$maa9d13s_sc5s12_c %in% c(5, 6)] <- 2
        resp$maa9d09s_sc5s12_c[resp$maa9d09s_sc5s12_c == 1] <- 0
        resp$maa9d09s_sc5s12_c[resp$maa9d09s_sc5s12_c == 2] <- 1
        resp$maa9d09s_sc5s12_c[resp$maa9d09s_sc5s12_c == 3] <- 2
        resp$maa9d09s_sc5s12_c[resp$maa9d09s_sc5s12_c == 4] <- 3
        resp$maa9d09s_sc5s12_c[resp$maa9d09s_sc5s12_c == 5] <- 4
        resp$maa9v07s_sc5s12_c[resp$maa9v07s_sc5s12_c == 2] <- 1
        resp$maa9r03s_sc5s12_c[resp$maa9r03s_sc5s12_c == 1] <- 0
        resp$maa9r03s_sc5s12_c[resp$maa9r03s_sc5s12_c == 2] <- 1
        resp$maa9r03s_sc5s12_c[resp$maa9r03s_sc5s12_c == 3] <- 2
      }
    } else if (domain == "IC") {
      resp$ics3002s_c[resp$ics3002s_c == 1] <- 0
      resp$ics3002s_c[resp$ics3002s_c == 2] <- 1
      resp$ics3029s_c[resp$ics3029s_c == 1] <- 0
      resp$ics3029s_c[resp$ics3029s_c == 2] <- 1
      resp$ics3029s_c[resp$ics3029s_c == 3] <- 2
      resp$ics3035s_c[resp$ics3035s_c == 1] <- 0
      resp$ics3035s_c[resp$ics3035s_c == 2] <- 1
      resp$ics3035s_c[resp$ics3035s_c == 3] <- 2
      resp$ics3035s_c[resp$ics3035s_c == 4] <- 3
      resp$ics3049s_c[resp$ics3049s_c == 1] <- 0
      resp$ics3049s_c[resp$ics3049s_c == 2] <- 1
      resp$ics3049s_c[resp$ics3049s_c == 3] <- 2
    } else if (domain == "SC") {
      resp$scs3623s_c[resp$scs3623s_c == 1] <- 0
      resp$scs3623s_c[resp$scs3623s_c == 2] <- 1
      resp$scs3623s_c[resp$scs3623s_c == 3] <- 2
      resp$scs3021s_c[resp$scs3021s_c == 1] <- 0
      resp$scs3021s_c[resp$scs3021s_c == 2] <- 1
      resp$scs3021s_c[resp$scs3021s_c == 3] <- 2
      resp$scs3022s_c[resp$scs3022s_c == 1] <- 0
      resp$scs3022s_c[resp$scs3022s_c == 2] <- 1
      resp$scs3022s_c[resp$scs3022s_c == 3] <- 2
      resp$scs3022s_c[resp$scs3022s_c == 4] <- 3
      resp$scs3643s_c[resp$scs3643s_c == 1] <- 0
      resp$scs3643s_c[resp$scs3643s_c == 2] <- 1
      resp$scs3643s_c[resp$scs3643s_c == 3] <- 2
      resp$scs3643s_c[resp$scs3643s_c == 4] <- 3
      resp$scs3642s_c[resp$scs3642s_c == 1] <- 0
      resp$scs3642s_c[resp$scs3642s_c == 2] <- 1
      resp$scs3642s_c[resp$scs3642s_c == 3] <- 2
      resp$scs3031s_c[resp$scs3031s_c == 1] <- 0
      resp$scs3031s_c[resp$scs3031s_c == 2] <- 1
      resp$scs3031s_c[resp$scs3031s_c == 3] <- 2
      resp$scs3112s_c[resp$scs3112s_c == 1] <- 0
      resp$scs3112s_c[resp$scs3112s_c == 2] <- 1
      resp$scs3112s_c[resp$scs3112s_c == 3] <- 2
      resp$scs3132s_c[resp$scs3132s_c == 1] <- 0
      resp$scs3132s_c[resp$scs3132s_c == 2] <- 1
      resp$scs3132s_c[resp$scs3132s_c == 3] <- 2
      resp$scs3132s_c[resp$scs3132s_c == 4] <- 3
      resp$scs3133s_c[resp$scs3133s_c %in% c(1, 2)] <- 0
      resp$scs3133s_c[resp$scs3133s_c == 3] <- 1
      resp$scs3133s_c[resp$scs3133s_c == 4] <- 2
      resp$scs3012s_c[resp$scs3012s_c %in% c(1, 2)] <- 0
      resp$scs3012s_c[resp$scs3012s_c == 3] <- 1
      resp$scs3012s_c[resp$scs3012s_c == 4] <- 2
      resp$scs3061s_c[resp$scs3061s_c %in% c(1, 2, 3)] <- 0
      resp$scs3061s_c[resp$scs3061s_c == 4] <- 1
    }
  } else if (SC == "SC4") {
    if (domain == "RE") {
      # reading w1 already collapsed in SUF
      if (wave == "w7") {
        resp$reg12014s_c[resp$reg12014s_c == 2] <- 1
        resp$reg12021s_c[resp$reg12021s_c == 1] <- 0
        resp$reg12021s_c[resp$reg12021s_c == 2] <- 1
        resp$reg12024s_c[resp$reg12024s_c == 1] <- 0
        resp$reg12024s_c[resp$reg12024s_c == 2] <- 1
        resp$reg12024s_c[resp$reg12024s_c == 3] <- 2
        resp$reg12024s_c[resp$reg12024s_c == 4] <- 3
        resp$reg12026s_c[resp$reg12026s_c == 1] <- 0
        resp$reg12026s_c[resp$reg12026s_c == 2] <- 1
        resp$reg12026s_c[resp$reg12026s_c == 3] <- 2
        resp$reg12026s_c[resp$reg12026s_c == 4] <- 3
        resp$reg12026s_c[resp$reg12026s_c == 5] <- 4
        resp$reg12026s_c[resp$reg12026s_c == 6] <- 5
        resp$reg12042s_c[resp$reg12042s_c == 1] <- 0
        resp$reg12042s_c[resp$reg12042s_c == 2] <- 1
        resp$reg12055s_c[resp$reg12055s_c == 1] <- 0
        resp$reg12055s_c[resp$reg12055s_c == 2] <- 1
        resp$reg12055s_c[resp$reg12055s_c == 3] <- 2
        resp$reg12071s_c[resp$reg12071s_c == 1] <- 0
        resp$reg12071s_c[resp$reg12071s_c == 2] <- 0
        resp$reg12071s_c[resp$reg12071s_c == 3] <- 1
        resp$reg12075s_c[resp$reg12075s_c == 1] <- 0
        resp$reg12075s_c[resp$reg12075s_c == 2] <- 1
        resp$reg12075s_c[resp$reg12075s_c == 3] <- 2
      }
      if (wave == "w10") {
        # TODO: change to correct item names
        resp$rea90101s_sc4a10_c[resp$rea90101s_sc4a10_c == 1] <- 0
        resp$rea90101s_sc4a10_c[resp$rea90101s_sc4a10_c == 2] <- 1
        resp$rea90101s_sc4a10_c[resp$rea90101s_sc4a10_c == 3] <- 2
        resp$rea90101s_sc4a10_c[resp$rea90101s_sc4a10_c == 4] <- 3
        resp$rea90206s_sc4a10_c[resp$rea90206s_sc4a10_c == 4] <- 3
        resp$rea90403s_sc4a10_c[resp$rea90403s_sc4a10_c == 1] <- 0
        resp$rea90403s_sc4a10_c[resp$rea90403s_sc4a10_c == 2] <- 1
        resp$rea90403s_sc4a10_c[resp$rea90403s_sc4a10_c == 3] <- 2
        resp$rea90403s_sc4a10_c[resp$rea90403s_sc4a10_c == 4] <- 3
        resp$rea90403s_sc4a10_c[resp$rea90403s_sc4a10_c == 5] <- 4
      }
    } else if (domain == "MA") {
      # math w1 already collapsed in SUF
      if (wave == "w7") {
        resp$mas1q02s_sc4g12_c[resp$mas1q02s_sc4g12_c == 1] <- 0
        resp$mas1q02s_sc4g12_c[resp$mas1q02s_sc4g12_c == 2] <- 1
        resp$mas1q02s_sc4g12_c[resp$mas1q02s_sc4g12_c == 3] <- 2
        resp$mas1q02s_sc4g12_c[resp$mas1q02s_sc4g12_c == 4] <- 3
      }
      if (wave == "w10") {
        # collapse categories
        # TODO: check correct item names
        resp$maa9v28s_sc4a10_c[resp$maa9v28s_sc4a10_c %in% c(0, 1, 2, 3)] <- 0
        resp$maa9v28s_sc4a10_c[resp$maa9v28s_sc4a10_c %in% c(4, 5, 6)] <- 1
        resp$maa9v27s_sc4a10_c[resp$maa9v27s_sc4a10_c == 1] <- 0
        resp$maa9v27s_sc4a10_c[resp$maa9v27s_sc4a10_c == 2] <- 1
        resp$maa9v27s_sc4a10_c[resp$maa9v27s_sc4a10_c == 3] <- 2
        resp$maa9r26s_sc4a10_c[resp$maa9r26s_sc4a10_c == 1] <- 0
        resp$maa9r26s_sc4a10_c[resp$maa9r26s_sc4a10_c == 2] <- 1
        resp$maa9r26s_sc4a10_c[resp$maa9r26s_sc4a10_c == 3] <- 2
        resp$maa9r26s_sc4a10_c[resp$maa9r26s_sc4a10_c == 4] <- 3
        # left out because of extreme amount of missing values (almost 100%!)
        # resp$maa9d23s_sc4a10_c[resp$maa9d23s_sc4a10_c == 1] <- 0
        # resp$maa9d23s_sc4a10_c[resp$maa9d23s_sc4a10_c == 2] <- 1
        # resp$maa9r18s_sc4a10_c[resp$maa9r18s_sc4a10_c == 2] <- 1
        resp$maa9d13s_sc4a10_c[resp$maa9d13s_sc4a10_c %in% c(1, 2, 3)] <- 0
        resp$maa9d13s_sc4a10_c[resp$maa9d13s_sc4a10_c == 4] <- 1
        resp$maa9d13s_sc4a10_c[resp$maa9d13s_sc4a10_c %in% c(5, 6)] <- 2
        resp$maa9d09s_sc4a10_c[resp$maa9d09s_sc4a10_c == 1] <- 0
        resp$maa9d09s_sc4a10_c[resp$maa9d09s_sc4a10_c == 2] <- 1
        resp$maa9d09s_sc4a10_c[resp$maa9d09s_sc4a10_c == 3] <- 2
        resp$maa9d09s_sc4a10_c[resp$maa9d09s_sc4a10_c == 4] <- 3
        resp$maa9d09s_sc4a10_c[resp$maa9d09s_sc4a10_c == 5] <- 4
        resp$maa9v07s_sc4a10_c[resp$maa9v07s_sc4a10_c == 2] <- 1
        resp$maa9r03s_sc4a10_c[resp$maa9r03s_sc4a10_c == 1] <- 0
        resp$maa9r03s_sc4a10_c[resp$maa9r03s_sc4a10_c == 2] <- 1
        resp$maa9r03s_sc4a10_c[resp$maa9r03s_sc4a10_c == 3] <- 2
      }
    } else if (domain == "IC") {
      if (wave == "w1") {
        resp$icg9102s_c[resp$icg9102s_c == 1] <- 0
        resp$icg9102s_c[resp$icg9102s_c == 2] <- 1
        resp$icg9102s_c[resp$icg9102s_c == 3] <- 2
        resp$icg9102s_c[resp$icg9102s_c == 4] <- 3
        resp$icg9107s_c[resp$icg9107s_c == 1] <- 0
        resp$icg9107s_c[resp$icg9107s_c == 2] <- 1
        resp$icg9107s_c[resp$icg9107s_c == 3] <- 2
        resp$icg9107s_c[resp$icg9107s_c == 4] <- 3
        resp$icg9107s_c[resp$icg9107s_c == 5] <- 4
        resp$icg9125s_c[resp$icg9125s_c == 1] <- 0
        resp$icg9125s_c[resp$icg9125s_c == 2] <- 1
        resp$icg9125s_c[resp$icg9125s_c == 3] <- 2
        resp$icg9125s_c[resp$icg9125s_c == 4] <- 3
        resp$icg9117s_c[resp$icg9117s_c == 1] <- 0
        resp$icg9117s_c[resp$icg9117s_c == 2] <- 1
        resp$icg9117s_c[resp$icg9117s_c == 3] <- 2
        resp$icg9117s_c[resp$icg9117s_c == 4] <- 3
        resp$icg9117s_c[resp$icg9117s_c == 5] <- 4
        resp$icg9117s_c[resp$icg9117s_c == 6] <- 5
        resp$icg9133s_c[resp$icg9133s_c == 1] <- 0
        resp$icg9133s_c[resp$icg9133s_c == 2] <- 1
        resp$icg9133s_c[resp$icg9133s_c == 3] <- 2
        resp$icg9133s_c[resp$icg9133s_c == 4] <- 3
        resp$icg9133s_c[resp$icg9133s_c == 5] <- 4
        resp$icg9133s_c[resp$icg9133s_c == 6] <- 5
        resp$icg9136s_c[resp$icg9136s_c == 1] <- 0
        resp$icg9136s_c[resp$icg9136s_c == 2] <- 1
        resp$icg9136s_c[resp$icg9136s_c == 3] <- 2
        resp$icg9136s_c[resp$icg9136s_c == 4] <- 3
        resp$icg9136s_c[resp$icg9136s_c == 5] <- 4
        resp$icg9136s_c[resp$icg9136s_c == 6] <- 5
        resp$icg9136s_c[resp$icg9136s_c == 7] <- 6
        resp$icg9140s_c[resp$icg9140s_c == 1] <- 0
        resp$icg9140s_c[resp$icg9140s_c == 2] <- 1
        resp$icg9140s_c[resp$icg9140s_c == 3] <- 2
        resp$icg9140s_c[resp$icg9140s_c == 4] <- 3
      } else if (wave == "w7") {
        resp$icg12013s_c[resp$icg12013s_c %in% c(1, 2)] <- 0
        resp$icg12013s_c[resp$icg12013s_c == 3] <- 1
        resp$icg12018s_c[resp$icg12018s_c %in% c(1, 2, 3)] <- 0
        resp$icg12018s_c[resp$icg12018s_c == 4] <- 1
        resp$icg12060s_c[resp$icg12060s_c %in% c(1, 2)] <- 0
        resp$icg12060s_c[resp$icg12060s_c == 3] <- 1
        resp$icg12060s_c[resp$icg12060s_c == 4] <- 2
        resp$icg12016s_c[resp$icg12016s_c %in% c(1, 2)] <- 0
        resp$icg12016s_c[resp$icg12016s_c == 3] <- 1
        resp$icg12016s_c[resp$icg12016s_c == 4] <- 2
        resp$icg12054s_c[resp$icg12054s_c %in% c(1, 2)] <- 0
        resp$icg12054s_c[resp$icg12054s_c == 3] <- 1
        resp$icg12054s_c[resp$icg12054s_c == 4] <- 2
        resp$icg12138s_c[resp$icg12138s_c == 1] <- 0
        resp$icg12138s_c[resp$icg12138s_c == 2] <- 1
        resp$icg12138s_c[resp$icg12138s_c == 3] <- 2
        resp$icg12138s_c[resp$icg12138s_c == 4] <- 3
        resp$icg12028s_c[resp$icg12028s_c %in% c(1, 2, 3)] <- 0
        resp$icg12028s_c[resp$icg12028s_c == 4] <- 1
        resp$icg12028s_c[resp$icg12028s_c == 5] <- 2
        resp$ica5021s_c[resp$ica5021s_c %in% c(1, 2, 3)] <- 0
        resp$ica5021s_c[resp$ica5021s_c == 4] <- 1
        resp$ica5021s_c[resp$ica5021s_c == 5] <- 2
        resp$icg12107s_c[resp$icg12107s_c %in% c(1, 2)] <- 0
        resp$icg12107s_c[resp$icg12107s_c == 3] <- 1
        resp$icg12107s_c[resp$icg12107s_c == 4] <- 2
        resp$icg12107s_c[resp$icg12107s_c == 5] <- 3
        resp$ica5052s_c[resp$ica5052s_c %in% c(1, 2)] <- 0
        resp$ica5052s_c[resp$ica5052s_c == 3] <- 1
        resp$ica5052s_c[resp$ica5052s_c == 4] <- 2
        resp$ica5052s_c[resp$ica5052s_c == 5] <- 3
        resp$icg12048s_c[resp$icg12048s_c == 1] <- 0
        resp$icg12048s_c[resp$icg12048s_c == 2] <- 1
        resp$icg12048s_c[resp$icg12048s_c == 3] <- 2
        resp$icg12048s_c[resp$icg12048s_c == 4] <- 3
        resp$icg12048s_c[resp$icg12048s_c == 5] <- 4
        resp$icg12119s_c[resp$icg12119s_c == 1] <- 0
        resp$icg12119s_c[resp$icg12119s_c == 2] <- 1
        resp$icg12119s_c[resp$icg12119s_c == 3] <- 2
        resp$icg12119s_c[resp$icg12119s_c == 4] <- 3
        resp$icg12119s_c[resp$icg12119s_c == 5] <- 4
        resp$icg12050s_c[resp$icg12050s_c %in% c(1, 2, 3)] <- 0
        resp$icg12050s_c[resp$icg12050s_c == 4] <- 1
        resp$icg12050s_c[resp$icg12050s_c == 5] <- 2
        resp$icg12050s_c[resp$icg12050s_c == 6] <- 3
        resp$icg12004s_c[resp$icg12004s_c %in% c(1, 2)] <- 0
        resp$icg12004s_c[resp$icg12004s_c == 3] <- 1
        resp$icg12004s_c[resp$icg12004s_c == 4] <- 2
        resp$icg12004s_c[resp$icg12004s_c == 5] <- 3
        resp$icg12004s_c[resp$icg12004s_c == 6] <- 4
        resp$icg12046s_c[resp$icg12046s_c == 1] <- 0
        resp$icg12046s_c[resp$icg12046s_c == 2] <- 1
        resp$icg12046s_c[resp$icg12046s_c == 2] <- 2
        resp$icg12046s_c[resp$icg12046s_c == 4] <- 3
        resp$icg12046s_c[resp$icg12046s_c == 5] <- 4
        resp$icg12046s_c[resp$icg12046s_c == 6] <- 5
      }
    } else if (domain == "SC") {
      if (wave == "w1") {
        resp$scg9012s_c[resp$scg9012s_c == 1] <- 0
        resp$scg9012s_c[resp$scg9012s_c == 2] <- 1
        resp$scg9012s_c[resp$scg9012s_c == 3] <- 2
        resp$scg9012s_c[resp$scg9012s_c == 4] <- 3
        resp$scg9052s_c[resp$scg9052s_c == 1] <- 0
        resp$scg9052s_c[resp$scg9052s_c == 2] <- 1
        resp$scg9052s_c[resp$scg9052s_c == 3] <- 2
        resp$scg9052s_c[resp$scg9052s_c == 4] <- 3
        resp$scg9042s_c[resp$scg9042s_c == 1] <- 0
        resp$scg9042s_c[resp$scg9042s_c == 2] <- 1
        resp$scg9042s_c[resp$scg9042s_c == 3] <- 2
        resp$scg9042s_c[resp$scg9042s_c == 4] <- 3
      } else if (wave == "w5") {
        # not available yet
      }
    }
  }

  # ind <- which(apply(resp, 2, max, na.rm = TRUE) > 1) # to catch only 'after-collapse' pc items (works for MD!)

  res <- list(resp = resp) # , ind = ind)
  return(res)
}

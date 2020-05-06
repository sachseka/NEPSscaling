#' scoring function for PCM items (and occasionally dichotomous items)
#'
#' @param SC String representation of starting cohort
#' @param wave String representation of current wave of test assessment
#' @param domain String representation of current competency domain
#'
#' @noRd

get_indicators_for_half_scoring <- function(SC, domain, wave) {
  if (SC == "SC5") {
    if (domain == "MA") {
      if (wave == "w1") {
        ind <- c(13)
      } else if (wave == "w12") {
        ind <- c(5, 14, 23, 25, 29, 39, 45, 46, 49)
      }
    } else if (domain == "RE") {
      if (wave == "w1") {
        ind <- c(2, 9, 10, 12, 17, 26)
      } else if (wave == "w12") {
        ind <- c(4, 5, 8, 9, 14, 17, 21)
      }
    } else if (domain == "EF") {
      if (wave == "w12") {
        ind <- c(19)
      }
    } else if (domain == "IC") {
      if (wave == "w5") {
        ind <- c(2, 15, 17, 18, 26)
      }
    } else if (domain == "SC") {
      if (wave == "w5") {
        ind <- c(4, 13, 14, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26)
      }
    }
  } else if (SC == "SC6") {
    if (domain == "MA") {
      if (wave == "w9") {
        ind <- c(5, 14, 23, 25, 29, 39, 45, 46, 49)
      }
    } else if (domain == "RE") {
      if (wave %in% c("w3", "w5")) {
        ind <- c(2, 5, 11, 19, 21, 29, 27)
      } else if (wave == "w9") {
        ind <- c(1, 2, 3, 4, 5, 9, 10, 14, 15, 20, 22, 24, 25, 36)
      }
    } else if (domain == "SC") {
      if (wave == "w5") {
        ind <- c(6, 16)
      }
    } else if (domain == "IC") {
      if (wave == "w5") {
        ind <- c(4, 9, 10, 11, 13, 14, 22, 24, 26, 27)
      }
    }
  } else if (SC == "SC4") {
    if (domain == "MA") {
      if (wave == "w1") {
        ind <- c(3, 16)
      } else if (wave == "w7") {
        # items 10 and 15 appear twice for different sample groups!
        ind <- c(21) # 19 ohne item splits!
      } else if (wave == "w10") {
        ind <- c(5, 14, 23, 25, 29, 39, 45, 46, 49)
      }
    } else if (domain == "SC") {
      if (wave == "w1") {
        ind <- c(2, 4, 7, 11, 14, 18, 19, 20, 24)
      } else if (wave == "w5") {
        ind <- c(5, 7, 10, 11, 15, 24)
      }
    } else if (domain == "RE") {
      if (wave == "w2") {
        ind <- c(4, 5, 13, 24)
      } else if (wave == "w7") {
        ind <- c(4, 8, 11, 13, 20, 22, 25, 28, 34, 37, 41)
      } else if (wave == "w10") {
        ind <- c(1, 2, 3, 4, 5, 9, 10, 14, 15, 20, 22, 24, 25, 36)
      }
    } else if (domain == "EF") {
      if (wave == "w3") {
        ind <- c(1, 2, 3, 4, 5, 6, 7, 11)
      } else if (wave == "w7") {
        ind <- c(1, 2, 3, 4)
      }
    } else if (domain == "IC") {
      if (wave == "w1") {
        ind <- c(2, 7, 15, 22, 30, 33, 36)
      } else if (wave == "w7") {
        ind <- c(1, 3, 4, 8, 9, 10, 13, 20, 21, 22, 24, 25, 26,
                 27, 28, 29, 30, 31)
      }
    } else if (domain == "ST") {
      ind <- 1:5
    }
  } else if (SC == "SC3") {
    if (domain == "MA") {
      if (wave == "w1") {
        ind <- c(20)
      } else if (wave == "w3") {
        ind <- c(23)
      } else if (wave == "w5") {
        ind <- c(3, 6, 26, 27, 33)
      } else if (wave == "w9") {
        ind <- c(14)
      }
    } else if (domain == "RE") {
      if (wave == "w1") {
        ind <- c(2, 6, 13, 22, 27, 30)
      } else if (wave == "w3") {
        ind <- c(3, 6, 9, 12, 15, 16, 18, 21, 29, 31, 33, 34, 36, 40)
      } else if (wave == "w6") {
        ind <- c(3, 4, 11, 13, 22, 24, 30, 37)
      } else if (wave == "w9") {
        ind <- c(4, 12, 15, 18, 26, 27, 29, 32, 35, 37, 41)
      }
    } else if (domain == "SC") {
      if (wave == "w2") {
        ind <- c(15, 20)
      } else if (wave == "w5") {
        ind <- c(2, 4, 7, 11, 14, 18, 19, 20, 24, 30, 33, 34)
      } else if (wave == "w8") {
        ind <- c(5, 7, 10, 15, 24, 25)
      }
    } else if (domain == "IC") {
      # if (wave == "w2") {
      #   ind <- c() # no polytomous items
      # } else
      if (wave == "w5") {
        ind <- c(26, 29, 36, 37, 43, 44, 46, 47, 51, 52, 53, 55, 56, 57, 58, 60)
      } else if (wave == "w9") {
        ind <- c(3, 4, 10, 11, 14, 21, 22, 23, 25, 27:32)
      }
    } else if (domain == "EF") {
      if (wave == "w7") {
        ind <- c(1, 2, 3, 4, 5, 6, 7, 11)
      } else if (wave == "w9") {
        ind <- 1:4
      }
    } else if (domain == "ST") {
      ind <- 1:5
    } else if (domain == "LI") {
      ind <- c(1:12, 14:16)
    }
  } else if (SC == "SC2") {
    # if (domain == "VO") {} # no polytomous items
    #
    if (domain == "RE") {
      if (wave == "w6") {
        ind <- c(2, 6, 13, 22, 27, 30)
      }
    } else if (domain == "SC") {
      if (wave == "w1") {
        ind <- c(5, 7, 9, 17)
      } else if (wave == "w3") {
        ind <- c(6, 8, 10)
      } else if (wave == "w5") {
        ind <- c(3, 7, 16, 17, 20)
      }
    } else if (domain == "MA") {
      if (wave == "w2") {
        ind <- c(25)
      } else if (wave == "w3") {
        ind <- c(7, 8, 9)
      } else if (wave == "w4") {
        ind <- c(10, 12)
      } else if (wave == "w6") {
        ind <- c(21)
      }
    }
  } else if (SC == "SC1") {
    # if (domain == "CD") {} # no polytomous items
    # if (domain == "VO") {} # no polytomous items
    if (domain == "SC") {
      if (wave == "w6") {
        ind <- c(2, 5, 13)
      } else if (wave == "w8") {
        ind <- c()
      }
    } else if (domain == "MA") {
      if (wave == "w5") {
        ind <- c(1, 5)
      } else if (wave == "w7") {
        ind <- c()
      } else if (wave == "w9") {
        ind <- c()
      }
    }
  }
  ind
}

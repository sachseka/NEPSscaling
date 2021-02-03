context("calculate_school_average")

test_that("calculate_school_average", {
  school_data <- data.frame(school_w1 = rep(1:2, 5), school_w2 = rep(1:2, 5),
                            wle1 = 1:10, wle2 = 21:30)
  waves <- c("_w1", "_w2")
  wle_vnames <- c("wle1", "wle2")
  
  # mean only over the subsets, wle1/2 are renamed
  result <- data.frame(school_w1 = rep(1:2, 5), school_w2 = rep(1:2, 5),
                       wle1_schavg = rep(c((1+3+5+7+9) / 5, (2+4+6+8+10) / 5), 5), 
                       wle2_schavg = rep(c((21+23+25+27+29) / 5, (22+24+26+28+30) / 5), 5))
  expect_equivalent(calculate_school_average(school_data, waves, wle_vnames),
                    result)
})

# function(school_data, waves, wle_vnames) {
#   school_waves <-
#     names(school_data)[names(school_data) %in% paste0("school", waves)]
#   for (i in seq(length(waves))) {
#     w <- school_waves[i]
#     vn <- wle_vnames[i]
#     # NAs: correspond to missing WLEs and are ignored
#     for (j in unique(na.omit(school_data[[w]]))) {
#       school_data[which(school_data[[w]] == j), vn] <-
#         mean(school_data[[vn]][which(school_data[[w]] == j)], na.rm = TRUE)
#     }
#     names(school_data)[which(names(school_data) == vn)] <- paste0(vn, "_schavg")
#   }
#   school_data
# }
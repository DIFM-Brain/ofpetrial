# min_rate <- 0
# max_rate <- 100
# gc_rate <- 0
# num_levels <- 5

get_rates <- function(min_rate,
                      max_rate,
                      gc_rate,
                      num_levels) {
  dif_min <- gc_rate - min_rate
  dif_max <- max_rate - gc_rate

  num_levels_temp <- num_levels + 1

  if (max_rate == gc_rate) {
    # if max_rate equals sq_rate

    rates <- seq(min_rate, max_rate, length = num_levels)
  } else if (min_rate == gc_rate) {
    # if min_rate equals sq_rate

    rates <- seq(min_rate, max_rate, length = num_levels)
  } else {
    if (dif_max > dif_min) {
      if (num_levels_temp %% 2 == 1) {
        num_high <- num_levels_temp %/% 2 + 1
        num_low <- num_levels_temp %/% 2
      } else if ((dif_max / dif_min) > 1.5) {
        num_high <- num_levels_temp %/% 2 + 1
        num_low <- num_levels_temp %/% 2 - 1
      } else {
        num_high <- num_levels_temp %/% 2
        num_low <- num_levels_temp %/% 2
      }
    } else {
      if (num_levels_temp %% 2 == 1) {
        num_high <- num_levels_temp %/% 2
        num_low <- num_levels_temp %/% 2 + 1
      } else if ((dif_min / dif_max) > 1.5) {
        num_high <- num_levels_temp %/% 2 - 1
        num_low <- num_levels_temp %/% 2 + 1
      } else {
        num_high <- num_levels_temp %/% 2
        num_low <- num_levels_temp %/% 2
      }
    }

    rates_low <- seq(min_rate, gc_rate, length = num_low) %>% round()
    rates_high <- seq(gc_rate, max_rate, length = num_high) %>% round()

    rates <- c(rates_low, rates_high) %>% unique()
  }

  return(rates)
}

#++++++++++++++++++++++++++++++++++++
#+ Group points by section
#++++++++++++++++++++++++++++++++++++

group_points_sc <- function(data_sf, by_var = NA, angle_threshold) {
  if (!is.na(by_var)) {
    setup_dt <- data_sf %>%
      cbind(., sf::st_coordinates(.)) %>%
      data.table() %>%
      .[, original_order_id := 1:nrow(.)] %>%
      setnames(by_var, "group_var")
  } else {
    by_var <- "group_var"
    setup_dt <- data_sf %>%
      cbind(., sf::st_coordinates(.)) %>%
      data.table() %>%
      .[, original_order_id := 1:nrow(.)] %>%
      .[, group_var := 1]
  }

  # plot(1:39127, angle_dt[!is.na(angle), angle])

  group_dt <- setup_dt %>%
    setorder(group_var, original_order_id) %>%
    .[, d_X := c(0, diff(X)), by = group_var] %>%
    .[, d_Y := c(0, diff(Y)), by = group_var] %>%
    .[, distance := sqrt(d_X^2 + d_Y^2)] %>%
    #--- if distance is 0, then it means the consecutive points are duplicates ---#
    .[distance != 0, ] %>%
    .[, d_X2 := data.table::shift(d_X, type = "lag", fill = NA), by = group_var] %>%
    .[, d_Y2 := data.table::shift(d_Y, type = "lag", fill = NA), by = group_var] %>%
    .[, distance2 := data.table::shift(distance, type = "lag", fill = NA), by = group_var] %>%
    .[, vec_ip_d := (d_X * d_X2 + d_Y * d_Y2) / (distance * distance2)] %>%
    #--- get the angle of three consecutive points ---#
    .[, angle := acos(vec_ip_d) / pi * 180] %>%
    .[0.99 < vec_ip_d, angle := 0] %>%
    #--- 15 is the magic number (may not work) ---#
    .[, change_group := angle >= angle_threshold] %>%
    .[is.na(change_group), change_group := TRUE] %>%
    .[1, change_group := TRUE] %>%
    .[, group := cumsum(change_group), by = group_var] %>%
    .[, obs_per_group := .N, by = group] %>%
    .[obs_per_group > 1, ]


  if (all(group_dt$group_var == 1)) {
    group_dt[, `:=`(
      group_var = NULL,
      vec_ip_d = NULL,
      d_X = NULL,
      d_Y = NULL,
      d_X2 = NULL,
      d_Y2 = NULL,
      distance2 = NULL
    )]
  } else {
    group_dt[, `:=`(
      vec_ip_d = NULL,
      d_X = NULL,
      d_Y = NULL,
      d_X2 = NULL,
      d_Y2 = NULL,
      distance2 = NULL
    )] %>%
      setnames("group_var", by_var)
  }

  return(sf::st_as_sf(group_dt))
}

#++++++++++++++++++++++++++++++++++++
#+ expand.grid for two data frames
#++++++++++++++++++++++++++++++++++++
expand_grid_df <- function(data_1, data_2) {
  expanded_data <- expand.grid(
    index_1 = seq_len(nrow(data_1)),
    index_2 = seq_len(nrow(data_2))
  ) %>%
    tibble::tibble() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      data = list(
        cbind(
          dplyr::slice(data.table(data_1), index_1),
          dplyr::slice(data.table(data_2), index_2)
        )
      )
    ) %>%
    dplyr::select(data) %>%
    dplyr::ungroup() %>%
    .$data %>%
    rbindlist() %>%
    tibble::tibble()

  return(expanded_data)
}

#++++++++++++++++++++++++++++++++++++
#+ Permutation
#++++++++++++++++++++++++++++++++++++
return_permutations <- function(x) {
  get_permutations <- function(x) {
    if (length(x) == 1) {
      return(x)
    } else {
      res <- matrix(nrow = 0, ncol = length(x))
      for (i in seq_along(x)) {
        res <- rbind(res, cbind(x[i], Recall(x[-i])))
      }
    }
    return(res)
  }
  return_list <-
    get_permutations(x) %>%
    t() %>%
    data.frame() %>%
    as.list() %>%
    unname()
  return(return_list)
}

# #++++++++++++++++++++++++++++++++++++
# #+ General unit conversion
# #++++++++++++++++++++++++++++++++++++
# convert_unit <- function(x, from, to) {
#   unit <- std <- NULL
#   if (nrow(subset(.conversions, unit == from, dim)) == 0) {
#     stop("the 'from' argument is not an acceptable unit.")
#   }
#   if (nrow(subset(.conversions, unit == to, dim)) == 0) {
#     stop("the 'to' argument is not an acceptable unit.")
#   }
#   if (subset(.conversions, unit == from, dim) != subset(
#     .conversions,
#     unit == to, dim
#   )) {
#     stop("these units cannot be converted because they are of different dimensions. Try using conv_dim().")
#   }
#   if ((from == "C" | from == "F" | from == "K" | from == "R") &
#     (to == "C" | to == "F" | to == "K" | to == "R")) {
#     frzC <- 0.01
#     frzF <- 32.018
#     frzK <- 273.16
#     frzR <- 491.688
#     boilC <- 99.9839
#     boilF <- 211.97102
#     boilK <- 373.1339
#     boilR <- 671.64102
#     rangeC <- boilC - frzC
#     rangeF <- boilF - frzF
#     rangeK <- boilK - frzK
#     rangeR <- boilR - frzR
#     prop <- (x - get(paste("frz", from, sep = ""))) / get(paste("range",
#       from,
#       sep = ""
#     ))
#     return(prop * get(paste("range", to, sep = "")) + get(paste("frz",
#       to,
#       sep = ""
#     )))
#   }
#   if (from %in% c("dec_deg", "deg_dec_min", "deg_min_sec") &
#     to %in% c("dec_deg", "deg_dec_min", "deg_min_sec")) {
#     neg <- grepl("^-", x)
#     x <- gsub("^-", "", x)
#     NAs <- is.na(x)
#     x_na_free <- x[!NAs]
#     if (from == "dec_deg") {
#       secs <- as.numeric(x_na_free) * 3600
#     }
#     if (from == "deg_dec_min") {
#       secs <- lapply(split(as.numeric(unlist(strsplit(
#         x_na_free,
#         " "
#       ))) * c(3600, 60), f = rep(1:length(x_na_free),
#         each = 2
#       )), sum)
#     }
#     if (from == "deg_min_sec") {
#       secs <- lapply(split(as.numeric(unlist(strsplit(
#         x_na_free,
#         " "
#       ))) * c(3600, 60, 1), f = rep(1:length(x_na_free),
#         each = 3
#       )), sum)
#     }
#     if (to == "dec_deg") {
#       inter <- as.character(lapply(secs, function(y) y / 3600))
#     }
#     if (to == "deg_dec_min") {
#       inter <- paste(
#         lapply(secs, function(y) y %/% 3600),
#         lapply(secs, function(y) y %% 3600 / 60)
#       )
#     }
#     if (to == "deg_min_sec") {
#       inter <- paste(
#         lapply(secs, function(y) y %/% 3600),
#         lapply(secs, function(y) y %% 3600 %/% 60), lapply(
#           secs,
#           function(y) y %% 3600 %% 60
#         )
#       )
#     }
#     x[!NAs] <- inter
#     x <- paste0(ifelse(neg, "-", ""), x)
#     x[grepl("NA", x)] <- NA
#     return(x)
#   }
#   value <- x / subset(.conversions, unit == from, std, drop = TRUE)
#   return(value * subset(.conversions, unit == to, std, drop = TRUE))
# }


#++++++++++++++++++++++++++++++++++++
#+ Ger number in english
#++++++++++++++++++++++++++++++++++++
get_number_in_english <- function(num) {
  return(number_english_dictionary[number == num, num_in_english])
}

#++++++++++++++++++++++++++++++++++++
#+ Calculate dot product
#++++++++++++++++++++++++++++++++++++
get_dot_product <- function(vec_1, vec_2) {
  return(sum(vec_1 * vec_2))
}

#' Assign rates to the plots of experimental plots
#'
#' This functions assign input rates for the plots created by make_exp_plots() according to the rate designs specified by the user in rate_info, which can be created by prep_rateingle().
#'
#' @param exp_data experiment plots created by make_exp_plots()
#' @param rate_info rate information created by prep_rate()
#' @returns trial design as sf (experiment plots with rates assigned)
#' @import data.table
#' @export
#' @examples
#' #--- load experiment plots made by make_exp_plots() ---#
#' data(exp_data)
#' exp_data
#'
#' #--- load rate information ---#
#' data(rate_info)
#' rate_info
#'
#' #--- assign rates ---#
#' td <- assign_rates(exp_data, rate_info)
#'
#' #--- visualization of the assigned rates ---#
#' viz(td)
assign_rates <- function(exp_data, rate_info) {
  if ("data.frame" %in% class(rate_info)) {
    input_trial_data <-
      rate_info %>%
      dplyr::left_join(exp_data, ., by = "input_name")
  } else if ("list" %in% class(rate_info)) {
    input_trial_data <-
      data.table::rbindlist(rate_info, fill = TRUE) %>%
      dplyr::left_join(exp_data, ., by = "input_name")
  }

  if (nrow(input_trial_data) > 1) {
    input_trial_data$push <- c(FALSE, TRUE)
  } else {
    input_trial_data$push <- FALSE
  }

  #  !===========================================================
  # ! Assign rates
  # !===========================================================
  # exp_sf <- input_trial_data$exp_plots[[1]]
  # exp_plots <- input_trial_data$exp_plots[[1]]
  # rates_data <- input_trial_data$rates_data[[1]]
  # rank_seq_ws <- input_trial_data$rank_seq_ws[[1]]
  # rank_seq_as <- input_trial_data$rank_seq_as[[1]]
  # design_type <- input_trial_data$design_type[[1]]
  # push <- input_trial_data$push[[1]]

  trial_design <-
    input_trial_data %>%
    dplyr::rowwise() %>%
    #--- create rates data  ---#
    dplyr::mutate(rates_data = list(
      find_rates_data(
        gc_rate = gc_rate,
        unit = unit,
        rates = tgt_rate_original,
        min_rate = min_rate,
        max_rate = max_rate,
        num_rates = num_rates,
        design_type = design_type
      )
    )) %>%
    dplyr::mutate(experiment_design = list(
      assign_rates_by_input(
        exp_sf = exp_plots,
        rates_data = rates_data,
        rank_seq_ws = rank_seq_ws,
        rank_seq_as = rank_seq_as,
        design_type = design_type,
        push = push
      ) %>%
        dplyr::select(rate, strip_id, plot_id) %>%
        dplyr::mutate(type = "experiment")
    )) %>%
    dplyr::mutate(headland = list(
      dplyr::mutate(headland, rate = gc_rate) %>%
        dplyr::select(rate) %>%
        dplyr::mutate(strip_id = NA, plot_id = NA, type = "headland")
    )) %>%
    dplyr::mutate(input_type = list(
      dplyr::case_when(
        input_name == "seed" ~ "S",
        input_name %in% c("uan28", "uan32", "urea", "NH3", "cover") ~ "N",
        input_name %in% "chicken_manure" ~ "M",
        input_name %in% "forage_pea" ~ "P",
        # === needs to change this ===#
        input_name %in% c("potash", "K") ~ "K",
        input_name == "KCL" ~ "C",
        input_name == "boron" ~ "B"
      )
    )) %>%
    dplyr::mutate(trial_design = list(
      rbind(
        experiment_design,
        headland
      ) %>%
        sf::st_transform(4326)
    )) %>%
    dplyr::mutate(trial_design = list(
      if ("tgts_K" %in% names(trial_design)) {
        dplyr::mutate(trial_design, tgts = trial_design$tgts_K * 1000) %>%
          dplyr::relocate(tgts_K, tgts)
      } else {
        trial_design
      }
    )) %>%
    dplyr::ungroup()

  return(trial_design)
}


# !==================-=========================================
# ! Helper internal functions
# !===========================================================
#* +++++++++++++++++++++++++++++++++++
#* Assign rates (latin and jump-rate-conscious)
#* +++++++++++++++++++++++++++++++++++

assign_rates_by_input <- function(exp_sf, rates_data, rank_seq_ws, rank_seq_as, design_type, push) {
  max_plot_id <- max(exp_sf$plot_id)
  max_strip_id <- max(exp_sf$strip_id)
  num_rates <- nrow(rates_data)

  if (design_type == "ls") {
    #--- get the rate rank sequence within a strip---#
    if (is.null(rank_seq_ws)) {
      basic_seq <- gen_sequence(num_rates, design_type, push)
    } else {
      basic_seq <- rank_seq_ws
    }

    #--- get the starting ranks across strips for the field---#
    if (is.null(rank_seq_as)) {
      start_rank_as <- get_starting_rank_across_strips(num_rates)
    } else {
      start_rank_as <- rank_seq_as
    }

    if (is.null(rank_seq_as) & is.null(rank_seq_ws)) {
      message(
        'Note: You specified neither rank_seq_as or rank_seq_ws. The resulting trial design is equivalent to design_type = "jcls"'
      )
    }

    full_start_seq <-
      rep(
        start_rank_as,
        ceiling(max_strip_id / num_rates) + 1
      ) %>%
      .[1:(max_strip_id + 1)]

    if (push & is.null(rank_seq_as)) {
      full_start_seq <- full_start_seq[2:(max_strip_id + 1)]
    } else {
      full_start_seq <- full_start_seq[1:max_strip_id]
    }

    assigned_rates_data <-
      data.table(
        strip_id = 1:max_strip_id,
        start_rank = full_start_seq
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(rate_rank = list(
        rep(
          get_seq_start(start_rank, basic_seq, strip_id, design_type),
          ceiling(max_plot_id / length(basic_seq))
        )
      )) %>%
      tidyr::unnest(rate_rank) %>%
      data.table() %>%
      .[, dummy := 1] %>%
      .[, plot_id := cumsum(dummy), by = strip_id] %>%
      rates_data[., on = "rate_rank"] %>%
      .[, .(strip_id, plot_id, rate)]

    return_data <-
      dplyr::left_join(
        exp_sf,
        assigned_rates_data,
        by = c("strip_id", "plot_id")
      )
  } else if (design_type == "rb") {
    if (!is.null(rank_seq_ws)) {
      message(
        'Note: rank_seq_ws is ignored when design_type = "rb"'
      )
    }
    if (!is.null(rank_seq_as)) {
      message(
        'Note: rank_seq_as is ignored when design_type = "rb"'
      )
    }

    return_data <-
      exp_sf %>%
      data.table::data.table() %>%
      .[, block_row := ((plot_id - 1) %/% num_rates + 1)] %>%
      .[, block_col := ((strip_id - 1) %/% num_rates + 1)] %>%
      .[, block_id := paste0(block_row, "-", block_col)] %>%
      dplyr::nest_by(block_id) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(data = list(
        dplyr::mutate(data, rate_rank = get_rank_for_rb(num_rates, data))
      )) %>%
      tidyr::unnest(cols = c(data)) %>%
      data.table::data.table() %>%
      rates_data[., on = "rate_rank"] %>%
      .[, block := .GRP, by = block_id] %>%
      .[, `:=`(block_id = NULL, block_row = NULL, block_col = NULL)] %>%
      sf::st_as_sf()
  } else if (design_type == "jcls") {
    #--- get the rate rank sequence within a strip---#
    if (!is.null(rank_seq_ws)) {
      message(
        'Note: rank_seq_ws is ignored when design_type = "jcls"'
      )
    }

    basic_seq <- gen_sequence(num_rates, design_type, push)

    #--- get the starting ranks across strips for the field---#
    if (is.null(rank_seq_as)) {
      start_rank_as <- get_starting_rank_across_strips(num_rates)
    } else {
      start_rank_as <- rank_seq_as
    }

    full_start_seq <-
      rep(
        start_rank_as,
        ceiling(max_strip_id / num_rates) + 1
      ) %>%
      .[1:(max_strip_id + 1)]

    if (push) {
      full_start_seq <- full_start_seq[2:(max_strip_id + 1)]
    } else {
      full_start_seq <- full_start_seq[1:max_strip_id]
    }

    assigned_rates_data <-
      data.table(
        strip_id = 1:max_strip_id,
        start_rank = full_start_seq
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(rate_rank = list(
        rep(
          get_seq_start(start_rank, basic_seq, strip_id, design_type),
          ceiling(max_plot_id / length(basic_seq))
        )
      )) %>%
      tidyr::unnest(rate_rank) %>%
      data.table() %>%
      .[, dummy := 1] %>%
      .[, plot_id := cumsum(dummy), by = strip_id] %>%
      rates_data[., on = "rate_rank"] %>%
      .[, .(strip_id, plot_id, rate)]

    return_data <-
      dplyr::left_join(
        exp_sf,
        assigned_rates_data,
        by = c("strip_id", "plot_id")
      )
  } else if (design_type == "strip") {
    if (!is.null(rank_seq_ws)) {
      message(
        "Note: You specified rank_seq_ws. However, it is irrelevant for strip design and it is ignored."
      )
    }
    if (is.null(rank_seq_as)) {
      start_rank_as <- get_starting_rank_across_strips(num_rates)
    } else {
      start_rank_as <- rank_seq_as
    }

    #--- get the starting ranks across strips for the field---#
    assigned_rates_data <-
      rep(
        start_rank_as,
        ceiling(max_strip_id / length(start_rank_as)) + 1
      ) %>%
      .[1:(max_strip_id + 1)] %>%
      data.table(rate_rank = .) %>%
      .[, strip_id := 1:.N] %>%
      rates_data[., on = "rate_rank"] %>%
      .[, .(strip_id, rate)]

    return_data <-
      dplyr::left_join(
        exp_sf,
        assigned_rates_data,
        by = "strip_id"
      )
  } else if (design_type == "sparse") {
    #* gc_rate is always ranked 1

    #--- get the rate rank sequence within a strip---#
    if (is.null(rank_seq_ws)) {
      basic_seq <- gen_sequence(num_rates, design_type, push)
    } else {
      basic_seq <- rank_seq_ws
    }

    #--- get the starting ranks across strips for the field---#
    if (is.null(rank_seq_as)) {
      start_rank_as <- get_starting_rank_across_strips(num_rates - 1) + 1
    } else {
      start_rank_as <- rank_seq_as
    }

    # === get the starting ranks across strips for the field ===#
    full_start_seq <- rep(
      start_rank_as,
      ceiling(max_strip_id / num_rates) + 1
    ) %>%
      .[1:(max_strip_id + 1)]

    if (push) {
      full_start_seq <- full_start_seq[2:(max_strip_id + 1)]
    } else {
      full_start_seq <- full_start_seq[1:max_strip_id]
    }

    assigned_rates_data <-
      data.table(
        strip_id = 1:max_strip_id,
        start_rank = full_start_seq
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(rate_rank = list(
        rep(
          get_seq_start(start_rank, basic_seq, strip_id, design_type),
          ceiling(max_plot_id / length(basic_seq))
        )
      )) %>%
      tidyr::unnest(rate_rank) %>%
      data.table() %>%
      .[, dummy := 1] %>%
      .[, plot_id := cumsum(dummy), by = strip_id] %>%
      rates_data[., on = "rate_rank"] %>%
      .[, .(strip_id, plot_id, rate)]

    return_data <-
      dplyr::left_join(
        exp_sf,
        assigned_rates_data,
        by = c("strip_id", "plot_id")
      )
  } else if (design_type == "ejca") { # Extra jump-conscious alternate

    rates_data[, tier := ifelse(rate_rank < median(rate_rank), 1, 2)] %>%
      .[, rank_in_tier := rowid(tier)]

    assigned_rates_data <-
      rates_data %>%
      dplyr::nest_by(tier) %>%
      dplyr::mutate(num_levels = nrow(data)) %>%
      dplyr::mutate(basic_seq = list(
        gen_sequence(num_levels, design_type, push)
      )) %>%
      dplyr::mutate(basic_seq = list(
        if (push) {
          c(basic_seq[2:num_rates], basic_seq[1])
        } else {
          basic_seq
        }
      )) %>%
      #--- split the strips to two tiers in an alternate fashion ---#
      dplyr::mutate(strip_plot_data = list(
        if (tier == 1) {
          dplyr::filter(exp_sf, (strip_id %% 2) == 1) %>%
            data.table() %>%
            .[, .(strip_id, plot_id)] %>%
            unique(by = c("strip_id", "plot_id"))
        } else {
          dplyr::filter(exp_sf, (strip_id %% 2) == 0) %>%
            data.table() %>%
            .[, .(strip_id, plot_id)] %>%
            unique(by = c("strip_id", "plot_id"))
        }
      )) %>%
      #--- create new strip id within tier (called group_in_strip) ---#
      dplyr::mutate(strip_plot_data = list(
        strip_plot_data[, group_in_strip := .GRP, by = strip_id]
      )) %>%
      #--- reverse the order of plots alternately---#
      dplyr::mutate(strip_plot_data = list(
        lapply(
          unique(strip_plot_data$strip_id),
          function(x) {
            temp_data <- strip_plot_data[strip_id == x, ]
            if ((unique(temp_data$group_in_strip) %% 2) == 0) {
              temp_data <- temp_data[order(rev(plot_id)), ]
            }
            return(temp_data)
          }
        ) %>%
          rbindlist()
      )) %>%
      dplyr::mutate(strip_plot_data = list(
        strip_plot_data[, rank_in_tier :=
          rep(basic_seq, ceiling(nrow(strip_plot_data) / num_levels))[1:nrow(strip_plot_data)]]
      )) %>%
      dplyr::mutate(rate_data = list(
        data.table(data)[strip_plot_data[, .(strip_id, plot_id, rank_in_tier)], on = "rank_in_tier"]
      )) %>%
      purrr::pluck("rate_data") %>%
      rbindlist()

    return_data <-
      dplyr::left_join(
        exp_sf,
        assigned_rates_data,
        by = c("strip_id", "plot_id")
      )
  }

  return(return_data)
}

gen_sequence <- function(length, design_type, push = FALSE) {
  if (length %% 2 == 0) { # even
    seq_r <- c(seq(1, length, by = 2), seq(length, 2, by = -2))
  } else { # odd
    seq_r <- c(seq(1, length, by = 2), seq(length - 1, 2, by = -2))
  }

  if (design_type == "sparse") {
    seq_r <- seq_r[-1]
    for (i in (seq(1, 2 * length(seq_r) - 1, by = 2))) {
      seq_r <- append(seq_r, 1, after = i)
    }
  }

  if (push) {
    seq_r <- c(seq_r[-1], seq_r[1])
  }

  return(seq_r)
}

get_seq_start <- function(rate_rank, basic_seq, strip_id, design_type) {
  max_rank <- length(basic_seq)
  start_position <- which(basic_seq == rate_rank)

  f_seq <- start_position:max_rank
  s_seq <- 1:start_position

  return_rank <- basic_seq[c(f_seq, s_seq) %>% unique()]

  if (strip_id %% 2 == 0 & design_type == "sparse") {
    return_rank <- append(1, return_rank[-length(return_rank)])
  }

  return(return_rank)
}

get_starting_rank_across_strips <- function(num_levels) {
  return_seq <- sample(1:num_levels, num_levels, replace = FALSE, prob = NULL)

  return(return_seq)
}

get_rank_for_rb <- function(num_rates, data) {
  n_plot <- nrow(data)
  n_comp_block <- n_plot %/% num_rates
  n_plots_remaining <- n_plot %% num_rates
  if (n_comp_block > 0) {
    rate_rank_ls <-
      c(
        c(replicate(n_comp_block, sample(1:num_rates, num_rates, replace = FALSE))),
        sample(1:num_rates, n_plots_remaining, replace = FALSE)
      )
  } else {
    rate_rank_ls <- sample(1:num_rates, n_plots_remaining, replace = FALSE)
  }

  return(rate_rank_ls)
}

find_rates_data <- function(gc_rate, unit, rates = NULL, min_rate = NA, max_rate = NA, num_rates = 5, design_type = NA) {
  #* +++++++++++++++++++++++++++++++++++
  #* Debug
  #* +++++++++++++++++++++++++++++++++++
  # gc_rate <- 180
  # unit <- "lb"
  # rates <- c(100, 140, 180, 220, 260)
  # design_type <- "ls"
  # min_rate <- NA
  # max_rate <- NA
  # num_rates <- 5
  # design_type <- NA
  # rank_seq_ws <- NULL
  # rank_seq_as <- NULL
  #* +++++++++++++++++++++++++++++++++++
  #* Main
  #* +++++++++++++++++++++++++++++++++++

  if (is.na(design_type)) {
    #--- if design_type not specified, use jcls ---#
    design_type <- "jcls"
  } else {
    design_type <- design_type
  }

  #++++++++++++++++++++++++++++++++++++
  #+Specify the trial rates
  #++++++++++++++++++++++++++++++++++++
  if (!is.null(rates)) {
    rates_ls <- rates
  } else if (!is.null(min_rate) & !is.null(max_rate) & !is.null(num_rates)) {
    #--- if min_rate, max_rate, and num_rates are specified ---#
    cat("Trial rates were not directly specified, so the trial rates were calculated using min_rate, max_rate, gc_rate, and num_rates")
    rates_ls <-
      get_rates(
        min_rate,
        max_rate,
        gc_rate,
        num_rates
      )
  } else {
    cat("Please provide either {rates} as a vector or all of {min_rate, max_rate, and num_rates}.")
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Order (rank) rates based on design type
  #++++++++++++++++++++++++++++++++++++
  if (design_type %in% c("ls", "jcls", "strip", "rb")) {
    rates_data <-
      data.table(
        rate = rates_ls,
        rate_rank = 1:length(rates_ls)
      )
  } else if (design_type == "sparse") {
    if (!gc_rate %in% rates_ls) {
      return(print(
        "Error: You specified the trial rates directly using the rates argument, but they do not include gc_rate. For the sparse design, please include gc_rate in the rates."
      ))
    } else {
      rates_ls <- rates_ls[!rates_ls %in% gc_rate]
      rates_data <-
        data.table(
          rate = append(gc_rate, rates_ls),
          rate_rank = 1:(length(rates_ls) + 1)
        )
    }
  } else if (design_type == "ejca") {
    if (length(rates_ls) %% 2 == 1) {
      stop(
        "Error: You cannot have an odd number of rates for the ejca design. Please either specify rates directly with even numbers of rates or specify an even number for num_rates along with min_rate and max_rate."
      )
    } else {
      rates_data <-
        data.table(
          rate = rates_ls,
          rate_rank = 1:length(rates_ls)
        )
    }
  } else {
    stop("Error: design_type you specified does not match any of the design type options available.")
  }

  return(rates_data)
}

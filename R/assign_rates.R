#' Assign rates to the plots of experimental plots
#' 
#' This functions assign input rates for the plots created by make_exp_plots() according to the rate designs specified by the user in rate_info, which can be created by make_input_rate_data()
#' 
#' @param exp_data experiment plots created by make_exp_plots()
#' @param rate_info rate information created by make_input_rate_data()
#' @returns trial design as sf (experiment plots with rates assigned)
#' @import data.table
#' @export
assign_rates <- function(exp_data, rate_info) {

  input_trial_data <-
    data.table::rbindlist(rate_info) %>%
    dplyr::left_join(exp_data, ., by = "form")

  if (nrow(input_trial_data) > 1) {
    input_trial_data$push <- c(FALSE, TRUE)
  } else {
    input_trial_data$push <- FALSE
  }

  #  !===========================================================
  # ! Assign rates
  # !===========================================================
  # exp_plots <- input_trial_data$exp_plots[[2]]
  # exp_sf <- input_trial_data$exp_plots[[2]]
  # rates_data <- input_trial_data$rates_data[[2]]
  # design_type <- input_trial_data$design_type[[2]]
  # push <- input_trial_data$push[[2]]

  trial_design <-
    input_trial_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(experiment_design = list(
      assign_rates_by_input(
        exp_sf = exp_plots,
        rates_data = rates_data,
        design_type = design_type,
        push = push
      ) %>%
        dplyr::select(rate)
    )) %>%
    dplyr::mutate(headland = list(
      dplyr::mutate(headland, rate = round(gc_rate)) %>%
        dplyr::select(rate)
    )) %>%
    dplyr::mutate(input_type = list(
      dplyr::case_when(
        form == "seed" ~ "S",
        form %in% c("uan28", "uan32", "urea", "NH3", "cover") ~ "N",
        form %in% "chicken_manure" ~ "M",
        form %in% "forage_pea" ~ "P",
        # === needs to change this ===#
        form %in% c("potash", "K") ~ "K",
        form == "KCL" ~ "C",
        form == "boron" ~ "B"
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
    dplyr::select(
      form, input_type, trial_design, design_type, unit, ab_lines, harvest_ab_lines, field_sf
    ) %>%
    dplyr::ungroup()

  return(trial_design)
}


#' Create data of input rate information for a single input
#'
#' Create data of input rate information for a single input. This can be used to assign rates to experimentl plots using assign_rates()
#'
#' @param plot_info (data.frame) plot information created by make_input_plot_data
#' @param gc_rate (numeric) Input ate the grower would have chosen if not running an experiment. This rate is assigned to the non-experiment part of the field. This rate also becomes one of the trial input rates unless you specify the trial rates directly using rates argument
#' @param unit (string) unit of input
#' @param rates (numeric vector) Default is NULL. Sequence of trial rates in the ascending order. 
#' @param min_rate (numeric) minimum input rate. Ignored if rates are specified.
#' @param max_rate (numeric) maximum input rate. Ignored if rates are specified
#' @param num_rates (numeric) Default is 5. It has to be an even number if design_type is "ejca". Ignored if rates are specified.
#' @param design_type (string) type of trial design. available options are "jcl", "sparse", and "ejca". See for more details. 
#' @returns data.frame of input rate information
#' @import data.table
#' @export
make_input_rate_data <- function(plot_info, gc_rate, unit, rates = NULL, min_rate = NULL, max_rate = NULL, num_rates = 5, design_type = NA) {

  #--- extract form and unit ---#
  input_trial_data <- dplyr::select(plot_info, form)

  #++++++++++++++++++++++++++++++++++++
  #+Design type
  #++++++++++++++++++++++++++++++++++++
  if (is.na(design_type)) {
    #--- if design_type not specified, use jcl ---#
    input_trial_data$design_type <- "jcl"
  } else {
    input_trial_data$design_type <- design_type
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
  if (design_type == "jcl") {
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
      return(print(
        "Error: You cannot have an odd number of rates for the ejca design. Please either specify rates directly with even numbers of rates or specify an even number for num_rates along with min_rate and max_rate."
      ))
    } else {
      rates_data <-
        data.table(
          rate = rates_ls,
          rate_rank = 1:length(rates_ls)
        )
    }
  }

  input_trial_data$gc_rate <- gc_rate
  input_trial_data$unit <- unit
  input_trial_data$rates_data <- list(rates_data)

  return(input_trial_data)
}


# !==================-=========================================
# ! Helper internal functions
# !===========================================================
#*+++++++++++++++++++++++++++++++++++
#* Assign rates (latin and jump-rate-conscious)
#*+++++++++++++++++++++++++++++++++++

assign_rates_by_input <- function(exp_sf, rates_data, design_type, push) {
  max_plot_id <- max(exp_sf$plot_id)
  max_strip_id <- max(exp_sf$strip_id)
  num_rates <- nrow(rates_data)

  if (design_type == "jcl") {

    #--- get the rate rank sequence within a strip---#
    basic_seq <- gen_sequence(num_rates, design_type, push)

    #--- get the starting ranks across strips for the field---#
    full_start_seq <-
      rep(
        get_starting_rank_across_strips(num_rates),
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
  } else if (design_type == "sparse") {

  } else if (design_type == "ejca") { # Extra jump-conscious alternate

    rates_data[, tier := ifelse(rate_rank < median(rate_rank), 1, 2)] %>%
      .[, rank_in_tier := rowid(tier)]

    assigned_rates_data <- rates_data %>%
      dplyr::nest_by(tier) %>%
      dplyr::mutate(num_levels = nrow(data)) %>%
      dplyr::mutate(basic_seq = list(
        gen_sequence(num_rates, design_type, push)
      )) %>%
      dplyr::mutate(basic_seq = list(
        if (push) {
          c(basic_seq[2:num_rates], basic_seq[1])
        } else {
          basic_seq
        }
      )) %>%
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
          rep(basic_seq, ceiling(nrow(strip_plot_data) / num_rates))[1:nrow(strip_plot_data)]]
      )) %>%
      dplyr::mutate(rate_data = list(
        data.table(data)[strip_plot_data[, .(strip_id, plot_id, rank_in_tier)], on = "rank_in_tier"]
      )) %>%
      purrr::pluck("rate_data") %>%
      rbindlist()
  }

  return_data <-
    dplyr::left_join(
      exp_sf,
      assigned_rates_data,
      by = c("strip_id", "plot_id")
    )
  # ggplot() +
  #   geom_sf(data = return_data, aes(fill = factor(rate)), color = NA) +
  #   scale_fill_viridis_d()

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
    return_rank <- append(0, return_rank[-length(return_rank)])
  }

  return(return_rank)
}

get_starting_rank_across_strips <- function(num_levels) {
  return_seq <- sample(1:num_levels, num_levels, replace = FALSE, prob = NULL)

  return(return_seq)
}


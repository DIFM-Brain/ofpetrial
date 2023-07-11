#' Create data of input rate information for a single input
#'
#' Create data of input rate information for a single input. This can be used to assign rates to experiment plots using assign_rates()
#'
#' @param plot_info (data.frame) plot information created by make_input_plot_data
#' @param gc_rate (numeric) Input ate the grower would have chosen if not running an experiment. This rate is assigned to the non-experiment part of the field. This rate also becomes one of the trial input rates unless you specify the trial rates directly using rates argument
#' @param unit (string) unit of input
#' @param rates (numeric vector) Default is NULL. Sequence of trial rates in the ascending order.
#' @param min_rate (numeric) minimum input rate. Ignored if rates are specified.
#' @param max_rate (numeric) maximum input rate. Ignored if rates are specified
#' @param num_rates (numeric) Default is 5. It has to be an even number if design_type is "ejca". Ignored if rates are specified.
#' @param design_type (string) type of trial design. available options are Latin Square ("ls"), Strip ("strip"), Randomized Block ("rb"), Jump-conscious Latin Square ("jcls"), Sparse ("sparse"), and Extra Jump-conscious Alternate "ejca". See for more details.
#' @param rank_seq_ws (integer) vector of integers indicating the order of the ranking of the rates, which will be repeated "within" a strip.
#' @param rank_seq_as (integer) vector of integers indicating the order of the ranking of the rates, which will be repeated "across" strip for their first plots.
#' @returns data.frame of input rate information
#' @import data.table
#' @export
#' @examples
#' plot_info <-
#'   prep_plot_fs(
#'     input_name = "seed",
#'     machine_width = 60,
#'     section_num = 24,
#'     harvester_width = 30,
#'     plot_width = 30
#'   )
#'
#' prep_rates_s(
#'   plot_info,
#'   gc_rate = 30000,
#'   unit = "seeds",
#'   rates = c(20000, 25000, 30000, 35000, 40000)
#' )
prep_rates_s <- function(plot_info, gc_rate, unit, rates = NULL, min_rate = NA, max_rate = NA, num_rates = 5, design_type = NA, rank_seq_ws = NULL, rank_seq_as = NULL) {

  #--- extract input_name and unit ---#
  input_trial_data <- dplyr::select(plot_info, input_name)

  #++++++++++++++++++++++++++++++++++++
  #+Design type
  #++++++++++++++++++++++++++++++++++++
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
    stop("Error: design_type you specified does not match any of the options available.")
  }

  input_trial_data$design_type <- design_type
  input_trial_data$gc_rate <- gc_rate
  input_trial_data$unit <- unit
  input_trial_data$rates_data <- list(rates_data)
  input_trial_data$rank_seq_ws <- list(rank_seq_ws)
  input_trial_data$rank_seq_as <- list(rank_seq_as)

  return(input_trial_data)
}

#' Create data of input rate information for a single input
#'
#' Create data of input rate information for a single input. This can be used to assign rates to experiment plots using assign_rates()
#'
#' @param plot_info (data.frame) plot information created by make_input_plot_data
#' @param gc_rate (numeric) Input ate the grower would have chosen if not running an experiment. This rate is assigned to the non-experiment part of the field. This rate also becomes one of the trial input rates unless you specify the trial rates directly using rates argument
#' @param unit (string) unit of input
#' @param rates (numeric vector) Default is NULL. Sequence of trial rates in the ascending order.
#' @param min_rate (numeric) minimum input rate. Ignored if rates are specified.
#' @param max_rate (numeric) maximum input rate. Ignored if rates are specified
#' @param num_rates (numeric) Default is 5. It has to be an even number if design_type is "ejca". Ignored if rates are specified.
#' @param design_type (string) type of trial design. available options are Latin Square ("ls"), Strip ("strip"), Randomized Block ("rb"), Jump-conscious Latin Square ("jcls"), Sparse ("sparse"), and Extra Jump-conscious Alternate "ejca". See for more details.
#' @param rank_seq_ws (integer) vector of integers indicating the order of the ranking of the rates, which will be repeated "within" a strip.
#' @param rank_seq_as (integer) vector of integers indicating the order of the ranking of the rates, which will be repeated "across" strip for their first plots.
#' @returns data.frame of input rate information
#' @import data.table
#' @export
#' @examples
#' seed_plot_info <-
#'   prep_plot_fs(
#'     input_name = "seed",
#'     machine_width = 60,
#'     section_num = 24,
#'     harvester_width = 30,
#'     plot_width = 30
#'   )
#'
#' n_plot_info <-
#'   prep_plot_ms(
#'     input_name = "NH3",
#'     machine_width = measurements::conv_unit(60, "ft", "m"),
#'     section_num = 1,
#'     harvester_width = measurements::conv_unit(30, "ft", "m"),
#'     plot_width = measurements::conv_unit(60, "ft", "m")
#'   )
#'
#' plot_info <- list(seed_plot_info, n_plot_info)
#'
#' prep_rates_d(
#'   plot_info,
#'   gc_rate = c(30000, 160),
#'   unit = c("seeds", "lb"),
#'   rates = list(
#'     c(20000, 25000, 30000, 35000, 40000),
#'     c(100, 130, 160, 190, 220)
#'   )
#' )
#'
prep_rates_d <- function(plot_info, gc_rate, unit, rates = list(NULL, NULL), min_rate = c(NA, NA), max_rate = c(NA, NA), num_rates = c(5, 5), design_type = c(NA, NA), rank_seq_ws = list(NULL, NULL), rank_seq_as = list(NULL, NULL)) {
  if (class(plot_info) == "list") {
    plot_info <- dplyr::bind_rows(plot_info)
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Dimension Checks
  #++++++++++++++++++++++++++++++++++++
  if (nrow(plot_info) != 2) {
    stop("Plot information (plot_info argument) should consist of two rows. Please check.")
  }

  fms_ls <- c(
    nrow(plot_info), length(gc_rate), length(unit), length(rates), length(min_rate),
    length(max_rate), length(num_rates), length(design_type), length(rank_seq_ws), length(rank_seq_as)
  )
  if (any(fms_ls != 2)) {
    stop("Inconsistent numbers of elements in the arguments.")
  }

  #--- extract input_name and unit ---#
  input_trial_data <-
    dplyr::select(plot_info, input_name) %>%
    #++++++++++++++++++++++++++++++++++++
    #+ design type
    #++++++++++++++++++++++++++++++++++++
    dplyr::mutate(design_type = design_type) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      design_type =
        if (is.na(design_type)) {
          #--- if design_type not specified, use jcls ---#
          "jcls"
        } else {
          design_type
        }
    ) %>%
    dplyr::ungroup() %>%
    #++++++++++++++++++++++++++++++++++++
    #+Specify the trial rates
    #++++++++++++++++++++++++++++++++++++
    dplyr::mutate(rates = rates) %>%
    dplyr::mutate(gc_rate = gc_rate) %>%
    dplyr::mutate(min_rate = min_rate) %>%
    dplyr::mutate(max_rate = max_rate) %>%
    dplyr::mutate(num_rates = num_rates) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rates_ls = list(
      if (!is.null(rates)) {
        rates
      } else if (!is.na(min_rate) & !is.na(max_rate) & !is.na(num_rates)) {
        #--- if min_rate, max_rate, and num_rates are specified ---#
        message("Trial rates were not specified directly for ", input_name, " so the trial rates were calculated using min_rate, max_rate, gc_rate, and num_rates\n")
        get_rates(
          min_rate,
          max_rate,
          gc_rate,
          num_rates
        )
      } else {
        stop("Please provide either {rates} as a vector or all of {min_rate, max_rate, and num_rates}.")
      }
    )) %>%
    dplyr::mutate(rates_data = list(
      if (design_type %in% c("ls", "jcls", "strip", "rb")) {
        data.table(
          rate = rates_ls,
          rate_rank = 1:length(rates_ls)
        )
      } else if (design_type == "sparse") {
        if (!gc_rate %in% rates_ls) {
          stop(
            "Error: You specified the trial rates directly using the rates argument, but they do not include gc_rate. For the sparse design, please include gc_rate in the rates."
          )
        } else {
          rates_ls <- rates_ls[!rates_ls %in% gc_rate]
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
          data.table(
            rate = rates_ls,
            rate_rank = 1:length(rates_ls)
          )
        }
      } else {
        stop("Error: design_type you specified does not match any of the options available.")
      }
    )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(unit = unit) %>%
    dplyr::mutate(rank_seq_ws = rank_seq_ws) %>%
    dplyr::mutate(rank_seq_as = rank_seq_as)

  return(input_trial_data)
}

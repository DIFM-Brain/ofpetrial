#' Create data of input rate information for a single input
#'
#' Create data of input rate information for a single input with some checks on the validity of the information provided by the user. This can be used to assign rates to experiment plots using assign_rates().
#'
#' @param plot_info (data.frame) plot information created by make_input_plot_data
#' @param gc_rate (numeric) Input rate the grower would have chosen if not running an experiment. This rate is assigned to the non-experiment part of the field. This rate also becomes one of the trial input rates unless you specify the trial rates directly using rates argument
#' @param unit (string) unit of input
#' @param rates (numeric vector) Default is NULL. Sequence of trial rates in the ascending order.
#' @param min_rate (numeric) minimum input rate. Ignored if rates are specified.
#' @param max_rate (numeric) maximum input rate. Ignored if rates are specified
#' @param num_rates (numeric) Default is 5. It has to be an even number if design_type is "ejca". Ignored if rates are specified.
#' @param design_type (string) type of trial design. available options are Latin Square ("ls"), Strip ("str"), Randomized Strip ("rstr"), Randomized Block ("rb"), Sparse ("sparse"), and Extra Jump-conscious Alternate "ejca". See the article on trial design for more details.
#' @param rank_seq_ws (integer) vector of integers indicating the order of the ranking of the rates, which will be repeated "within" a strip.
#' @param rank_seq_as (integer) vector of integers indicating the order of the ranking of the rates, which will be repeated "across" strip for their first plots.
#' @param rate_jump_threshold (integer) highest jump in rate rank acceptable
#' @returns data.frame of input rate information
#' @import data.table
#' @export
#' @examples
#' plot_info <-
#'   prep_plot(
#'     input_name = "seed",
#'     unit_system = "imperial",
#'     machine_width = 60,
#'     section_num = 24,
#'     harvester_width = 30,
#'     plot_width = 30
#'   )
#'
#' prep_rate(
#'   plot_info,
#'   gc_rate = 30000,
#'   unit = "seeds",
#'   rates = c(20000, 25000, 30000, 35000, 40000)
#' )
prep_rate <- function(plot_info, gc_rate, unit, rates = NULL, min_rate = NA, max_rate = NA, num_rates = 5, design_type = NA, rank_seq_ws = NULL, rank_seq_as = NULL, rate_jump_threshold = NA) {
  #* +++++++++++++++++++++++++++++++++++
  #* Main
  #* +++++++++++++++++++++++++++++++++++
  #--- extract input_name and unit ---#
  input_trial_data <- dplyr::select(plot_info, input_name)

  #++++++++++++++++++++++++++++++++++++
  #+Specify the trial rates
  #++++++++++++++++++++++++++++++++++++
  if (!is.null(rates)) {
    rates_ls <- rates
    num_rates <- length(rates)
    if (!(gc_rate %in% rates)) {
      message("Please note that gc_rate is not one of the rates you specified.")
    }
  } else if (!is.null(min_rate) & !is.null(max_rate) & !is.null(num_rates)) {
    #--- if min_rate, max_rate, and num_rates are specified ---#
    message("Trial rates were not directly specified via the {rates} option, so the trial rates will be calculated using min_rate, max_rate, gc_rate, and num_rates")
    rates_ls <-
      get_rates(
        min_rate,
        max_rate,
        gc_rate,
        num_rates
      )
  } else {
    message("Please provide either {rates} as a vector or all of {min_rate, max_rate, and num_rates}.")
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Order (rank) rates based on design type
  #++++++++++++++++++++++++++++++++++++
  if (is.na(design_type)) {
    #--- do nothing ---#
    message("You did not specify design_type. It is assumed to be of type ls.")
  } else if (design_type %in% c("ls", "str", "rstr", "rb")) {
    #--- do nothing ---#
  } else if (design_type == "sparse") {
    if (!gc_rate %in% rates_ls) {
      stop(
        "Error: You specified the trial rates directly using the rates argument, but they do not include gc_rate. For the sparse design, please include gc_rate in the rates."
      )
    }
  } else if (design_type == "ejca") {
    if (length(rates_ls) %% 2 == 1) {
      stop(
        "Error: You cannot have an odd number of rates for the ejca design. Please either specify rates directly with even numbers of rates or specify an even number for num_rates along with min_rate and max_rate."
      )
    }
  } else {
    stop("Error: design_type you specified does not match any of the design type options available.")
  }

  # # conversions
  # warning(paste0("Please ensure that the applicator is compatible with applying ", input_trial_data$input_name, " in ", unit, "."))

  tgt_rate_original <- rates_ls

  #++++++++++++++++++++++++++++++++++++
  #+ Convert origninal unit to equivalent
  #++++++++++++++++++++++++++++++++++++
  # try to convert if the input is anything other than seed
  # if the combination of input and inut is not found, the conversion factor is simply 1

  #--- trial rates ---#
  tgt_rate_equiv <- convert_rates(input_trial_data$input_name, unit, rates_ls)

  #++++++++++++++++++++++++++++++++++++
  #+ Assign all the values to the returnig object
  #++++++++++++++++++++++++++++++++++++

  # creating final data set
  input_trial_data$design_type <- design_type
  input_trial_data$gc_rate <- gc_rate
  input_trial_data$unit <- unit
  input_trial_data$tgt_rate_original <- list(tgt_rate_original)
  input_trial_data$tgt_rate_equiv <- list(tgt_rate_equiv)
  input_trial_data$min_rate <- min_rate
  input_trial_data$max_rate <- max_rate
  input_trial_data$num_rates <- num_rates
  input_trial_data$rank_seq_ws <- list(rank_seq_ws)
  input_trial_data$rank_seq_as <- list(rank_seq_as)
  input_trial_data$rate_jump_threshold <- rate_jump_threshold

  return(input_trial_data)
}


# !===========================================================
# ! Helper internal functions
# !===========================================================
# Convert nitrogen units to N_equivalent

convert_rates <- function(input_name,
                          unit,
                          rate,
                          conversion_type = "to_n_equiv") {
  if (input_name == "seed") {
    converted_rate <- rate
  } else {
    # change rates to the imperial form for the table
    if (unit == "liters") {
      rate <- conv_unit(rate, "liters", "gallons")
      new_unit <- "gallons"
      reporting_unit <- "metric"
    } else if (unit == "kg") {
      rate <- conv_unit(rate, "kg", "pounds")
      new_unit <- "lb"
      reporting_unit <- "metric"
    } else {
      rate <- rate
      new_unit <- unit
      reporting_unit <- "imperial"
    }

    if (input_name == "N_equiv") {
      conv_factor_n <- 1
    } else {
      conv_factor_n <-
        which(input_unit_conversion_table[, "form_unit"] %in% paste(input_name, new_unit, sep = "_")) %>%
        input_unit_conversion_table[., "conv_factor"]
    }
    if (is.numeric(conv_factor_n) == FALSE) {
      message("There is no combination of your specific input name and unit for conversion into target nutrient rate. We will assume the conversion is 1, and the target rates will be in your given unit.")
      conv_factor_n <- 1
    }

    if (reporting_unit == "metric") {
      conv_factor_n <- conv_factor_n * conv_unit(1, "pounds", "kg") * conv_unit(1, "hectares", "acres")
    }

    if (conversion_type == "to_n_equiv") {
      converted_rate <- (conv_factor_n) * rate
    } else {
      converted_rate <- (1 / conv_factor_n) * rate
    }
  }

  return(as.numeric(converted_rate))
}

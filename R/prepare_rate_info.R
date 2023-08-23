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
#' @param design_type (string) type of trial design. available options are Latin Square ("ls"), Strip ("strip"), Randomized Block ("rb"), Jump-conscious Latin Square ("jcls"), Sparse ("sparse"), and Extra Jump-conscious Alternate "ejca". See for more details.
#' @param rank_seq_ws (integer) vector of integers indicating the order of the ranking of the rates, which will be repeated "within" a strip.
#' @param rank_seq_as (integer) vector of integers indicating the order of the ranking of the rates, which will be repeated "across" strip for their first plots.
#' @param base_rate (integer) optional base application information created by add_base_rate
#' @returns data.frame of input rate information
#' @import data.table
#' @export
#' @examples
#' plot_info <-
#'   prep_plot_f(
#'     input_name = "seed",
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
prep_rate <- function(plot_info, gc_rate, unit, rates = NULL, min_rate = NA, max_rate = NA, num_rates = 5, design_type = NA, rank_seq_ws = NULL, rank_seq_as = NULL, base_rate = NULL) {
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
  } else if (!is.null(min_rate) & !is.null(max_rate) & !is.null(num_rates)) {
    #--- if min_rate, max_rate, and num_rates are specified ---#
    cat("Trial rates were not directly specified via the {rates} option, so the trial rates will be calculated using min_rate, max_rate, gc_rate, and num_rates")
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
  if (is.na(design_type)) {
    #--- do nothing ---#
  } else if (design_type %in% c("ls", "jcls", "strip", "rb")) {
    #--- do nothing ---#
  } else if (design_type == "sparse") {
    if (!gc_rate %in% rates_ls) {
      return(print(
        "Error: You specified the trial rates directly using the rates argument, but they do not include gc_rate. For the sparse design, please include gc_rate in the rates."
      ))
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

  if (is.null(base_rate) == FALSE) {
    base_rate_original <- base_rate$rate

    base_rate_equiv <- convert_rates(base_rate$input_name, base_rate$unit, base_rate$rate)

    base_input <- base_rate$input_name
  } else {
    base_rate_equiv <- 0
  }

  tgt_rate_original <- rates_ls

  # try to convert if the input is anything other than seed
  # if the combination of input and inut is not found, the conversion factor is simply 1
  if (input_trial_data$input_name != "seed") {
    tgt_rate_equiv <- convert_rates(input_trial_data$input_name, unit, rates)
  } else {
    tgt_rate_equiv <- tgt_rate_original
  }

  # creating final data set
  input_trial_data$design_type <- design_type
  input_trial_data$gc_rate <- gc_rate
  input_trial_data$unit <- unit
  input_trial_data$tgt_rate_original <- list(tgt_rate_original)
  input_trial_data$tgt_rate_equiv <- list(tgt_rate_equiv)
  if (is.null(base_rate) == FALSE) {
    input_trial_data$include_base_rate <- TRUE
    input_trial_data$base_input <- base_input
    input_trial_data$base_rate_original <- base_rate_original
    input_trial_data$base_rate_equiv <- base_rate_equiv
  }else{
    input_trial_data$include_base_rate <- FALSE
  }
  input_trial_data$total_equiv <- list(tgt_rate_original + base_rate_equiv)
  input_trial_data$min_rate <- min_rate
  input_trial_data$max_rate <- max_rate
  input_trial_data$num_rates <- num_rates
  input_trial_data$rank_seq_ws <- list(rank_seq_ws)
  input_trial_data$rank_seq_as <- list(rank_seq_as)

  return(input_trial_data)
}

#' Create data of base application rate information for a single input
#'
#' Create data of base application rate information for a single input when applicable. This can be added when preparing trial rates using prepare_rates().
#'
#' @param base_input_name (string) input name
#' @param base_unit (string) unit of input applied at base
#' @param base_rate (numeric) amount of input applied
#' @returns data.frame of base rate information
#' @import data.table
#' @export
#' @examples
#' base_info <-
#'   add_base_rate(
#'     "uan28",
#'     "gallons",
#'     15
#'   )
#'
add_base_rate <- function(base_input_name, base_unit, base_rate) {
  base_info <- data.frame(
    input_name = base_input_name,
    unit = base_unit,
    rate = base_rate
  )

  return(base_info)
}

# !===========================================================
# ! Helper internal functions
# !===========================================================
# Convert nitrogen units to N_equivalent

convert_rates <- function(input_name,
                          unit,
                          rate,
                          conversion_type = "to_n_equiv") {
  # change rates to the imperial form for the table
  if (unit == "liters") {
    rate <- conv_unit(rate, "l", "us_gal")
    new_unit <- "gallons"
    reporting_unit <- "metric"
  } else if (unit == "kg") {
    rate <- conv_unit(rate, "kg", "lb")
    new_unit <- "lb"
    reporting_unit <- "metric"
  } else {
    rate <- rate
    new_unit <- unit
    reporting_unit <- "imperial"
  }

  conv_table <-
    jsonlite::fromJSON(
      system.file("extdata", "nitrogen_conversion.json", package = "ofpetrial"),
      flatten = TRUE
    ) %>%
    data.table() %>%
    .[, conv_factor := as.numeric(conv_factor)] %>%
    .[, form_unit := paste(type, unit, sep = "_")] %>%
    as.data.frame()

  if (input_name == "N_equiv") {
    conv_factor_n <- 1
  } else {
    conv_factor_n <- which(conv_table[, "form_unit"] %in% paste(input_name, new_unit, sep = "_")) %>%
      conv_table[., "conv_factor"]
  }
  if (is.numeric(conv_factor_n) == FALSE) {
    message("There is no combination of your specific input name and unit for conversion into target nutrient rate. We will assume the conversion is 1, and the target rates will be in your given unit.")
    conv_factor_n <- 1
  }

  if (reporting_unit == "metric") {
    conv_factor_n <- conv_factor_n * conv_unit(1, "lbs", "kg") * conv_unit(1, "hectare", "acre")
  }

  if (conversion_type == "to_n_equiv") {
    converted_rate <- (conv_factor_n) * rate
  } else {
    converted_rate <- (1 / conv_factor_n) * rate
  }

  return(as.numeric(converted_rate))
}

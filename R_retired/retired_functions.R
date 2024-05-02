
#' Create data of input rate information for two inputs at the same time
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

mod_rate_info <- function(rate_info, what, new_value) {
  #*+++++++++++++++++++++++++++++++++++
  #* Debug
  #*+++++++++++++++++++++++++++++++++++
  # data(rate_info)
  # what <- "rates"
  # new_value <- c(100, 130, 180, 230, 290)

  #*+++++++++++++++++++++++++++++++++++
  #* Main
  #*+++++++++++++++++++++++++++++++++++
  if (!what %in% colnames(rate_info)) {
    stop('The component you are referring to with "what" does not exist.')
  }

  setnames(rate_info, what, "temp")
  if (is.null(new_value)) {
    rate_info <- dplyr::mutate(rate_info, temp = new_value)
  } else if (length(new_value) > 1) {
    rate_info <- dplyr::mutate(rate_info, temp = list(new_value))
  }
  setnames(rate_info, "temp", what)
  
  return_info <- 
    prep_rates_s(
    plot_info = rate_info$plot_info[[1]],
    gc_rate = rate_info$gc_rate,
    unit = rate_info$unit,
    rates = rate_info$rates[[1]],
    min_rate = rate_info$min_rate,
    max_rate = rate_info$max_rate,
    num_rates = rate_info$num_rates,
    design_type = rate_info$design_type,
    rank_seq_ws = rate_info$rank_seq_ws,
    rank_seq_as = rate_info$rank_seq_as
    )
  
  return(return_info)
}

#' Prepare plot information for a two-input experiment (length in meter)
#'
#' Prepare plot information for a two-input experiment case. All the length values need to be specified in meter.
#'
#' @param input_name (character) A vector of two input names
#' @param machine_width (numeric) A vector of two numeric numbers in meter that indicate the width of the applicator or planter of the inputs 
#' @param section_num (numeric) A vector of two numeric numbers that indicate the number of sections of the applicator or planter of the inputs
#' @param harvester_width (numeric) A numeric number in meter that indicates the width of the harvester
#' @param plot_width (numeric) A vector of two numeric numbers in meter that indicate the plot width for each of the two inputs. Default is c(NA, NA). 
#' @param headland_length (numeric) A numeric number in meter that indicates the length of the headland (how long the non-experimental space is in the direction machines drive). Default is NA.
#' @param side_length (numeric) A numeric number in meter that indicates the length of the two sides of the field (how long the non-experimental space is in the direction perpendicular to the direction of machines). Default is NA.
#' @param max_plot_width (numeric) Maximum width of the plots in meter. Default is 36.576 meter (120 feet).
#' @param min_plot_length (numeric) Minimum length of the plots in meter. Default is 73.152 meter (240 feet).
#' @param max_plot_length (numeric) Maximum length of the plots in meter. Default is 91.44 meter (300 feet).
#' @returns a tibble with plot information necessary to create experiment plots
#' @import data.table
#' @export
#' @examples
#' input_name <- c("seed", "NH3")
#' machine_width <- c(12, 9)
#' section_num <- c(12, 1)
#' plot_width <- c(12, 36)
#' harvester_width <- 12
#' prep_plot_md(input_name, machine_width, section_num, harvester_width, plot_width)
#'
prep_plot_md <- function(input_name,
                         machine_width,
                         section_num,
                         harvester_width,
                         plot_width = c(NA, NA),
                         headland_length = NA,
                         side_length = NA,
                         max_plot_width = measurements::conv_unit(120, "ft", "m"), # 36.4576 meter
                         min_plot_length = measurements::conv_unit(240, "ft", "m"), # 73.152 feet
                         max_plot_length = measurements::conv_unit(300, "ft", "m") # 79.248 meter
) {

  #--- dimension check ---#
  fms_ls <- c(length(input_name), length(machine_width), length(section_num), length(plot_width))
  if (any(fms_ls != 2)) {
    stop("Inconsistent numbers of elements in input_name, machine_width, section_num, and plot_width. Check if all of them have two elements.")
  }

  section_width <- machine_width / section_num

  plot_data <-
    data.frame(
      input_name = input_name,
      machine_width = machine_width,
      section_num = section_num,
      harvester_width = harvester_width,
      plot_width = plot_width
    ) %>%
    dplyr::mutate(section_width = machine_width / section_num) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      lcm_found =
        lcm_check(section_width, harvester_width, max_plot_width)
    ) %>%
    dplyr::mutate(
      proposed_plot_width =
        find_plotwidth(section_width, harvester_width, max_plot_width)
    ) %>%
    dplyr::mutate(plot_width = ifelse(is.na(plot_width), proposed_plot_width, plot_width))

  for (i in 1:nrow(plot_data)) {
    temp <- plot_data[i, ]
      if ((temp$plot_width %% temp$section_width) != 0) {
        stop(paste0(
          "Plot width provided is not a multiple of the machine (section) width for ", temp$input_name,".\n"
        ))
     }
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Check and notify the mixed treatment problems (with potential suggestions)
  #++++++++++++++++++++++++++++++++++++
  warning_message <-
    plot_data %>%
    dplyr::mutate(messages = list(
      if (lcm_found & plot_width %% proposed_plot_width == 0 & proposed_plot_width < plot_width) {
        paste0(
          "For ", input_name, ", there is a plot width that is smaller than the plot width you suggested and avoids mixed treatement problem. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
      } else if (lcm_found & plot_width %% proposed_plot_width != 0) {
        paste0(
          "For ", input_name, ", the plot width you specified would cause mixed treatment problems. However, there is a plot width that avoids them. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
      } else if (!lcm_found & plot_width != proposed_plot_width) {
        paste0(
          "For ", input_name, ", the plot width you specified would cause mixed treatment problems. Unfortunately, there is no plot width that avoids them. Plot width of ", proposed_plot_width, " ensures that at least one harvest path within the path of ", input_name, " does not have the problems."
        )
      } else {
        NULL
      }
    )) %>%
    dplyr::pull(messages)

  #--- notify the user of potential problems and improvements ---#
  message(unlist(warning_message))

  #--- warnd the user that they may have serious mixed treatment problems   ---#
  if (all(!plot_data$lcm_found)) {
    message(paste0(
      "Neither of ", input_name[1], " and ", input_name[2], " does not have a plot width without mixed treatment problems. Please consider running experiments with a single input instead of two."
    ))
  }

  #++++++++++++++++++++++++++++++++++++
  #+ head and side lengths
  #++++++++++++++++++++++++++++++++++++
  #--- head distance ---#
  if (is.na(headland_length)) {
    headland_length <- 2 * max(machine_width)
  }

  #--- side distance ---#
  if (is.na(side_length)) {
    side_length <- max(max(section_width), measurements::conv_unit(30, "ft", "m"))
  }

  #++++++++++++++++++++++++++++++++++++
  #+ put together the data
  #++++++++++++++++++++++++++++++++++++
  plot_data <-
    plot_data %>%
    dplyr::mutate(
      headland_length = headland_length,
      side_length = side_length,
      min_plot_length = min_plot_length,
      max_plot_length = max_plot_length
    ) %>%
    dplyr::select(
      input_name, machine_width, section_num, section_width, harvester_width, plot_width, proposed_plot_width, headland_length, side_length, min_plot_length, max_plot_length
    ) %>%
    dplyr::ungroup()

  return(plot_data)
}

#' Prepare plot information for a two-input experiment (length in feet)
#'
#' Prepare plot information for a two-input experiment case. All the length values need to be specified in feet.
#'
#' @param input_name (character) A vector of two input names
#' @param machine_width (numeric) A vector of two numeric numbers in feet that indicate the width of the applicator or planter of the inputs 
#' @param section_num (numeric) A vector of two numeric numbers that indicate the number of sections of the applicator or planter of the inputs
#' @param harvester_width (numeric) A numeric number in feet that indicates the width of the harvester
#' @param plot_width (numeric) A vector of two numeric numbers in feet that indicate the plot width for each of the two inputs. Default is c(NA, NA). 
#' @param headland_length (numeric) A numeric number in feet that indicates the length of the headland (how long the non-experimental space is in the direction machines drive). Default is NA.
#' @param side_length (numeric) A numeric number in feet that indicates the length of the two sides of the field (how long the non-experimental space is in the direction perpendicular to the direction of machines). Default is NA.
#' @param max_plot_width (numeric) Maximum width of the plots in feet. Default is (36.576 meter).
#' @param min_plot_length (numeric) Minimum length of the plots in feet. Default is 240 feet (73.152 meter).
#' @param max_plot_length (numeric) Maximum length of the plots in feet. Default is 300 feet (91.44 meter).
#' @returns a tibble with plot information necessary to create experiment plots
#' @import data.table
#' @export
#' @examples
#' input_name <- c("seed", "NH3")
#' machine_width <- c(12, 9)
#' section_num <- c(12, 1)
#' plot_width <- c(12, 36)
#' harvester_width <- 12
#' prep_plot_fd(input_name, machine_width, section_num, harvester_width, plot_width)
#'
prep_plot_fd <- function(input_name,
                         machine_width,
                         section_num,
                         harvester_width,
                         plot_width = c(NA, NA),
                         headland_length = NA,
                         side_length = NA,
                         max_plot_width = 120,
                         min_plot_length = 240,
                         max_plot_length = 300
) {

  #--- dimension check ---#
  fms_ls <- c(length(input_name), length(machine_width), length(section_num), length(plot_width))
  if (any(fms_ls != 2)) {
    stop("Inconsistent numbers of elements in input_name, machine_width, section_num, and plot_width. Check if all of them have two elements.")
  }

  section_width <- machine_width / section_num

  plot_data <-
    data.frame(
      input_name = input_name,
      machine_width = machine_width,
      section_num = section_num,
      harvester_width = harvester_width,
      plot_width = plot_width
    ) %>%
    dplyr::mutate(section_width = machine_width / section_num) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      lcm_found =
        lcm_check(section_width, harvester_width, max_plot_width)
    ) %>%
    dplyr::mutate(
      proposed_plot_width =
        find_plotwidth(section_width, harvester_width, max_plot_width)
    ) %>%
    dplyr::mutate(plot_width = ifelse(is.na(plot_width), proposed_plot_width, plot_width))
  
  for (i in 1:nrow(plot_data)) {
    temp <- plot_data[i, ]
      if ((temp$plot_width %% temp$section_width) != 0) {
        stop(paste0(
          "Plot width provided is not a multiple of the machine (section) width for ", temp$input_name,".\n"
        ))
     }
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Check and notify the mixed treatment problems (with potential suggestions)
  #++++++++++++++++++++++++++++++++++++
  warning_message <-
    plot_data %>%
    dplyr::mutate(messages = list(
      if (lcm_found & plot_width %% proposed_plot_width == 0 & proposed_plot_width < plot_width) {
        paste0(
          "For ", input_name, ", there is a plot width that is smaller than the plot width you suggested and avoids mixed treatement problem. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
      } else if (lcm_found & plot_width %% proposed_plot_width != 0) {
        paste0(
          "For ", input_name, ", the plot width you specified would cause mixed treatment problems. However, there is a plot width that avoids them. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
      } else if (!lcm_found & plot_width != proposed_plot_width) {
        paste0(
          "For ", input_name, ", the plot width you specified would cause mixed treatment problems. Unfortunately, there is no plot width that avoids them. Plot width of ", proposed_plot_width, " ensures that at least one harvest path within the path of ", input_name, " does not have the problems."
        )
      } else {
        NULL
      }
    )) %>%
    dplyr::pull(messages)

  #--- notify the user of potential problems and improvements ---#
  message(unlist(warning_message))

  #--- warnd the user that they may have serious mixed treatment problems   ---#
  if (all(!plot_data$lcm_found)) {
    message(paste0(
      "Neither of ", input_name[1], " and ", input_name[2], " does not have a plot width without mixed treatment problems. Please consider running experiments with a single input instead of two."
    ))
  }

  #++++++++++++++++++++++++++++++++++++
  #+ head and side lengths
  #++++++++++++++++++++++++++++++++++++
  #--- head distance ---#
  if (is.na(headland_length)) {
    headland_length <- 2 * max(machine_width)
  }

  #--- side distance ---#
  if (is.na(side_length)) {
    side_length <- max(max(section_width), 30)
  }
  #++++++++++++++++++++++++++++++++++++
  #+ put together the data
  #++++++++++++++++++++++++++++++++++++
  plot_data <-
    plot_data %>%
    dplyr::mutate(
      headland_length = headland_length,
      side_length = side_length,
      min_plot_length = min_plot_length,
      max_plot_length = max_plot_length
    ) %>%
    dplyr::select(
      input_name, machine_width, section_num, section_width, harvester_width, plot_width, proposed_plot_width, headland_length, side_length, min_plot_length, max_plot_length
    )

  #++++++++++++++++++++++++++++++++++++
  #+ Unit converstion (feet to meter)
  #++++++++++++++++++++++++++++++++++++
  cols_conv <- c("machine_width", "section_width", "harvester_width", "plot_width", "proposed_plot_width", "headland_length", "side_length", "min_plot_length", "max_plot_length")

  plot_data <- dplyr::mutate(plot_data, dplyr::across(dplyr::all_of(cols_conv), ~ measurements::conv_unit(.x, "ft", "m")))

  # plot_data[, (cols_conv) := lapply(.SD, function(x) measurements::conv_unit(x, "ft", "m")), .SDcols = cols_conv]

  return(plot_data)
}

change_rate_by_plot(
  td,
  input_name = "NH3",
  strip_ids = 1:10,
  plot_ids = c(1, 5, 9, 14, 19, 24),
  new_rates = 0
) %>%
viz(abline = FALSE)

#!===========================================================
#! Change rates
#!===========================================================

#' Change the assigned rate by block
#'
#' Change the assigned rates by block on trial design
#'
#' @param td trial design
#' @param input_name (character) input name
#' @param block_ids (numeric) vector of block_ids
#' @param new_rates (numeric) vector of rates
#' @returns trial design with changed rates
#' @import data.table
#' @export
#' @examples
#' #--- load rate information ---#
#' data(td_single_input)
#'
#' #--- add blocks ---#
#' td_with_blocks <- add_blocks(td_single_input)
#'
#' #--- change rates of some blocks ---#
#' block_ids <- c(3, 5)
#' new_rates <- c(180, 180)
#'
#' td_modified <- change_rate_by_block(td_with_blocks, "NH3", block_ids, new_rates)
#'
#' #--- visualize ---#
#' viz(td_modified)
change_rate_by_block <- function(td, input_name, block_ids, new_rates) {
  temp_design <-
    td %>%
    dplyr::filter(input_name == input_name) %>%
    .$trial_design %>%
    .[[1]]

  headland_rate <- dplyr::filter(temp_design, type != "experiment") %>% dplyr::pull(rate)

  for (i in seq_len(length(block_ids))) {
    temp_design <-
      temp_design %>%
      dplyr::mutate(rate = ifelse(block_id == block_ids[i], new_rates[i], rate))
  }

  temp_design <- dplyr::mutate(temp_design, rate = ifelse(type == "experiment", rate, headland_rate))

  return_td <- dplyr::mutate(td, trial_design = ifelse(input_name == input_name, list(temp_design), list(trial_design)))

  return(return_td)
}

#' Change the assigned rate by strip
#'
#' Change the assigned rates by strip on trial design
#'
#' @param td trial design
#' @param input_name (character) input name
#' @param strip_ids (numeric) vector of strip_ids
#' @param new_rates (numeric) vector of rates
#' @returns trial design with changed rates
#' @import data.table
#' @export
#' @examples
#' #--- load rate information ---#
#' data(td_single_input)
#'
#' #--- change rates of some strips ---#
#' strip_ids <- c(1, 6, 11, 16, 21)
#' new_rates <- rep(0, length(strip_ids))
#'
#' td_modified <- change_rate_by_strip(td_single_input, "NH3", strip_ids, new_rates)
#'
#' #--- visualize ---#
#' viz(td_modified)
change_rate_by_strip <- function(td, input_name, strip_ids, new_rates) {
  temp_design <-
    td %>%
    dplyr::filter(input_name == input_name) %>%
    .$trial_design %>%
    .[[1]]

  headland_rate <- dplyr::filter(temp_design, type != "experiment") %>% dplyr::pull(rate)

  num_strips <- length(strip_ids)
  num_rates <- length(new_rates)

  if (num_strips == num_rates & num_strips != 1) {
    for (i in seq_len(length(strip_ids))) {
      temp_design <- dplyr::mutate(temp_design, rate = ifelse(strip_id == strip_ids[i], new_rates[i], rate))
    }
  } else if (num_rates == 1) {
    temp_design <- dplyr::mutate(temp_design, rate = ifelse(strip_id %in% strip_ids, new_rates, rate))
  } else {
    stop("Inconsistent numbers of strip_ids and new_rates. If new_rates has more than one values, then its length must be the same as the length of strip_ids.")
  }

  temp_design <- dplyr::mutate(temp_design, rate = ifelse(type == "experiment", rate, headland_rate))

  return_td <- dplyr::mutate(td, trial_design = ifelse(input_name == input_name, list(temp_design), list(trial_design)))

  return(return_td)
}

#' Change the assigned rate by plot
#'
#' Change the assigned rates by plot on trial design
#'
#' @param td trial design
#' @param input_name (character) input name
#' @param strip_ids (numeric) vector of strip_ids
#' @param plot_ids (numeric) vector of plot_ids
#' @param new_rates (numeric) scalar number that is assigned to all the plots specified using strip_ids and plot_ids
#' @returns trial design with changed rates
#' @import data.table
#' @export
#' @examples
#' #--- load rate information ---#
#' data(td_single_input)
#'
#' #--- change rates of some strips ---#
#' strip_ids <- 1:5
#' plot_ids <- 5:10
#' new_rates <- 200
#'
#' td_modified <- change_rate_by_plot(td_single_input, "NH3", strip_ids, plot_ids, new_rates)
#'
#' #--- visualize ---#
#' viz(td_modified)
change_rate_by_plot <- function(td, input_name, strip_ids, plot_ids, new_rates, rate_by = "all") {
  temp_design <-
    td %>%
    dplyr::filter(input_name == input_name) %>%
    .$trial_design %>%
    .[[1]]

  headland_rate <- dplyr::filter(temp_design, type != "experiment") %>% dplyr::pull(rate)

  if (rate_by == "all") {
    temp_design <-
      temp_design %>%
      dplyr::mutate(rate = ifelse(strip_id %in% strip_ids & plot_id %in% plot_ids, new_rates, rate)) %>%
      dplyr::mutate(rate = ifelse(type == "experiment", rate, headland_rate))
  } else if (rate_by == "strip") {
    if (length(strip_ids) != length(new_rates)) {
      stop('The number of strips in "strip_ids" and rates in "new_rates" do not matcch.')
    }
    for (s in 1:length(strip_ids)) {
      temp_design <-
        dplyr::mutate(temp_design, rate = ifelse(strip_id == strip_ids[s] & plot_id %in% plot_ids, new_rates[s], rate))
    }
    temp_design <- dplyr::mutate(temp_design, rate = ifelse(type == "experiment", rate, headland_rate))
  }

  return_td <- dplyr::mutate(td, trial_design = ifelse(input_name == input_name, list(temp_design), list(trial_design)))

  return(return_td)
}

make_design_for_2_by_2 <- function(input_trial_data_with_rates, non_rectangle) {
  if (non_rectangle == FALSE) {
    num_strips <-
      input_trial_data_with_rates$exp_plots[[1]] %>%
      pull(strip_id) %>%
      max()

    #--- starting sequence for first input ---#
    full_start_seq_1 <-
      rep(
        c(1, 2),
        ceiling(num_strips / 2)
      ) %>%
      .[1:num_strips]

    #--- starting sequence for second input ---#
    full_start_seq_2 <-
      rep(
        c(1, 1, 2, 2),
        ceiling(num_strips / 4)
      ) %>%
      .[1:num_strips]

    plots_with_rates_assigned <-
      data.table(input_trial_data_with_rates) %>%
      dplyr::mutate(basic_seq = list(c(1, 2), c(1, 2))) %>%
      dplyr::mutate(full_start_seq = list(full_start_seq_1, full_start_seq_2)) %>%
      rowwise() %>%
      dplyr::mutate(max_plot_id = list(max(exp_plots$plot_id))) %>%
      dplyr::mutate(experiment_design = list(
        data.table::data.table(
          strip_id = 1:num_strips,
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
          data.table::data.table() %>%
          .[, dummy := 1] %>%
          rates_data[., on = "rate_rank"] %>%
          .[, plot_id := cumsum(dummy), by = strip_id] %>%
          .[, .(strip_id, plot_id, rate)] %>%
          left_join(exp_plots, ., by = c("strip_id", "plot_id")) %>%
          dplyr::select(rate, strip_id, plot_id) %>%
          dplyr::mutate(type = "experiment")
      )) %>%
      dplyr::select(-basic_seq, -full_start_seq, -max_plot_id)
  } else {
    print("It is indicated that this field is NOT rectangular. This may take some time as ensuring spatial balance and maximizing local variance of the input rate take more care in this case.")

    exp_plots <- input_trial_data_with_rates$exp_plots[[1]]
    num_strips <- max(exp_plots$strip_id)

    design_1_ls <- vector("list", length = num_strips)
    design_2_ls <- vector("list", length = num_strips)

    print("Creating the design for the first input...")

    for (i in 1:num_strips) {
      working_strip_1 <- dplyr::filter(exp_plots, strip_id == i)
      num_plots <- max(working_strip_1$plot_id)

      if (i == 1) {
        # first input alternates its rate in the moving direction
        working_strip_1$rate_rank <- rep(c(1, 2), ceiling(num_plots / 2))[1:num_plots]
      } else { # if even strip number
        prev_strip_1 <- design_1_ls[[i - 1]]

        closest_index <-
          st_distance(
            st_centroid_quietly(st_as_sf(working_strip_1)),
            st_centroid_quietly(st_as_sf(prev_strip_1))
          ) %>%
          apply(., 1, which.min)

        rate_ranks_nb <-
          prev_strip_1[closest_index, ] %>%
          pull(rate_rank)

        rate_ranks_option_1 <- rep(c(1, 2), ceiling(num_plots / 2))[1:num_plots]
        rate_ranks_option_2 <- rep(c(2, 1), ceiling(num_plots / 2))[1:num_plots]

        variability_score_1 <- abs(rate_ranks_option_1 - rate_ranks_nb) %>% sum()
        variability_score_2 <- abs(rate_ranks_option_2 - rate_ranks_nb) %>% sum()

        if (variability_score_1 >= variability_score_2) {
          working_strip_1$rate_rank <- rate_ranks_option_1
        } else {
          working_strip_1$rate_rank <- rate_ranks_option_2
        }
      }

      design_1_ls[[i]] <- data.table(working_strip_1)
    }

    design_first_input <-
      rbindlist(design_1_ls) %>%
      sf::st_as_sf() %>%
      left_join(., input_trial_data_with_rates$rates_data[[1]], by = "rate_rank")

    # ggplot(data = design_first_input) +
    #   geom_sf(aes(fill = factor(rate_rank)))

    #++++++++++++++++++++++++++++++++++++
    #+ Second input
    #++++++++++++++++++++++++++++++++++++
    print("Creating the design for the second input...")

    comb_table <-
      data.table::CJ(
        rate_rank_1 = c(1, 2),
        rate_rank_2 = c(1, 2)
      ) %>%
      .[, cases := 0]

    rate_table <-
      data.table(exp_plots) %>%
      .[, .(plot_id, strip_id)] %>%
      .[, rate_rank := 0]

    for (row_index in 1:nrow(exp_plots)) {
      # print(row_index)

      #---------------------
      #- Progress tracking
      #---------------------
      track_data <-
        data.table::data.table(
          plot_id = round(nrow(exp_plots) * seq(0.1, 1, by = 0.1), digits = 0)
        ) %>%
        .[, index := 1:.N]

      if (row_index %in% round(nrow(exp_plots) * seq(0.1, 1, by = 0.1), digits = 0)) {
        percentage <- track_data[plot_id == row_index, index]
        print(paste(paste0(rep("==", percentage), collapse = ""), percentage * 10, "% complete"))
      }
      working_plot <- exp_plots[row_index, ]
      working_strip_id <- working_plot$strip_id
      rate_rank_1st <- data.table(design_first_input)[row_index, rate_rank]
      rate_rank_2nd_prev <- rate_table[row_index - 1, rate_rank]

      if (row_index == 1) {
        rate_rank_2nd <- 1
      } else {
        if (working_strip_id == 1) {
          rate_rank_2nd <-
            comb_table[rate_rank_1 == rate_rank_1st, ] %>%
            .[cases == min(cases), ] %>%
            .[, score := abs(rate_rank_2 - rate_rank_2nd_prev)] %>%
            .[score == max(score), ] %>%
            .[sample(1:.N, 1), ] %>%
            .[, rate_rank_2]
        } else {
          if (working_strip_id == 2) {
            prev_strip <- dplyr::filter(exp_plots, strip_id == (working_plot$strip_id - 1))

            plot_nb <-
              st_distance(
                st_centroid_quietly(working_plot),
                st_centroid_quietly(prev_strip)
              ) %>%
              which.min() %>%
              prev_strip[., ]

            rate_rank_2nd_nb <-
              dplyr::filter(rate_table, strip_id == plot_nb$strip_id & plot_id == plot_nb$plot_id) %>%
              pull(rate_rank)

            rate_rank_2nd <-
              comb_table[rate_rank_1 == rate_rank_1st, ] %>%
              .[cases == min(cases), ] %>%
              .[, score := abs(rate_rank_2 - rate_rank_2nd_nb) + abs(rate_rank_2 - rate_rank_2nd_prev)] %>%
              .[score == max(score), ] %>%
              .[sample(1:.N, 1), ] %>%
              .[, rate_rank_2]

          } else {
            prev_strip <- dplyr::filter(exp_plots, strip_id == (working_plot$strip_id - 1))
            prev_prev_strip <- dplyr::filter(exp_plots, strip_id == (working_plot$strip_id - 2))

            plot_nb <-
              st_distance(
                st_centroid_quietly(working_plot),
                st_centroid_quietly(prev_strip)
              ) %>%
              which.min() %>%
              prev_strip[., ]

            rate_rank_2nd_nb <-
              dplyr::filter(rate_table, strip_id == plot_nb$strip_id & plot_id == plot_nb$plot_id) %>%
              pull(rate_rank)

            plot_nb_nb <-
              st_distance(
                st_centroid_quietly(plot_nb),
                st_centroid_quietly(prev_prev_strip)
              ) %>%
              which.min() %>%
              prev_prev_strip[., ]

            rate_rank_2nd_nb_nb <-
              dplyr::filter(rate_table, strip_id == plot_nb_nb$strip_id & plot_id == plot_nb_nb$plot_id) %>%
              pull(rate_rank)

            rate_rank_2nd <-
              comb_table[rate_rank_1 == rate_rank_1st, ] %>%
              .[cases == min(cases), ] %>%
              #--- local variability score ---#
              # slightly prefer rate that is different from the previous plot
              .[, score := abs(rate_rank_2 - rate_rank_2nd_nb) + abs(rate_rank_2 - rate_rank_2nd_nb_nb) + abs(rate_rank_2 - rate_rank_2nd_prev) * 1.5] %>%
              .[score == max(score), ] %>%
              .[sample(1:.N, 1), ] %>%
              .[, rate_rank_2]
          }
        }
      }

      rate_table[row_index, rate_rank := rate_rank_2nd]
      update_comb_table(comb_table, rate_rank_1st, rate_rank_2nd)

      print(paste(rate_rank_1st, "-", rate_rank_2nd))
    }

    design_second_input <-
      left_join(exp_plots, rate_table, by = c("plot_id", "strip_id")) %>%
      left_join(., input_trial_data_with_rates$rates_data[[2]], by = "rate_rank")

    input_trial_data_with_rates$experiment_design <- list(design_first_input, design_second_input)

    plots_with_rates_assigned <-
      input_trial_data_with_rates %>%
      dplyr::mutate(experiment_design = list(
        experiment_design %>%
          dplyr::select(rate, strip_id, plot_id) %>%
          dplyr::mutate(type = "experiment")
      ))
    # temp <- left_join(design_first_input, data.table(second_design)[, .(plot_id, strip_id, rate_rank)], by = c("plot_id", "strip_id"))

    # ggplot(temp) +
    #   geom_sf(aes(fill = rate_rank.y))

    # data.table(temp)[, .N, by = .(rate_rank.x, rate_rank.y)]
  }
}

make_design_for_2_by_2 <- function(input_trial_data_with_rates) {
  #---------------------
  #- Initial setup
  #---------------------
  #--- get experimental plots ---#
  # this code is only for the case with identical experimental plots for the two inputs
  exp_sf <- input_trial_data_with_rates$exp_plots[[1]]
  #--- define rate_rank ---#
  exp_sf$rate_rank <- NA
  #--- find the number of strips ---#
  max_strip_id <- max(exp_sf$strip_id)
  #--- initiate shift counter ---#
  shift_counter <- 0
  #--- initiate an empty lis ---#
  # to be filled with strips with assigned rates sequentially
  strip_list <- vector(mode = "list", max_strip_id)

  #--- get the rate rank sequence within a strip
  basic_seq <- c(1, 3, 2, 4)
  num_rates <- 4
  # start_rank_as <- get_starting_rank_across_strips_ls(num_rates, basic_seq)
  start_rank_as <- c(1, 3, 2, 4)
  #--- elongated starting ranking sequence across strips ---#
  full_start_seq_long <-
    rep(
      start_rank_as,
      ceiling(max_strip_id / num_rates) + 5
    )

  rates_data <-
    data.table::CJ(
      rate_rank_1 = 1:2,
      rate_rank_2 = 1:2
    ) %>%
    .[, rate_rank := 1:.N]

  #---------------------
  #- Assign rates
  #---------------------
  for (i in 1:max(exp_sf$strip_id)) {
    working_strip <- dplyr::filter(exp_sf, strip_id == i)
    start_rank <- full_start_seq_long[i + shift_counter]

    if (i == 1) {
      rate_ranks <-
        rep(
          get_seq_start(start_rank, basic_seq),
          ceiling(max(working_strip$plot_id) / length(basic_seq))
        ) %>%
        .[1:max(working_strip$plot_id)]
    } else {
      previous_strip <- strip_list[[i - 1]]

      rate_ranks <-
        rep(
          get_seq_start(start_rank, basic_seq),
          ceiling(max(working_strip$plot_id) / length(basic_seq))
        ) %>%
        .[1:max(working_strip$plot_id)]

      suppressWarnings(
        neighbor_rate_ranks <-
          st_distance(st_centroid_quietly(working_strip), st_centroid_quietly(previous_strip)) %>%
          apply(1, which.min) %>%
          previous_strip[., ] %>%
          pull(rate_rank)
      )

      duplication_score <- mean(rate_ranks == neighbor_rate_ranks)

      # print(paste0("dup score = ", duplication_score))

      if (duplication_score > 0.5) {
        # create a new start_rank sequence that starts with a value except the current one
        shift_counter <- shift_counter + 1
        start_rank <- full_start_seq_long[i + shift_counter]

        # determine rates
        rate_ranks <-
          rep(
            get_seq_start(start_rank, basic_seq),
            ceiling(max(working_strip$plot_id) / length(basic_seq))
          ) %>%
          .[1:max(working_strip$plot_id)]
      }
    }
    # exp_sf <- dplyr::mutate(exp_sf, rate_rank = ifelse(strip_id == i, rate_ranks, rate_rank))
    working_strip$rate_rank <- rate_ranks
    strip_list[[i]] <- working_strip
  }

  intermediate_design <-
    data.table::rbindlist(strip_list) %>%
    st_as_sf() %>%
    left_join(., rates_data, by = "rate_rank")

  # ggplot(intermediate_design) +
  #   geom_sf(aes(fill = rate_rank))

  design_first_input <-
    intermediate_design %>%
    dplyr::select(plot_id, strip_id, rate_rank_1) %>%
    left_join(., input_trial_data_with_rates$rates_data[[1]], by = c("rate_rank_1" = "rate_rank"))

  design_second_input <-
    intermediate_design %>%
    dplyr::select(plot_id, strip_id, rate_rank_2) %>%
    left_join(., input_trial_data_with_rates$rates_data[[2]], by = c("rate_rank_2" = "rate_rank"))

  # ggplot(design_2nd_input) +
  #   geom_sf(aes(fill = rate))

  # ggplot(design_1st_input) +
  #   geom_sf(aes(fill = rate))
  input_trial_data_with_rates$experiment_design <- list(design_first_input, design_second_input)

  plots_with_rates_assigned <-
    input_trial_data_with_rates %>%
    dplyr::mutate(experiment_design = list(
      experiment_design %>%
        dplyr::select(rate, strip_id, plot_id) %>%
        dplyr::mutate(type = "experiment")
    ))

  return(plots_with_rates_assigned)
}

add_base_rate <- function(base_input_name, base_unit, base_rate) {
  base_info <- data.frame(
    input_name = base_input_name,
    unit = base_unit,
    rate = base_rate
  )

  return(base_info)
}
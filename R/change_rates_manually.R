#' Add blocks to trial design
#'
#' Delineate blocks on a trial design and assign block id to all the plots
#'
#' @param td trial design made by applying assign_rates() to experimental plots made by make_exp_plots()
#' @returns trial design with block_id added
#' @import data.table
#' @export
#' @examples
#' #--- load rate information ---#
#' data(td_single_input)
#'
#' #--- add blocks ---#
#' td_with_blocks <- add_blocks(td_single_input)
#'
#' #--- take a look ---#
#' td_with_blocks$trial_design
#'
#' #--- visualize ---#
#' viz(td_with_blocks, type = "block_id")
#'
add_blocks <- function(td) {
  td_return <-
    td %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      num_rates =
        dplyr::filter(trial_design, type == "experiment")$rate %>% unique() %>% length()
    ) %>%
    dplyr::mutate(trial_design = list(
      trial_design %>%
        data.table::data.table() %>%
        .[, block_row := ((plot_id - 1) %/% num_rates + 1)] %>%
        .[, block_col := ((strip_id - 1) %/% num_rates + 1)] %>%
        .[, block_id := .GRP, by = .(block_row, block_col)] %>%
        .[type == "headland", block_id := NA] %>%
        .[, plot_id_within_block := 1:.N, by = block_id] %>%
        .[type == "headland", plot_id_within_block := NA] %>%
        .[, `:=`(block_row = NULL, block_col = NULL)] %>%
        sf::st_as_sf()
    ))
  return(td_return)
}

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
    stop('Inconsistent numbers of strip_ids and new_rates. If new_rates has more than one values, then its length must be the same as the length of strip_ids.')
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
#' @param new_rate (numeric) scalar number that is assigned to all the plots specified using strip_ids and plot_ids
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
#' new_rate <- 200
#'
#' td_modified <- change_rate_by_plot(td_single_input, "NH3", strip_ids, plot_ids, new_rate)
#'
#' #--- visualize ---#
#' viz(td_modified)
change_rate_by_plot <- function(td, input_name, strip_ids, plot_ids, new_rate) {
  temp_design <-
    td %>%
    dplyr::filter(input_name == input_name) %>%
    .$trial_design %>%
    .[[1]]

  headland_rate <- dplyr::filter(temp_design, type != "experiment") %>% dplyr::pull(rate)

  temp_design <-
    temp_design %>%
    dplyr::mutate(rate = ifelse(strip_id %in% strip_ids & plot_id %in% plot_ids, new_rate, rate)) %>%
    dplyr::mutate(rate = ifelse(type == "experiment", rate, headland_rate))

  return_td <- dplyr::mutate(td, trial_design = ifelse(input_name == input_name, list(temp_design), list(trial_design)))

  return(return_td)
}

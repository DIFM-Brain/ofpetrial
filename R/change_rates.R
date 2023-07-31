
#' Change the assigned rates
#'
#' Change the assigned rates by plot and strip
#'
#' @param td trial design
#' @param input_name (character) input name
#' @param strip_ids (numeric) vector of strip_ids
#' @param plot_ids (numeric) vector of plot_ids
#' @param new_rates (numeric) single numeric number for `rate_by = "all"``, a vector of numeric values for `rate_by = "strip"``, a matrix of numeric numbers for `rate_by = "plot"`.
#' @param rate_by (character) default is "all". The other options are "plot" and "strip".
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
#' td_modified <- change_rates(td_single_input, "NH3", strip_ids, plot_ids, new_rates)
#'
#' #--- visualize ---#
#' viz(td_modified)
change_rates <- function(td, input_name = NA, strip_ids, plot_ids = NULL, new_rates, rate_by = "all") {

  #++++++++++++++++++++++++++++++++++++
  #+ Debug
  #++++++++++++++++++++++++++++++++++++
  # data(td_single_input)
  # td <- td_single_input

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  if (nrow(td) == 1) {
    temp_design <- td$trial_design[[1]]
  } else {
    if (is.na(input_name)) {
      stop('Please specify which input you want to change rates for using the "input)name" option.')
    } else {
      temp_design <-
        td %>%
        dplyr::filter(input_name == input_name) %>%
        .$trial_design %>%
        .[[1]]
    }
  }

  headland_rate <- dplyr::filter(temp_design, type != "experiment") %>% dplyr::pull(rate)

  if (rate_by == "all") {
    if (length(new_rates) != 1) {
      stop('For "rate_by == all" (default), you can provide only a single numeric number to "new_rate"')
    }

    if (is.null(plot_ids)) {
      #--- applied to all the plots in the specified strips ---#
      temp_design <-
        temp_design %>%
        dplyr::mutate(rate = ifelse(strip_id %in% strip_ids, new_rates, rate))
    } else {
      temp_design <-
        temp_design %>%
        dplyr::mutate(rate = ifelse(strip_id %in% strip_ids & plot_id %in% plot_ids, new_rates, rate))
    }
  } else if (rate_by == "strip") {
    #--- error message if inconsistent lengths ---#
    if (length(strip_ids) != length(new_rates)) {
      stop('The number of strips in "strip_ids" and rates in "new_rates" do not matcch.')
    }

    if (is.null(plot_ids)) {
      #--- if plot_ids are missing, then all the plots in the strips ---#
      for (s in 1:length(strip_ids)) {
        temp_design <-
          dplyr::mutate(temp_design, rate = ifelse(strip_id == strip_ids[s], new_rates[s], rate))
      }
    } else {
      for (s in 1:length(strip_ids)) {
        temp_design <-
          dplyr::mutate(temp_design, rate = ifelse(strip_id == strip_ids[s] & plot_id %in% plot_ids, new_rates[s], rate))
      }
    }
  } else if (rate_by == "plot") {
    if (nrow(new_rates) != length(plot_ids) | ncol(new_rates) != length(strip_ids)) {
      stop('Inconsistent dimension of the numeric matrix provided to "new_rate" and the number of "plot_ids" (row) and "strip_ids" (column).\n')
    }
    if (is.null(plot_ids)) {
      stop('For rate_by = plot, you must specify plot_ids.') 
    }
    temp_design_dt <- data.table(temp_design)
    for (i in 1:length(strip_ids)) {
      temp_design_dt[plot_id %in% plot_ids & strip_id %in% strip_ids[i], rate := new_rates[, i]]
    }
    temp_design <- st_as_sf(temp_design_dt)
  }

  temp_design <- dplyr::mutate(temp_design, rate = ifelse(type == "experiment", rate, headland_rate))

  return_td <- dplyr::mutate(td, trial_design = ifelse(input_name == input_name, list(temp_design), list(trial_design)))

  return(return_td)
}

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
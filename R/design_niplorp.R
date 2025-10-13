#' Design a Nitrogen-induced Profit Loss Recovery Program (NIPLORP) experiment
#'
#' Design an experiment for the Nitrogen-induced Profit Loss Recovery Program (NIPLORP)
#'
#' @param exp_data Experiment plots generated using make_exp_plots().
#' @param N_ref A numeric value representing the nitrogen rate for the reference plots.
#' @param N_reduced A numeric value representing the nitrogen rate for the plots with reducend nitrogen.
#' @param ref_pct A numeric value between 0 and 1 representing the proportion of reference plots in the design. Default is 0.25.
#' @param N_unit A character string representing the unit of nitrogen rate.
#'
#' @returns A tibble containing the trial design with assigned nitrogen rates.
#' @import data.table
#' @export
#' @examples
#'
#' data(exp_data)
#'
#' td <- design_niplorp(exp_data, N_ref = 200, N_reduced = 120, N_unit = "lb", ref_pct = 0.2)
#'
#' viz(td)
design_niplorp <- function(exp_data, N_ref, N_reduced, ref_pct = 0.25, N_unit) {
  # ref_pct = 0.4
  # N_ref <- 200
  # N_reduced <- 100

  pct_data <-
    data.table::CJ(
      plot_num = 2:9,
      ref_num = 1:9
    ) %>%
    .[plot_num > ref_num, ] %>%
    .[, pct := ref_num / plot_num] %>%
    .[, dif_pct := abs(pct - ref_pct)] %>%
    .[dif_pct == min(dif_pct), ] %>%
    .[plot_num == min(plot_num), ]

  rates_design <-
    data.table::CJ(
      strip_id = 1:pct_data$plot_num,
      plot_id = 1:pct_data$plot_num
    ) %>%
    .[, score := pct_data$plot_num]

  num_ref <- pct_data$ref_num
  num_reduced <- pct_data$plot_num - pct_data$ref_num
  dominant_rate <- ifelse(num_ref > num_reduced, "Reference", "Reduced")

  plot_ids <- (1:pct_data$plot_num)

  for (strip in (1:pct_data$plot_num)) {
    if (strip == 1) {
      # strip <- 1
      ref_plot_ids <-
        plot_ids[plot_ids %% 2 == 1] %>%
        .[1:num_ref]

      rates_design[strip_id == 1, rate := ifelse(plot_id %in% ref_plot_ids, N_ref, N_reduced)]

      rates_design[plot_id %in% ref_plot_ids, score := score - 1]
    } else {
      # strip <- 2

      if (strip %% 2) {
        plot_id_seq <-
          rates_design %>%
          .[strip_id == strip, ] %>%
          .[order(-score, -plot_id), plot_id]
      } else {
        plot_id_seq <-
          rates_design %>%
          .[strip_id == strip, ] %>%
          .[order(-score, plot_id), plot_id]
      }

      for (i in 1:num_ref) {
        if (i == 1) {
          ref_plot_ids <- plot_id_seq[1]
        } else {
          for (j in (2:pct_data$plot_num)) {
            if (all(abs(plot_id_seq[j] - ref_plot_ids) > 1)) {
              ref_plot_ids <- c(ref_plot_ids, plot_id_seq[j])
              break
            }
          }
        }
      }

      rates_design[strip_id == strip, rate := ifelse(plot_id %in% ref_plot_ids, N_ref, N_reduced)]

      rates_design[plot_id %in% ref_plot_ids, score := score - 1]
    }
  }

  strip_num <- max(exp_data$exp_plots[[1]]$strip_id)
  max_plot_num <- max(exp_data$exp_plots[[1]]$plot_id)

  plot_multiiplier <- ceiling(max_plot_num / pct_data$plot_num)
  strip_multiiplier <- ceiling(strip_num / pct_data$plot_num)

  plot_multiplied <-
    rates_design[rep(1:nrow(rates_design), each = plot_multiiplier), ] %>%
    .[, index := 1] %>%
    .[, numobs := cumsum(index), by = .(strip_id, plot_id)] %>%
    .[, plot_id := plot_id + (numobs - 1) * pct_data$plot_num] %>%
    .[order(strip_id, plot_id), ]

  strip_multiplied <-
    plot_multiplied[rep(1:nrow(plot_multiplied), each = strip_multiiplier), ] %>%
    .[, numobs := cumsum(index), by = .(strip_id, plot_id)] %>%
    .[, strip_id := strip_id + (numobs - 1) * pct_data$plot_num] %>%
    .[order(strip_id, strip_id), ] %>%
    .[, .(strip_id, plot_id, rate)]

  plots_with_rates_assigned <-
    exp_data %>%
    rowwise() %>%
    dplyr::mutate(experiment_design = list(
      left_join(exp_plots, strip_multiplied, by = c("strip_id", "plot_id")) %>%
        dplyr::select(rate, strip_id, plot_id) %>%
        dplyr::mutate(type = "experiment")
    ))

  trial_design <-
    plots_with_rates_assigned %>%
    dplyr::mutate(headland = list(
      dplyr::mutate(headland, rate = N_ref) %>%
        dplyr::select(rate) %>%
        dplyr::mutate(strip_id = NA, plot_id = NA, type = "headland")
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
    dplyr::ungroup() %>%
    dplyr::mutate(unit = N_unit) %>%
    dplyr::mutate(design_type = "niplorp") %>%
    dplyr::mutate(input_type = "N")

  return(trial_design)
}

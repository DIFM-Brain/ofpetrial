#' Check the correlation of the two inputs
#'
#' Check the correlation between the rates of the two inputs for a two-input experiment.
#'
#' @param td trial design with rates assigned created by assign_rates()
#' @returns table
#' @import data.table
#' @export
#' @examples
#' #--- load a trial design for a two-input experiment ---#
#' data(td_two_input)
#' 
#' #--- check correlation ---#
#' check_cor_inputs(td_two_input)
check_cor_inputs <- function(td) {
  if (nrow(td) > 1) {
    td_1 <- td$trial_design[[1]]
    td_2 <- td$trial_design[[2]]

    td_1_area <- mean(sf::st_area(td_1))
    td_2_area <- mean(sf::st_area(td_2))

    suppressWarnings(
      if (td_1_area < td_2_area) {
        cor_input <-
          dplyr::mutate(td_1, rate_2 = stats::aggregate(td_2, td_1, mean)$rate) %>%
          data.table() %>%
          .[, stats::cor(rate, rate_2)]
      } else {
        cor_input <-
          dplyr::mutate(td_2, rate_2 = stats::aggregate(td_1, td_2, mean)$rate) %>%
          data.table() %>%
          .[, stats::cor(rate, rate_2)]
      }
    )
  }

  return(cor_input)
}


#' Check the alignment of harvester and applicator/planter
#'
#' Check the alignment of harvester and applicator/planter for mixed treatment problems where multiple input rates are associated with yield monitor data
#'
#' @param td trial design data created by make_exp_plots() and assign_rates()
#' @returns a tibble
#' @import data.table
#' @export
#' @examples 
#' #--- check the alignment of harvester and applicator/planter ---#
#' machine_alignment <- check_alignment(td)
#' 
#' #--- check the degree of mixed treatment problem ---#
#' machine_alignment$overlap_data
#' 
#' #--- visualize the degree of mixed treatment problem ---#
#' machine_alignment$g_overlap[[2]]
check_alignment <- function(td) {
  checks <-
    td %>%
    dplyr::select(input_name, trial_design, harvester_width, harvest_ab_lines, field_sf) %>%
    tidyr::unnest(harvest_ab_lines) %>%
    dplyr::rename(harvest_ab_line = x) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(harvester_path = list(
      make_harvest_path(harvester_width, harvest_ab_line, field_sf) %>%
        dplyr::mutate(ha_area = as.numeric(st_area(geometry)))
    )) %>%
    dplyr::mutate(overlap_data = list(
      st_intersection_quietly(harvester_path, st_transform_utm(trial_design)) %>%
        .$result %>%
        dplyr::mutate(area = as.numeric(st_area(geometry))) %>%
        data.table() %>%
        .[, .(area = sum(area), ha_area = mean(ha_area)), by = .(ha_strip_id, strip_id)] %>%
        .[!is.na(strip_id), ] %>%
        .[, total_intersecting_ha_area := sum(area), by = ha_strip_id] %>%
        .[, intersecting_pct := total_intersecting_ha_area / ha_area] %>%
        #--- remove strips whose intersecting area is less than 10% of its area ---#
        .[intersecting_pct > 0.1, ] %>%
        .[, .SD[which.max(area), ], by = ha_strip_id] %>%
        .[, dominant_pct := area / total_intersecting_ha_area] %>%
        .[order(ha_strip_id), ]
    )) %>%
    dplyr::select(input_name, harvest_ab_line, overlap_data, harvester_path) %>%
    dplyr::mutate(g_overlap = list(
      ggplot(overlap_data) +
        geom_histogram(aes(x = dominant_pct)) +
        xlim(0, NA) +
        theme_bw() +
        xlab("Percentage of the strip area occupied by a single rate") +
        ylab("The number of strips")
    )) %>%
    dplyr::ungroup()
  return(checks)
}

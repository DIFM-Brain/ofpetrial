#' Diagnose the trial design
#'
#' Check the correlation between the input rate and observed characteristics. It checks the correlation between two inputs for a two-input case.
#'
#' @param trial_design trial design with rates assigned created by assign_rates()
#' @param field_data (list) list of sf
#' @returns table
#' @import data.table
#' @export
diagnose <- function(trial_design, field_data  = NA) {
  if (nrow(trial_design) > 1) {
    td_1 <- trial_design$trial_design[[1]]
    td_2 <- trial_design$trial_design[[2]]

    td_1_area <- mean(sf::st_area(td_1))
    td_2_area <- mean(sf::st_area(td_2))

    if (td_1_area < td_2_area) {
      cor_input <- dplyr::mutate(td_1, rate_2 = aggregate(td_2, td_1, mean)$rate) %>%
        data.table() %>%
        .[, cor(rate, rate_2)]
    } else {
      cor_input <- dplyr::mutate(td_2, rate_2 = aggregate(td_1, td_2, mean)$rate) %>%
        data.table() %>%
        .[, cor(rate, rate_2)]
    }
  }

  return(cor_input)
}

#!===========================================================
#! Helper functions
#!===========================================================
make_harvest_path <- function(harvester_width, harvest_ab_line, field_sf) {
  base_ab_lines_data <-
    prepare_ablines(
      ab_line = harvest_ab_line,
      field = field_sf,
      plot_width = harvester_width
    )

  #--- ab-line tilted by harvester angle ---#
  plot_heading <- base_ab_lines_data$plot_heading
  #--- unit vector pointing in the direction the machine moves ---#
  ab_xy_nml <- base_ab_lines_data$ab_xy_nml
  #--- unit vector pointing in the direction PERPENDICULAR to the direction the machine moves ---#
  ab_xy_nml_p90 <- base_ab_lines_data$ab_xy_nml_p90

  #++++++++++++++++++++++++++++++++++++
  #+ Create strips
  #++++++++++++++++++++++++++++++++++++
  f_bbox <- sf::st_bbox(field)

  #--- maximum distance ---#
  radius <-
    sqrt(
      (f_bbox["xmax"] - f_bbox["xmin"])^2 +
        (f_bbox["ymax"] - f_bbox["ymin"])^2
    ) / 2 + 100

  #--- create strips ---#
  #* only the angle of plot is used from plot_heading
  strips <- create_strips(field, plot_heading, harvester_width, radius)

  # ggplot() +
  #   geom_sf(data = strips, aes(fill = group)) +
  #   geom_sf(data = field, col = "black", fill = NA) +
  #   geom_sf(data = plot_heading, col = "red")

  #++++++++++++++++++++++++++++++++++++
  #+ Shift the polygons
  #++++++++++++++++++++++++++++++++++++
  #--- find the group id for the cells that are intersecting with the ab-line  ---#
  ab_int_group <-
    suppressWarnings(sf::st_intersection(strips, plot_heading)) %>%
    dplyr::pull(group) %>%
    unique()

  #--- get the sf of the intersecting sf ---#
  int_group <- dplyr::filter(strips, group == ab_int_group)

  # ggplot() +
  #   geom_sf(data = int_group, fill = "blue", color = NA) +
  #   geom_sf(data = plot_heading, color = "red", size = 0.3)

  #--- the distance between the ab-line and the line that connect the centroids of the intersecting sf ---#
  correction_dist <-
    sf::st_distance(
      get_through_line(int_group, radius, ab_xy_nml),
      plot_heading
    ) %>%
    as.numeric()

  #--- shift the intersecting sf  ---#
  int_group_corrected <-
    st_shift(
      int_group,
      correction_dist * ab_xy_nml_p90,
      merge = FALSE
    )

  new_dist <-
    sf::st_distance(
      get_through_line(int_group_corrected, radius, ab_xy_nml),
      plot_heading
    ) %>%
    as.numeric()

  # move the intersecting strip so the ab-line goes through the center
  if (new_dist > correction_dist) {
    #--- if moved further away ---#
    strips_shifted <- st_shift(strips, -correction_dist * ab_xy_nml_p90)
  } else {
    #--- if get close ---#
    strips_shifted <- st_shift(strips, correction_dist * ab_xy_nml_p90)
  }

  harvester_path <-
    st_intersection_quietly(strips_shifted, field_sf) %>%
    .$result %>%
    dplyr::mutate(ha_strip_id = 1:dplyr::n()) %>%
    dplyr::select(ha_strip_id)

  return(harvester_path)
}
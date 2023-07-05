
#' Check the alignment of harvester and applicator/planter
#'
#' Check the alignment of harvester and applicator/planter for mixed treatment problems where multiple input rates are associated with yield monitor data
#'
#' @param td
#' @returns a tibble
#' @import data.table
#' @export
check_alignment <- function(td) {
  checks <-
    td %>%
    dplyr::select(form, trial_design, harvester_width, harvest_ab_lines) %>%
    tidyr::unnest(harvest_ab_lines) %>%
    dplyr::rename(harvest_ab_line = x) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(harvester_path = list(
      make_harvest_path(harvester_width, harvest_ab_line, field_sf) %>%
        dplyr::mutate(ha_area = as.numeric(st_area(geometry)))
    )) %>%
    dplyr::mutate(overlap_test = list(
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
    dplyr::select(form, harvest_ab_line, overlap_test, harvester_path) %>%
    dplyr::ungroup()
  return(checks)
}

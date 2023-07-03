#' Diagnose the trial design
#'
#' Check the correlation between the input rate and observed characteristics. It checks the correlation between two inputs for a two-input case.
#'
#' @param trial_design
#' @param field_data
#' @returns
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

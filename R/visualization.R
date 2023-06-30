#' Visualize experiment plots
#'
#' Visualize experiment plots (no rates assigned)
#'
#' @param exp_data experiment plots created by make_exp_plots()
#' @returns ggplot2 figure
#' @import ggplot2
#' @export
viz_exp_plots <- function(exp_data) {
  gg_exp <-
    dplyr::rowwise(exp_data) %>%
    dplyr::mutate(g_fig = list(
      ggplot() +
        geom_sf(data = field_sf, fill = NA) +
        geom_sf(data = exp_plots, fill = NA, color = "blue") +
        theme_void() +
        ggtitle(paste0("Trial plots for ", form))
    )) %>%
    dplyr::select(g_fig)

  if (nrow(gg_exp) > 1) {
    ggpubr::ggarrange(gg_exp$g_fig[[1]], gg_exp$g_fig[[2]], ncol = 2)
  } else {
    gg_exp$g_fig[[1]]
  }
}

#' Visualize trial designs
#'
#' Visualize trial designs plots (rates assigned)
#'
#' @param trial_design trial design: experiment plots with input rates assigned
#' @returns ggplot2 figure
#' @import ggplot2
#' @export
viz_td <- function(trial_design) {
  gg_td <-
    dplyr::rowwise(trial_design) %>%
    mutate(g_fig = list(
      ggplot() +
        geom_sf(data = field_sf, fill = NA) +
        geom_sf(data = trial_design, aes(fill = factor(rate)), color = NA) +
        scale_fill_viridis_d() +
        theme_void() +
        ggtitle(paste0("Trial design for ", form))
    )) %>%
    dplyr::select(g_fig)

  if (nrow(gg_td) > 1) {
    ggpubr::ggarrange(gg_td$g_fig[[1]], gg_td$g_fig[[2]], ncol = 2)
  } else {
    gg_td$g_fig[[1]]
  }
}

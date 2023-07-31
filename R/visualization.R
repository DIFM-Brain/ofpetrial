
#' Visualize various aspects of a trial design
#'
#' Create plots of experiment rates, plot layout, plot_id, strip_id, and block_id, which can be specified by the `type` argument.
#'
#' @param td (tibble) experiment plots made by make_exp_plots()
#' @param type (character) type of plots to create. Available options are "rates", "layout", "plot_id", "strip_id", "block_id", "ab_line"
#' @param input_index (numeric) a vector of length 1 or 2. 1 means the 1st input of the td, 2 means the second input of the td, and c(1, 2) means both of the inputs, which is the DEFAULT
#' @param text_size (numeric) the size of plot ID, strip ID, and block ID numbers printed in the plots
#' @param abline (logical) If TRUE, ab-lines are displayed as well. Default = FALSE. This applies only ton type = "rates" and type = "layout".
#' @returns plot as a ggplot object
#' @import ggplot2
#' @export
#' @examples
#' #--- load trial design ---#
#' data(td_two_input)
#' viz(td_two_input)
#' viz(td_two_input, type = "plot_id")
#'
viz <- function(td, type = "rates", input_index = c(1, 2), text_size = 3, abline = FALSE) {
  #--- select rows ---#
  if (nrow(td) == 1) {
    input_index <- 1
  }

  #--- determine the stack orientation ---#
  field_bbox <-
    td$field_sf[[1]] %>%
    sf::st_bbox()

  x_length <- field_bbox["xmax"] - field_bbox["xmin"]
  y_length <- field_bbox["ymax"] - field_bbox["ymin"]

  if (x_length > y_length) {
    stack_field_orientation <- "vertical"
  } else {
    stack_field_orientation <- "horizontal"
  }

  #--- prepare data to be used across different types ---#
  td_rows <-
    td[input_index, ] %>%
    dplyr::rowwise()

  if (type == "block_id") {
    gg_td <-
      td_rows %>%
      dplyr::mutate(g_fig = list(
        ggplot() +
          geom_sf(data = trial_design, aes(fill = factor(block_id))) +
          geom_sf_text(
            data = trial_design,
            aes(label = block_id),
            size = text_size,
            fun.geometry = sf::st_centroid
          ) +
          scale_fill_discrete(name = "Block ID") +
          theme_void() +
          ggtitle(paste0("Block ID of experiment plots for ", input_name))
      ))
  } else if (type == "strip_id") {
    gg_td <-
      td_rows %>%
      dplyr::mutate(g_fig = list(
        ggplot() +
          geom_sf(data = trial_design, aes(fill = factor(strip_id))) +
          geom_sf_text(
            data = trial_design,
            aes(label = strip_id),
            size = text_size,
            fun.geometry = sf::st_centroid
          ) +
          scale_fill_discrete(name = "Strip ID") +
          theme_void() +
          ggtitle(paste0("Strip ID of experiment plots for ", input_name))
      ))
  } else if (type == "plot_id") {
    gg_td <-
      td_rows %>%
      dplyr::mutate(g_fig = list(
        ggplot() +
          geom_sf(data = trial_design, fill = NA) +
          geom_sf_text(
            data = trial_design,
            aes(label = plot_id),
            size = text_size,
            fun.geometry = sf::st_centroid
          ) +
          theme_void() +
          ggtitle(paste0("Plot ID of experiment plots for ", input_name))
      ))
  } else if (type == "rates") {
    gg_td <-
      td_rows %>%
      dplyr::mutate(g_tr = list(
        ggplot() +
          geom_sf(data = field_sf, fill = NA) +
          geom_sf(data = trial_design, aes(fill = factor(rate)), color = "black") +
          scale_fill_viridis_d(name = paste0(input_name, " (", unit, ")")) +
          theme_void() +
          ggtitle(
            paste0(
              "Trial design for ",
              input_name,
              "\n(",
              dplyr::case_when(
                design_type == "ls" ~ "Latin Square",
                design_type == "strip" ~ "Strip",
                design_type == "rb" ~ "Randomized Block",
                design_type == "jcls" ~ "Jump-conscious Latin Square",
                design_type == "ejca" ~ "Extra Jump-conscious Alternate",
                design_type == "sparse" ~ "Sparse"
              ),
              ")"
            )
          )
      )) %>%
      dplyr::mutate(g_fig = list(
        if (abline == TRUE) {
          g_tr +
            geom_sf(data = ab_lines, aes(color = "applicator/planter ab-line")) +
            geom_sf(data = harvest_ab_lines, aes(color = "harvester ab-line")) +
            scale_color_manual(
              name = "",
              values = c(
                "applicator/planter ab-line" = "red", "harvester ab-line" = "blue"
              )
            )
        } else {
          g_tr
        }
      )) %>%
      dplyr::select(g_fig)
  } else if (type == "layout") {
    gg_td <-
      td_rows %>%
      dplyr::mutate(g_exp = list(
        ggplot() +
          geom_sf(data = field_sf, fill = NA) +
          geom_sf(data = exp_plots, fill = NA, color = "blue") +
          theme_void() +
          ggtitle(paste0("Trial plots for ", input_name))
      )) %>%
      dplyr::mutate(g_fig = list(
        if (abline == TRUE) {
          g_exp +
            geom_sf(data = ab_lines, aes(color = "applicator/planter ab-line")) +
            geom_sf(data = harvest_ab_lines, aes(color = "harvester ab-line")) +
            scale_color_manual(
              name = "",
              values = c(
                "applicator/planter ab-line" = "red", "harvester ab-line" = "blue"
              )
            )
        } else {
          g_exp
        }
      ))
  } else if (type == "ab_line") {

    #--- determine the stack orientation ---#
    line_bbox <-
      td_rows$ab_lines[[1]] %>%
      sf::st_bbox()

    x_length <- line_bbox["xmax"] - line_bbox["xmin"]
    y_length <- line_bbox["ymax"] - line_bbox["ymin"]

    if (x_length > y_length) {
      stack_ab_orientation <- "vertical"
    } else {
      stack_ab_orientation <- "horizontal"
    }

    gg_td <-
      td_rows %>%
      dplyr::mutate(g_ab = list(
        ggplot() +
          geom_sf(data = dplyr::filter(trial_design, strip_id %in% 1:3)) +
          geom_sf(data = ab_lines, color = "red") +
          theme_void() +
          ggtitle(paste0("Applicator/Planter ab-line\n", "(", input_name, ")"))
      )) %>%
      dplyr::mutate(g_h_ab = list(
        ggplot() +
          geom_sf(data = dplyr::filter(trial_design, strip_id %in% 1:3)) +
          geom_sf(data = harvest_ab_lines, color = "blue") +
          theme_void() +
          ggtitle("Harvester ab-line")
      )) %>%
      dplyr::mutate(g_fig = list(
        ggpubr::ggarrange(g_ab, g_h_ab, ncol = ifelse(stack_ab_orientation == "vertical", 1, 2))
      ))
  } else {
    stop("The type you specified is not one of the allowed options.")
  }


  if (nrow(gg_td) > 1) {
    ggpubr::ggarrange(gg_td$g_fig[[1]], gg_td$g_fig[[2]], ncol = 2)
  } else {
    gg_td$g_fig[[1]]
  }
}

#' Visualize experiment plots
#'
#' Visualize the layuot of experiment plots
#'
#' @param td a tibble with experiment plots made by make_exp_plots()
#' @param input_index (numeric) a vector of length 1 or 2. 1 means the 1st input of the td, 2 means the second input of the td, and c(1, 2) means both of the inputs, which is the DEFAULT
#' @param abline (logical) Default = TRUE. If TRUE, ab-lines are displayed along with the experiment plots
#' @returns ggplot figure
#' @import ggplot2
#' @export
viz_layout <- function(td, input_index = c(1, 2), abline = TRUE) {
  if (nrow(td) == 1) {
    input_index <- 1
  }
  gg_exp <-
    td[input_index, ] %>%
    dplyr::rowwise() %>%
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
          geom_sf(data = ab_lines, aes(color = "ab-line")) +
          scale_color_manual(name = "", values = c("ab-line" = "red"))
      } else {
        g_exp
      }
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
#' @param td a tibble with experiment plots made by make_exp_plots()
#' @param input_index (numeric) a vector of length 1 or 2. 1 means the 1st input of the td, 2 means the second input of the td, and c(1, 2) means both of the inputs, which is the DEFAULT
#' @param abline (logical) Default = TRUE. If TRUE, ab-lines are displayed along with the trial designs
#' @returns ggplot figure
#' @import ggplot2
#' @export
viz_design <- function(td, input_index = c(1, 2), abline = TRUE) {
  if (nrow(td) == 1) {
    input_index <- 1
  }
  gg_td <-
    td[input_index, ] %>%
    dplyr::rowwise() %>%
    dplyr::mutate(g_tr = list(
      ggplot() +
        geom_sf(data = field_sf, fill = NA) +
        geom_sf(data = trial_design, aes(fill = factor(rate)), color = NA) +
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
          geom_sf(data = ab_lines, aes(color = "ab-line")) +
          scale_color_manual(name = "", values = c("ab-line" = "red"))
      } else {
        g_tr
      }
    )) %>%
    dplyr::select(g_fig)

  if (nrow(gg_td) > 1) {
    ggpubr::ggarrange(gg_td$g_fig[[1]], gg_td$g_fig[[2]], ncol = 2)
  } else {
    gg_td$g_fig[[1]]
  }
}

#' Visualize plot IDs
#'
#' Visualize plot IDs of experiment plots
#'
#' @param td a tibble with experiment plots made by make_exp_plots()
#' @param input_index (numeric) a vector of length 1 or 2. 1 means the 1st input of the td, 2 means the second input of the td, and c(1, 2) means both of the inputs, which is the DEFAULT
#' @param text_size (numeric) the size of plot id numbers printed in each plot
#' @param abline (logical) Default = FALSE. If TRUE, ab-lines are displayed along with the trial designs
#' @returns ggplot figure
#' @import ggplot2
#' @export
#' @examples
#' data(td_single_input)
#' viz_plot_id(td_single_input, text_size = 2)
#'
viz_plot_id <- function(td, input_index = c(1, 2), text_size = 3, abline = FALSE) {
  if (nrow(td) == 1) {
    input_index <- 1
  }
  gg_td <-
    td[input_index, ] %>%
    dplyr::rowwise() %>%
    dplyr::mutate(g_fig = list(
      ggplot() +
        geom_sf(data = trial_design, fill = NA) +
        geom_sf_text(data = trial_design, aes(label = plot_id), size = text_size) +
        theme_void() +
        ggtitle(paste0("Plot ID of experiment plots for ", input_name))
    ))

  if (nrow(gg_td) > 1) {
    ggpubr::ggarrange(gg_td$g_fig[[1]], gg_td$g_fig[[2]], ncol = 2)
  } else {
    gg_td$g_fig[[1]]
  }
}

#' Visualize strip IDs
#'
#' Visualize strip IDs of experiment plots
#'
#' @param td a tibble with experiment plots made by make_exp_plots()
#' @param input_index (numeric) a vector of length 1 or 2. 1 means the 1st input of the td, 2 means the second input of the td, and c(1, 2) means both of the inputs, which is the DEFAULT
#' @param text_size (numeric) the size of plot id numbers printed in each plot
#' @param abline (logical) Default = FALSE. If TRUE, ab-lines are displayed along with the trial designs
#' @returns ggplot figure
#' @import ggplot2
#' @export
#' @examples
#' data(td_single_input)
#' viz_strip_id(td_single_input, text_size = 2)
#'
viz_strip_id <- function(td, input_index = c(1, 2), text_size = 3, abline = FALSE) {
  if (nrow(td) == 1) {
    input_index <- 1
  }
  gg_td <-
    td[input_index, ] %>%
    dplyr::rowwise() %>%
    dplyr::mutate(g_fig = list(
      ggplot() +
        geom_sf(data = trial_design, aes(fill = factor(strip_id))) +
        geom_sf_text(data = trial_design, aes(label = strip_id), size = text_size) +
        scale_fill_discrete(name = "Strip ID") +
        theme_void() +
        ggtitle(paste0("Strip ID of experiment plots for ", input_name))
    ))

  if (nrow(gg_td) > 1) {
    ggpubr::ggarrange(gg_td$g_fig[[1]], gg_td$g_fig[[2]], ncol = 2)
  } else {
    gg_td$g_fig[[1]]
  }
}

#' Visualize block IDs
#'
#' Visualize block IDs of experiment plots
#'
#' @param td a tibble with experiment plots made by make_exp_plots()
#' @param input_index (numeric) a vector of length 1 or 2. 1 means the 1st input of the td, 2 means the second input of the td, and c(1, 2) means both of the inputs, which is the DEFAULT
#' @param text_size (numeric) the size of plot id numbers printed in each plot
#' @param abline (logical) Default = FALSE. If TRUE, ab-lines are displayed along with the trial designs
#' @returns ggplot figure
#' @import ggplot2
#' @export
#' @examples
#' data(td_single_input)
#' viz_strip_id(td_single_input, text_size = 2)
#'
viz_block_id <- function(td, input_index = c(1, 2), text_size = 3, abline = FALSE) {
  if (nrow(td) == 1) {
    input_index <- 1
  }
  gg_td <-
    td[input_index, ] %>%
    dplyr::rowwise() %>%
    dplyr::mutate(g_fig = list(
      ggplot() +
        geom_sf(data = trial_design, aes(fill = factor(block_id))) +
        geom_sf_text(data = trial_design, aes(label = block_id), size = text_size) +
        scale_fill_discrete(name = "Block ID") +
        theme_void() +
        ggtitle(paste0("Block ID of experiment plots for ", input_name))
    ))

  if (nrow(gg_td) > 1) {
    ggpubr::ggarrange(gg_td$g_fig[[1]], gg_td$g_fig[[2]], ncol = 2)
  } else {
    gg_td$g_fig[[1]]
  }
}



#' Visualize various aspects of a trial design
#'
#' Create plots of experiment rates, plot layout, plot_id, strip_id, and block_id, which can be specified by the `type` argument.
#'
#' @param td (tibble) experiment plots made by make_exp_plots()
#' @param type (character) type of plots to create. Available options are "rates", "layout", "plot_id", "strip_id", "block_id", "ab_line"
#' @param input_index (numeric) a vector of length 1 or 2. 1 means the 1st input of the td, 2 means the second input of the td, and c(1, 2) means both of the inputs, which is the DEFAULT
#' @param text_size (numeric) the size of plot ID, strip ID, and block ID numbers printed in the plots
#' @param abline (logical) If TRUE, ab-lines are displayed as well. Default = FALSE. This applies only ton type = "rates" and type = "layout".
#' @param leaflet (logical) If TRUE, the plot will be superimposed on a satellite imagery of the field. Default is FALSE. This option is effective only for type = "rates".
#' @returns ggplot or leaflet (if leaflet == TRUE) object
#' @import ggplot2
#' @import leaflet
#' @export
#' @examples
#' #--- load trial design ---#
#' data(td_two_input)
#' viz(td_two_input)
#'
viz <- function(td, type = "rates", input_index = c(1, 2), text_size = 3, abline = FALSE, leaflet = FALSE) {
  #++++++++++++++++++++++++++++++++++++
  #+ Debug
  #++++++++++++++++++++++++++++++++++++
  # data(td_single_input)
  # td <- td_single_input

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
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
            fun.geometry = st_centroid_quietly
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
            fun.geometry = st_centroid_quietly
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
            fun.geometry = st_centroid_quietly
          ) +
          theme_void() +
          ggtitle(paste0("Plot ID of experiment plots for ", input_name))
      ))
  } else if (type == "rates") {
    # input_name <- gg_td$input_name[[1]]
    # input_type <- gg_td$input_type[[1]]
    # unit <- gg_td$unit[[1]]
    # unit_system <- gg_td$unit_system[[1]]
    # gc_rate <- gg_td$gc_rate[[1]]
    # tgt_rate_original <- gg_td$tgt_rate_original[[1]]
    # trial_design <- gg_td$trial_design[[1]]

    if (leaflet == TRUE) {
      color_pal_1 <- colorFactor(palette = "Greens", domain = NULL)
      color_pal_2 <- colorFactor(palette = "Oranges", domain = NULL)

      data_for_leaflet <-
        td_rows %>%
        dplyr::mutate(data_for_plot = list(
          data.table::data.table(rate = unique(trial_design$rate)) %>%
            .[, rata_equiv := convert_rates(input_name, unit, rate)] %>%
            .[order(rate), ] %>%
            .[, all_units := factor(paste(rate, " | ", rata_equiv))] %>%
            .[rata_equiv == rate, all_units := factor(rate)] %>%
            dplyr::left_join(trial_design, ., by = "rate")
        )) %>%
        dplyr::mutate(need_equiv_rate = list(
          data.table(data_for_plot)[, all(rate != rata_equiv)]
        )) %>%
        dplyr::mutate(legend_title = list(
          get_legend_title(
            unit_system,
            need_equiv_rate,
            input_name,
            input_type,
            unit
          )
        ))

      legend_titles <- data_for_leaflet$legend_title %>% unlist()
      layer_titles <-
        data_for_leaflet$input_name %>%
        lapply(to_title) %>%
        unlist()

      if (nrow(td_rows) == 2) {
        leaflet_map <-
          leaflet() %>%
          addPolygons(
            data = data_for_leaflet$data_for_plot[[1]],
            fillOpacity = 0.6,
            color = "black",
            weight = 1,
            fillColor = ~ color_pal_1(all_units),
            group = layer_titles[1]
          ) %>%
          addPolygons(
            data = data_for_leaflet$data_for_plot[[2]],
            fillOpacity = 0.6,
            color = "black",
            weight = 1,
            fillColor = ~ color_pal_2(all_units),
            group = layer_titles[2]
          ) %>%
          addProviderTiles(
            providers$Esri.WorldImagery
          ) %>%
          addLegend(
            data = data_for_leaflet$data_for_plot[[1]],
            pal = color_pal_1,
            position = "topleft",
            values = ~all_units,
            title = legend_titles[[1]],
            group = layer_titles[1]
          ) %>%
          addLegend(
            data = data_for_leaflet$data_for_plot[[2]],
            pal = color_pal_2,
            position = "topleft",
            values = ~all_units,
            title = legend_titles[[2]],
            group = layer_titles[2]
          ) %>%
          addLayersControl(
            overlayGroups = c(layer_titles[1], layer_titles[2]),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          hideGroup(layer_titles[2])
      } else {
        leaflet_map <-
          leaflet() %>%
          addPolygons(
            data = data_for_leaflet$data_for_plot[[1]],
            fillOpacity = 0.6,
            color = "black",
            weight = 1,
            fillColor = ~ color_pal_1(all_units),
            group = layer_titles[1]
          ) %>%
          addProviderTiles(
            providers$Esri.WorldImagery
          ) %>%
          addLegend(
            data = data_for_leaflet$data_for_plot[[1]],
            pal = color_pal_1,
            position = "topleft",
            values = ~all_units,
            title = legend_titles[[1]],
            group = layer_titles[1]
          ) %>%
          addLayersControl(
            overlayGroups = c(layer_titles[1]),
            options = layersControlOptions(collapsed = FALSE)
          )
      }
    } else {

      gg_td <-
        td_rows %>%
        dplyr::mutate(data_for_plot = list(
          data.table::data.table(rate = unique(trial_design$rate)) %>%
            .[, rata_equiv := convert_rates(input_name, unit, rate)] %>%
            .[order(rate), ] %>%
            .[, all_units := factor(paste(rate, " | ", rata_equiv))] %>%
            .[rata_equiv == rate, all_units := factor(rate)] %>%
            dplyr::left_join(trial_design, ., by = "rate")
        )) %>%
        dplyr::mutate(need_equiv_rate = list(
          data.table(data_for_plot)[, all(rate != rata_equiv)]
        )) %>%
        dplyr::mutate(legend_title = list(
          get_legend_title(
            unit_system,
            need_equiv_rate,
            input_name,
            input_type,
            unit
          )
        )) %>%
        dplyr::mutate(g_tr = list(
          ggplot() +
            geom_sf(
              data = field_sf,
              fill = NA
            ) +
            geom_sf(
              data = data_for_plot,
              aes(fill = factor(all_units)),
              color = "black"
            ) +
            scale_fill_brewer(name = legend_title, palette = "Greens") +
            theme_void() +
            ggtitle(
              paste0(
                "Trial design",
                " (",
                dplyr::case_when(
                  design_type == "ls" ~ "Latin Square",
                  design_type == "str" ~ "Strip",
                  design_type == "rstr" ~ "Randomized Strip",
                  design_type == "rb" ~ "Randomized Block",
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
    }
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

  if (leaflet == TRUE & type == "rates") {
    leaflet_map
  } else if (nrow(gg_td) > 1) {
    ggpubr::ggarrange(gg_td$g_fig[[1]], gg_td$g_fig[[2]], ncol = 2)
  } else {
    gg_td$g_fig[[1]]
  }
}

# !===========================================================
# ! Helper functions
# !===========================================================

get_legend_title <- function(unit_system, need_equiv_rate, input_name, input_type, unit) {
  `%notin%` <- Negate(`%in%`)

  land_unit <- if (unit_system == "metric") {
    "ha"
  } else {
    "ac"
  }

  converted_unit <- if (unit_system == "metric") {
    "kg"
  } else {
    "lb"
  }

  name <- if (!need_equiv_rate) {
    paste0(to_title(input_name), " (", unit, "/", land_unit, ")")
  } else if (need_equiv_rate) {
    paste0(
      to_title(input_name), " (", unit, "/", land_unit, ") | ",
      input_type, " Equivalent (", converted_unit, "/", land_unit, ")"
    )
  }

  return(name)
}

# td <- trial_design$trial_design[[1]]
# trial_design <- trial_design[[1]]
# input_name <- trial_design$input_name[1]
# unit <- trial_design$unit[1]
# get_plot_data <- function(td, input_name, unit, base_rate = NULL) {
#   plot_data <-
#     td %>%
#     dplyr::mutate(
#       tgt_rate_equiv = ifelse(
#         input_name != "seed",
#         convert_rates(input_name, unit, rate),
#         rate
#       )
#     ) %>%
#     dplyr::mutate(tgt_rate_original = rate) %>%
#     dplyr::mutate(base_rate_original = ifelse(!is.null(base_rate), base_rate$rate, 0)) %>%
#     dplyr::mutate(base_rate_equiv = ifelse(!is.null(base_rate), convert_rates(base_rate$input_name, base_rate$unit, base_rate_original), 0)) %>%
#     dplyr::mutate(total_equiv = tgt_rate_equiv + base_rate_equiv)
# }

get_palette_green <- function(num_rates) {
  return(my_palettes_green[n_rates == num_rates, my_palette][[1]])
}

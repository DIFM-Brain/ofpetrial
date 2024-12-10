#' Check the correlation of the two inputs
#'
#' Check the correlation between the rates of the two inputs for a two-input experiment.
#'
#' @param td trial design for a two-input experiment with rates assigned
#' @returns table
#' @import data.table
#' @export
#' @examples
#' #--- load a trial design for a two-input experiment ---#
#' data(td_two_input)
#'
#' #--- check correlation ---#
#' check_ortho_inputs(td_two_input)
check_ortho_inputs <- function(td) {
  #* +++++++++++++++++++++++++++++++++++
  #* Debug
  #* +++++++++++++++++++++++++++++++++++
  # data(td_two_input)
  # td <- td_two_input

  #* +++++++++++++++++++++++++++++++++++
  #* Main
  #* +++++++++++++++++++++++++++++++++++
  if (nrow(td) > 1) {
    message("Checking the correlation between the two inputs. This may take some time depending on the number of experiment plots.")
    td_1 <- td$trial_design[[1]] %>% dplyr::filter(type == "experiment")
    td_2 <- td$trial_design[[2]] %>% dplyr::filter(type == "experiment")

    suppressWarnings(
      interesected_data <-
        sf::st_intersection(td_1, td_2) %>%
        sf::st_make_valid() %>%
        dplyr::mutate(area = st_area(geometry) %>% as.numeric()) %>%
        dplyr::select(rate, rate.1, area) %>%
        sf::st_drop_geometry()
    )

    cor_input <-
      stats::cov.wt(
        interesected_data[, c("rate", "rate.1")],
        wt = interesected_data[, "area"],
        cor = TRUE
      ) %>%
      .$cor %>%
      .[1, 2]
  } else {
    message("This is not a two-input experiment. You do not have to worry about high correlation between the two inputs here.")
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
#' #--- load trial design ---#
#' data(td_single_input)
#'
#' #--- check the alignment of harvester and applicator/planter ---#
#' machine_alignment <- check_alignment(td_single_input)
#'
#' #--- check the degree of mixed treatment problem ---#
#' machine_alignment$overlap_data
#'
#' #--- visualize the degree of mixed treatment problem ---#
#' machine_alignment$g_overlap[[1]]
check_alignment <- function(td) {
  #++++++++++++++++++++++++++++++++++++
  #+ Debug helper
  #++++++++++++++++++++++++++++++++++++
  # data(td_single_input)
  # td <- td_single_input
  # input_name <-  td$input_name[[1]]
  # exp_plots <-  td$exp_plots[[1]]
  # harvester_width <-  td$harvester_width[[1]]
  # harvest_ab_lines <-  td$harvest_ab_lines[[1]]
  # field_sf <-  td$field_sf[[1]]
  # harvester_path <- checks$harvester_path[[1]]

  #++++++++++++++++++++++++++++++++++++
  #+ Main
  #++++++++++++++++++++++++++++++++++++
  checks <-
    td %>%
    dplyr::select(input_name, exp_plots, harvester_width, harvest_ab_lines, field_sf) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(exp_plots = list(make_sf_utm(exp_plots))) %>%
    dplyr::mutate(harvest_ab_lines = list(make_sf_utm(harvest_ab_lines))) %>%
    dplyr::mutate(field_sf = list(make_sf_utm(field_sf))) %>%
    tidyr::unnest(harvest_ab_lines) %>%
    dplyr::rename(harvest_ab_line = x) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(harvester_path = list(
      make_harvest_path(harvester_width, harvest_ab_line, field_sf) %>%
        dplyr::mutate(ha_area = as.numeric(st_area(geometry)))
    )) %>%
    dplyr::mutate(g_path_alignment = list(
      ggplot() +
        geom_sf(
          data = harvester_path,
          aes(color = "Harvester"),
          fill = NA,
          alpha = 0.3
        ) +
        geom_sf(
          data = exp_plots,
          aes(color = "Applicator/Planter"),
          fill = NA
        ) +
        scale_color_manual(
          name = "",
          values = c("Harvester" = "red", "Applicator/Planter" = "blue")
        ) +
        theme_void()
    )) %>%
    dplyr::mutate(overlap_data = list(
      st_intersection_quietly(harvester_path, st_transform_utm(exp_plots)) %>%
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
    dplyr::select(input_name, harvest_ab_line, overlap_data, harvester_path, g_path_alignment) %>%
    dplyr::mutate(g_overlap = list(
      ggplot(overlap_data) +
        geom_histogram(aes(x = dominant_pct * 100)) +
        xlim(0, NA) +
        theme_bw() +
        xlab("Percentage of the strip area occupied by a single rate") +
        ylab("The number of strips")
    )) %>%
    dplyr::ungroup()
  return(checks)
}

#' Check the orthogonality with field/topographic characteristics
#'
#' Check the orthogonality of the trial input rates and observed characteristics provided by the user
#'
#' @param td (tibble) trial design data created by make_exp_plots() and assign_rates()
#' @param sp_data_list (list) list of spatial datasets as `sf` from the `sf` package or `SpatRaster` from the `terra` package
#' @param vars_list (list) list of character vectors indicating the name of the variables to be used in the datasets specified in sp_data_list
#' @returns a list
#' @import data.table
#' @import ggplot2
#' @export
#' @examples
#' data(td_single_input)
#' yield_sf <- sf::st_read(system.file("extdata", "yield-simple1.shp", package = "ofpetrial"))
#' ssurgo_sf <-
#'   sf::st_read(system.file("extdata", "ssurgo-simple1.shp", package = "ofpetrial")) %>%
#'   dplyr::mutate(mukey = factor(mukey))
#' topo_rast <-
#'   c(
#'     terra::rast(system.file("extdata", "slope.tif", package = "ofpetrial")),
#'     terra::rast(system.file("extdata", "twi.tif", package = "ofpetrial"))
#'   )
#'
#' checks <-
#'   check_ortho_with_chars(
#'     td = td_single_input,
#'     sp_data_list = list(yield_sf, ssurgo_sf, topo_rast),
#'     vars_list = list("Yld_Vol_Dr", c("mukey", "clay"), names(topo_rast))
#'   )
#'
#' checks$summary_data[[1]]
#'
#' checks$summary_fig[[1]]
check_ortho_with_chars <- function(td, sp_data_list, vars_list) {
  char_data <-
    tibble::tibble(
      variable = vars_list,
      spatial_data = sp_data_list
    )

  diagnostics <-
    expand_grid_df(char_data, dplyr::select(td, input_name, trial_design)) %>%
    dplyr::mutate(
      checks =
        purrr::pmap(
          list(trial_design, spatial_data, variable),
          summarize_chars
        )
    ) %>%
    tidyr::unnest(checks) %>%
    dplyr::select(var, input_name, summary_data, summary_fig)

  return(diagnostics)
}


# !===========================================================
# ! Helper functions
# !===========================================================
# trial_design <- td_single_input$trial_design[[1]]
# char_vars <- char_data$char_var[[3]]
# topo_rast <-
#   c(
#     terra::rast("inst/extdata/slope.tif"),
#     terra::rast("inst/extdata/twi.tif")
#   )
#++++++++++++++++++++++++++++++++++++
#+ summarize one set of characteristic variables and spatial data
#++++++++++++++++++++++++++++++++++++
# trial_design <- diagnostics$trial_design[[1]]
# spatial_data <- diagnostics$spatial_data[[1]]
# char_vars <- diagnostics$variable[[1]]

summarize_chars <- function(trial_design, spatial_data, char_vars) {
  rate_design <- dplyr::select(trial_design, rate)

  sp_data_class <- class(spatial_data)

  if ("sf" %in% sp_data_class) {
    char_sf <- dplyr::select(spatial_data, one_of(char_vars))
    #---------------------
    # sf
    #---------------------
    dominant_geom_type <-
      data.table(geom = sf::st_geometry_type(spatial_data)) %>%
      .[, .(count = .N), by = geom] %>%
      .[order(count), ] %>%
      .[1, geom]

    if (dominant_geom_type == "POINT") {
      joined_data <- sf::st_join(trial_design, char_sf)
    } else if (dominant_geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
      suppressWarnings(
        joined_data <-
          sf::st_intersection(rate_design, char_sf)
      )
    }
  } else if (sp_data_class %in% "SpatRaster") {
    #---------------------
    # Raster
    #---------------------
    joined_data <-
      terra::extract(spatial_data, rate_design, fun = mean, na.rm = TRUE) %>%
      data.table() %>%
      .[, ..char_vars] %>%
      cbind(rate_design, .)
  } else {
    "The object you provided as a character data is not compatible with this function."
  }

  #--- summarize ---#
  return_data <-
    purrr::map(char_vars, \(x) summarize_indiv_char(joined_data, var = x)) %>%
    dplyr::bind_rows()

  return(return_data)
}

#++++++++++++++++++++++++++++++++++++
#+ summarize single set of variable and spatial data
#++++++++++++++++++++++++++++++++++++
# ssurgo_sf <- st_read("inst/extdata/ssurgo-simple1.shp")
# spatial_data <- ssurgo_sf
# char_vars <- c("mukey", "clay")
# var <- "mukey"
# var <- "clay"
# var <- "Yld_Vol_Dr"

#- Note: Summarize the relationship between input rate and a characteristic. If the characteristics variable is numeric, it find its correlation with the input rate. If categorical, it finds the mean and sd of rate by category.

summarize_indiv_char <- function(joined_data, var) {
  var_class <- class(joined_data[[var]])
  var_with_rate <- c("rate", var)

  if (var_class %in% c("numeric", "integer")) {
    cor_temp <-
      data.table(joined_data)[, ..var_with_rate] %>%
      stats::cor(use = "complete.obs")

    sum_data <- data.frame(var = var, cor_with_rate = cor_temp[1, 2])

    setnames(joined_data, var, "var")

    g_fig <-
      (
        ggplot(joined_data, aes(y = rate, x = var)) +
          geom_point() +
          ylab("Input Rate") +
          xlab(var) +
          theme_bw()
      ) %>%
      ggExtra::ggMarginal(type = "histogram")
  } else if (var_class %in% c("character", "factor")) {
    sum_data <-
      data.table(joined_data)[, ..var_with_rate] %>%
      .[, .(rate_mean = mean(rate), rate_sd = stats::sd(rate)), by = var] %>%
      as.data.frame()

    setnames(joined_data, var, "var")

    g_cor <-
      ggplot(joined_data) +
      geom_boxplot(aes(y = rate, x = var)) +
      ylab("Input Rate") +
      xlab(var) +
      theme_bw()

    g_hist_var <-
      ggplot(joined_data) +
      geom_bar(aes(x = var)) +
      ylab("Count") +
      theme_bw()

    g_fig <- ggpubr::ggarrange(g_cor, g_hist_var, ncol = 1)
  }

  return_table <-
    tibble::tibble(
      var = var,
      summary_data = list(sum_data),
      summary_fig = list(g_fig)
    )
  return(return_table)
}

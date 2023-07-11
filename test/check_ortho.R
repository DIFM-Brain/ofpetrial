#' Check the orthogonality
#'
#' Check the orthogonality of the trial input rates and observed characteristics provided by the user
#'
#' @param td (tibble) trial design data created by make_exp_plots() and assign_rates()
#' @param char_data (tibble) a tibble of
#' @returns a list
#' @import data.table
#' @export
check_orthogonality <- function(td) {

}

yield_sf <- st_read("inst/extdata/yield-simple1.shp")
ssurgo_sf <- st_read("inst/extdata/ssurgo-simple1.shp")

char_data <-
  tibble::tibble(
    vars = list("Yld_Vol_Dr", c("mukey", "clay")),
    data = list(yield_sf, ssurgo_sf)
  )

char_data %>%
  purrr::pmap(list(vars, data), )

td_temp <- td$trial_design[[1]]

joined_data <- 
  st_join(td_temp, dplyr::select(yield_sf, Yld_Vol_Dr)) %>%
  data.table()

select(yield_sf, one_of(vars))

var_name <- "Yld_Vol_Dr"
spatial_data <- yield_sf
spatial_data <- ssurgo_sf
char_vars <- c("Yld_Vol_Dr")
char_vars <- c("mukey", "clay")

trial_design <- td$trial_design[[1]]
summarize_char(joined_data, "mukey")

summarize_chars <- function(trial_design, spatial_data, char_vars) {

  rate_design <- dplyr::select(trial_design, rate)
  char_sf <- dplyr::select(spatial_data, one_of(char_vars))

  sp_data_class <- class(spatial_data)

  if ("sf" %in% sp_data_class) {
    #*+++++++++++++++++++++++++++++++++++
    #* sf
    #*+++++++++++++++++++++++++++++++++++
    dominant_geom_type <-
      data.table(geom = sf::st_geometry_type(spatial_data)) %>%
      .[, .(count = .N), by = geom] %>%
      .[order(count), ] %>%
      .[1, geom]

    all_vars <- c("rate", char_vars)

    if ("POINT" == dominant_geom_type) {
    joined_data <-
      st_join(trial_design, dplyr::select(spatial_data, one_of(char_vars))) %>%
      data.table()

    } else if (dominant_geom_type %in% c("POLYGON", "MULTIPOLYGON")) {

    joined_data <- 
      sf::st_intersection(rate_design, char_sf)
    }

    #--- summarize ---#
    return_data <- 
      lapply(
        char_vars, 
        function(x) summarize_indiv_char(joined_data, x)
      ) %>%
      bind_rows()

  } else if (sp_data_class %in% c("raster", "SpatRaster")) {
  #*+++++++++++++++++++++++++++++++++++
  #* Raster
  #*+++++++++++++++++++++++++++++++++++

  } else {
    "The object you provided as a character data is not compatible with this function."
  }
}
#*===========================================================
#* Helper functions
#*===========================================================
# ssurgo_sf <- st_read("inst/extdata/ssurgo-simple1.shp")
# spatial_data <- ssurgo_sf
# char_vars <- c("mukey", "clay")
# var <- "mukey"
# var <- "clay"

summarize_indiv_char <- function(joined_data, var) {
  var_class <- class(joined_data[[var]])
  var_with_rate <- c("rate", var)

  if (var_class == "numeric") {
    cor_temp <-
      data.table(joined_data)[, ..var_with_rate] %>%
      cor(use = "complete.obs")

    sum_data <- data.table(var = var, cor_with_rate = cor_temp[1, 2])

    g_fig <-
      # ggplot(joined_data, aes_string(y = "rate", x = var)) +
      (
      ggplot(joined_data, aes_string(y = "rate", x = var)) +
        geom_point() +
        ylab("Input Rate") +
        theme_bw()
      ) %>%
      ggExtra::ggMarginal(type = "histogram")
    
    return_table <- 
      tibble::tibble(
        var = var,
        summary_data = list(sum_data),
        g_fig = list(g_fig)
      )

  } else if (var_class %in% c("character", "factor")) {

    sum_data <- 
      data.table(joined_data)[, ..var_with_rate] %>%
      .[, .(rate_mean = mean(rate), rate_sd = sd(rate)), by = var]

    g_cor <-
      ggplot(joined_data) +
        geom_boxplot(aes_string(y = "rate", x = var)) +
        ylab("Input Rate") +
        theme_bw()

    g_hist_var <- 
      ggplot(joined_data) +
        geom_bar(aes_string(x = var)) +
        ylab("Count") +
        theme_bw()

    g_fig <- ggpubr::ggarrange(g_cor, g_hist_var, ncol = 1)

    return_table <- 
      tibble::tibble(
        var = var,
        summary_data = list(sum_data),
        g_fig = list(g_fig)
      )
  }
  return(return_table)
}


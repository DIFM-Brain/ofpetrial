#!===========================================================
# ! Create datasets
# !===========================================================

#* First, overwrite make_sf_utm() for only when creating datasets. This version replace degree (circle) to an ASCII character in the wkt of sf objects in UTM. This cannot be part of the R package as this would create a warning in R CMD check.

make_sf_utm <- function(data_sf) {
  return_sf <- data_sf %>%
    # st_set_4326() %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326) %>%
    st_transform_utm()

  #--- important ---#
  #* This avoids R CMD check warning of the use of non-ASCII character observed in the wkt of sf in UTM
  st_crs(return_sf)$wkt <- gsub("°|º", "", st_crs(return_sf)$wkt)

  return(return_sf)
}

source("R/make_exp_plots.R")

#++++++++++++++++++++++++++++++++++++
#+ plot info
#++++++++++++++++++++++++++++++++++++
plot_info <-
  prep_plot(
    input_name = "NH3",
    unit_system = "imperial",
    machine_width = 30,
    section_num = 1,
    harvester_width = 20,
    headland_length = 30,
    side_length = 60
  )

usethis::use_data(plot_info, overwrite = TRUE)

#++++++++++++++++++++++++++++++++++++
#+ experiment plots
#++++++++++++++++++++++++++++++++++++
exp_data <-
  make_exp_plots(
    input_plot_info = plot_info,
    boundary_data = system.file("extdata", "boundary-simple1.shp", package = "ofpetrial"),
    abline_data = system.file("extdata", "ab-line-simple1.shp", package = "ofpetrial"),
    abline_type = "free"
  )

usethis::use_data(exp_data, overwrite = TRUE)

# exp_data$field_sf %>% st_crs() %>% .$wkt %>% grepl("[^ -~]", .)

#++++++++++++++++++++++++++++++++++++
#+ rate information
#++++++++++++++++++++++++++++++++++++
base_info <-
  add_base_rate(
    "uan28",
    "gallons",
    15
  )

rate_info <-
  prep_rate(
    plot_info = plot_info,
    gc_rate = 30,
    unit = "gallons",
    rates = c(10, 20, 30, 40, 50),
    design_type = "ls",
    rank_seq_ws = c(1, 2, 3, 4, 5),
    rank_seq_as = c(1, 2, 3, 4, 5),
    base_rate = base_info
  )

usethis::use_data(rate_info, overwrite = TRUE)

#++++++++++++++++++++++++++++++++++++
#+ trial design (single-input)
#++++++++++++++++++++++++++++++++++++
td_single_input <-
  assign_rates(
    exp_data = exp_data,
    rate_info = rate_info
  )

# colnames(trial_design)
usethis::use_data(td_single_input, overwrite = TRUE)

#--- check the size (1.5 Mb) ---#
object.size(td_single_input)

#++++++++++++++++++++++++++++++++++++
#+ trial design (two-input)
#++++++++++++++++++++++++++++++++++++
seed_plot_info <-
  prep_plot(
    input_name = "seed",
    unit_system = "imperial",
    machine_width = 60,
    section_num = 24,
    harvester_width = 30,
    plot_width = 30
  )

n_plot_info <-
  prep_plot(
    input_name = "NH3",
    unit_system = "imperial",
    machine_width = 45,
    section_num = 1,
    harvester_width = 30,
    plot_width = 45
  )

input_plot_info <- list(seed_plot_info, n_plot_info)

exp_data <-
  make_exp_plots(
    input_plot_info = input_plot_info,
    boundary_data = system.file("extdata", "boundary-simple1.shp", package = "ofpetrial"),
    abline_data = system.file("extdata", "ab-line-simple1.shp", package = "ofpetrial"),
    abline_type = "free"
  )

exp_data$exp_plots

seed_rate_info <-
  prep_rate(
    plot_info = seed_plot_info,
    gc_rate = 32000,
    unit = "seed",
    min_rate = 16000,
    max_rate = 40000,
    num_rates = 5,
    design_type = "ls"
  )

n_rate_info <-
  prep_rate(
    plot_info = n_plot_info,
    gc_rate = 180,
    unit = "lb",
    rates = c(100, 140, 180, 220, 260),
    design_type = "ls",
    rank_seq_ws = c(5, 4, 3, 2, 1)
  )

td_two_input <- assign_rates(exp_data, rate_info = list(seed_rate_info, n_rate_info))

#--- check the size (1.5 Mb) ---#
object.size(td_two_input)

usethis::use_data(td_two_input, overwrite = TRUE)

# !===========================================================
# ! Trial design for a curved field
# !===========================================================
n_plot_info <-
  prep_plot(
    input_name = "NH3",
    unit_system = "imperial",
    machine_width = 30,
    section_num = 1,
    harvester_width = 20,
    headland_length = 30,
    side_length = 60
  )

exp_data <-
  make_exp_plots(
    input_plot_info = n_plot_info,
    boundary_data = system.file("extdata", "boundary-irregular1.shp", package = "ofpetrial"),
    abline_data = system.file("extdata", "ab-line-irregular1.shp", package = "ofpetrial"),
    abline_type = "free"
  )

n_rate_info <-
  prep_rate(
    plot_info = n_plot_info,
    gc_rate = 180,
    unit = "lb",
    rates = c(100, 140, 180, 220, 260),
    design_type = "ls",
  )

td_curved <-
  assign_rates(
    exp_data = exp_data,
    rate_info = n_rate_info
  )

usethis::use_data(td_curved, overwrite = TRUE)

# !===========================================================
# ! Internal Data
# !===========================================================
#++++++++++++++++++++++++++++++++++++
#+ Number to english
#++++++++++++++++++++++++++++++++++++
number_english_dictionary <-
  data.table(number = 1:1000) %>%
  .[, num_in_english := english::english(number)]

#++++++++++++++++++++++++++++++++++++
#+ Color sequence
#++++++++++++++++++++++++++++++++++++
my_palettes_grey <-
  data.table(
    n_rates = 1:6
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(my_palette = list(
    RColorBrewer::brewer.pal(n = n_rates + 3, "Greys")[1:n_rates]
  )) %>%
  data.table()

my_palettes_green <-
  data.table(
    n_rates = 3:8
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(my_palette = list(
    RColorBrewer::brewer.pal(n = n_rates, "Greens")
  )) %>%
  data.table()

#++++++++++++++++++++++++++++++++++++
#+ Input type table
#++++++++++++++++++++++++++++++++++++
input_type_table <-
  jsonlite::fromJSON(
    here::here("data-raw/input_type_table.json"),
    flatten = TRUE
  ) %>%
  data.frame()

#++++++++++++++++++++++++++++++++++++
#+ Input unit conversion table
#++++++++++++++++++++++++++++++++++++
input_unit_conversion_table <-
  jsonlite::fromJSON(
    here::here("data-raw/input_unit_conversion_table.json"),
    flatten = TRUE
  ) %>%
  data.table() %>%
  .[, conv_factor := as.numeric(conv_factor)] %>%
  .[, form_unit := paste(type, unit, sep = "_")] %>%
  as.data.frame()

#++++++++++++++++++++++++++++++++++++
#+ General unit conversion table
#++++++++++++++++++++++++++++++++++++
gen_unit_conversion_table <- data.table(
  from = c("hectares", "acres", "meters", "feet", "kg", "pounds", "acres", "m2"),
  to = c("acres", "hectares", "feet", "meters", "pounds", "kg", "m2", "acres"),
  conv_factor = c(1/0.40468564224, 0.40468564224, 1/0.3048, 0.3048, 1/0.45359237, 0.45359237, 4046.856422, 1/4046.856422)
)

#++++++++++++++++++++++++++++++++++++
#+ Save all as internal data
#++++++++++++++++++++++++++++++++++++
usethis::use_data(
  number_english_dictionary,
  gen_unit_conversion_table,
  input_unit_conversion_table,
  input_type_table,
  my_palettes_grey,
  my_palettes_green,
  internal = TRUE,
  overwrite = TRUE
)

# !===========================================================
# ! External Data
# !===========================================================
#++++++++++++++++++++++++++++++++++++
#+ Harvest data
#++++++++++++++++++++++++++++++++++++
# Make the yield data small to make the package light
yield_sf_small <-
  #--- read the raw yield data file ---#
  st_read(here::here("data-raw/yield-simple1.shp")) %>%
  dplyr::select(Yld_Vol_Dr) %>%
  #--- get every 10 ---#
  dplyr::slice(seq(1, dplyr::n(), by = 5))

st_write(yield_sf_small, here::here("inst/extdata/yield-simple1.shp"))

#++++++++++++++++++++++++++++++++++++
#+ As-applied data
#++++++++++++++++++++++++++++++++++++
# make the as-applied data small to make the package light
as_applied_sf_small <-
  #--- read the raw yield data file ---#
  st_read(here::here("data-raw/as-applied-simple1.shp")) %>%
  dplyr::select(Tgt_Rate_k, Elevation_, Speed_mph_) %>%
  #--- get every 10 ---#
  dplyr::slice(seq(1, dplyr::n(), by = 5))

st_write(as_applied_sf_small, here::here("inst/extdata/as-applied-simple1.shp"))

#++++++++++++++++++++++++++++++++++++
#+ Field boundary and ab-line for a field with holes
#++++++++++++++++++++++++++++++++++++
field_with_holes <-
  st_read(here::here("data-raw/FarmC_FieldZ boundary.shp")) %>%
  dplyr::select(geometry)

st_write(field_with_holes, here::here("inst/extdata/field_boundary_with_holes.shp"))

ab_line <-
  st_read(here::here("data-raw/FarmC_FieldZ ab-line.shp")) %>%
  dplyr::select(geometry)

st_write(ab_line, here::here("inst/extdata/ab_line_for_field_with_holes.shp"))

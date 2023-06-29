######################################
# R codes to assist Trial Design Coordinators
######################################

# !===========================================================
# ! Preparation
# # !===========================================================
# #--- load all the packages ---#
library(here)
# library(rmarkdown)
library(sf)
library(data.table)
# library(bookdown)
# library(knitr)
# library(tmap)
# library(stringr)
# library(jsonlite)
# library(measurements)
# library(tidyverse)
# library(smoother)
library(dplyr)
library(lwgeom)
library(ggplot2)
library(stringr)

# /*----------------------------------*/
#' ## Load the field parameter data
# /*----------------------------------*/
field_data <-
  jsonlite::fromJSON(
    here("data/field_parameter.json"),
    flatten = TRUE
  ) %>%
  data.table() %>%
  .[, field_year := paste(farm, field, year, sep = "_")]

field_year_ls <- field_data$field_year

source("R/utility.R")
source("R/utility_spatial.R")
source("R/make_exp_plots.R")
source("R/assign_rates.R")

# /*=================================================*/
#' # Make trial designs
# /*=================================================*/

ffy <- field_year_ls[1]

#!===========================================================
#! Create experiment plots
#!===========================================================
seed_plot_info <-
  make_input_plot_data(
    form = "seed",
    unit = "seeds",
    plot_width = 40,
    machine_width = 60,
    section_num = 24
  )

n_plot_info <-
  make_input_plot_data(
    form = "NH3",
    unit = "lbs",
    plot_width = 30,
    machine_width = 30,
    section_num = 1
  )

input_plot_info <- list(seed_plot_info, n_plot_info)

ggplot(data_exp$exp_plots[[1]]) +
  geom_sf()

exp_data <-
  make_exp_plots(
    input_plot_info = input_plot_info,
    boundary_file = here("data/boundary-simple1.shp"),
    abline_file = here("data/ab-line-simple1.shp"),
    harvester_width = 30,
    abline_type = "free",
    headland_length = 30,
    side_length = 60,
    min_plot_length = 200,
    max_plot_length = 300,
    length_unit = "feet",
    perpendicular = FALSE,
    file_name_append = "test"
  )

#!===========================================================
#! Assign rates
#!===========================================================
seed_rate_info <-
  make_input_rate_data(
    seed_plot_info,
    min_rate = 16000,
    max_rate = 40000,
    gc_rate = 32000,
    num_rates = 5,
    design_type = "jcl"
  )

n_rate_info <-
  make_input_rate_data(
    n_plot_info,
    gc_rate = 180,
    rates = c(100, 140, 180, 220, 260),
    design_type = "jcl",
  )

rate_info <- list(seed_rate_info, n_rate_info)


td <- assign_rates(exp_data, rate_info)

#*===========================================================
#* Write out all the files for 
#*===========================================================

#*===========================================================
#* Visualization #*===========================================================


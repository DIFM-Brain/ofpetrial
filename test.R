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

source("R/utility.R")
source("R/utility_spatial.R")
source("R/make_exp_plots.R")
source("R/assign_rates.R")

# /*=================================================*/
#' # Make trial designs
# /*=================================================*/


#!===========================================================
#! Create experiment plots
#!===========================================================
seed_plot_info <-
  make_input_plot_data(
    form = "seed",
    plot_width = 30,
    machine_width = 60,
    section_num = 24,
    length_unit = "feet"
  )

n_plot_info <-
  make_input_plot_data(
    form = "NH3",
    plot_width = measurements::conv_unit(60, "ft", "m"),
    machine_width = measurements::conv_unit(60, "ft", "m"),
    section_num = 1
  )

input_plot_info <- list(seed_plot_info, n_plot_info)


exp_data <-
  make_exp_plots(
    input_plot_info = input_plot_info,
    boundary_file = "inst/extdata/boundary-simple1.shp",
    abline_file = "inst/extdata/ab-line-simple1.shp",
    harvester_width = 30,
    abline_type = "free",
    headland_length = 30,
    side_length = 60,
    min_plot_length = 200,
    max_plot_length = 300,
    length_unit = "feet",
    perpendicular = FALSE
  )

viz_exp_plots(exp_data)

exp_data$harvest_ab_lines
exp_data$ab_lines
exp_data$exp_plots

#!===========================================================
#! Assign rates
#!===========================================================
seed_rate_info <-
  prep_rates_s(
    seed_plot_info,
    gc_rate = 32000,
    unit = "seed",
    min_rate = 16000,
    max_rate = 40000,
    num_rates = 5,
    design_type = "jcl"
  )

n_rate_info <-
  prep_rates_s(
    plot_info = n_plot_info,
    gc_rate = 180,
    unit = "lb",
    rates = c(100, 140, 180, 220, 260),
    design_type = "jcl",
  )

rate_info <- list(seed_rate_info, n_rate_info)

td <- assign_rates(exp_data, rate_info)

ls(td)

viz_td(td)

trial_design <- td

#!===========================================================
#! test of abline
#!===========================================================

n_plot_info <-
  make_input_plot_data(
    form = "NH3",
    plot_width = measurements::conv_unit(60, "ft", "m"),
    machine_width = measurements::conv_unit(30, "ft", "m"),
    section_num = 1
  )

exp_data <-
  make_exp_plots(
    input_plot_info = n_plot_info,
    boundary_file = system.file("extdata", "boundary-simple1.shp", package = "ofpetrial"),
    abline_file = system.file("extdata", "ab-line-simple1.shp", package = "ofpetrial"),
    harvester_width = 30,
    abline_type = "free",
    headland_length = 30,
    side_length = 60,
    min_plot_length = 240,
    max_plot_length = 260,
    length_unit = "feet",
    perpendicular = FALSE
  )

viz_ep(exp_data)

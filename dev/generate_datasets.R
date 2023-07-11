#!===========================================================
#! Create datasets
#!===========================================================

#++++++++++++++++++++++++++++++++++++
#+ plot info
#++++++++++++++++++++++++++++++++++++
plot_info <-
  prep_plot_fs(
    input_name = "NH3",
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

#++++++++++++++++++++++++++++++++++++
#+ rate information
#++++++++++++++++++++++++++++++++++++
rate_info <-
  prep_rates_s(
    plot_info = plot_info,
    gc_rate = 180,
    unit = "lb",
    rates = c(100, 140, 180, 220, 260),
    design_type = "ls",
    rank_seq_ws = c(1, 2, 3, 4, 5),
    rank_seq_as = c(1, 2, 3, 4, 5)
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
  prep_plot_fs(
    input_name = "seed",
    machine_width = 60,
    section_num = 24,
    harvester_width = 30,
    plot_width = 30
  )

n_plot_info <-
  prep_plot_fs(
    input_name = "NH3",
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
  prep_rates_s(
    plot_info = seed_plot_info,
    gc_rate = 32000,
    unit = "seed",
    min_rate = 16000,
    max_rate = 40000,
    num_rates = 5,
    design_type = "jcls"
  )

n_rate_info <-
  prep_rates_s(
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

# error 1: plots extend outside of boundary when the sidelands are too narrow
seed_plot_info <-
  prep_plot(
    input_name = "seed",
    unit_system = "imperial",
    machine_width = 60,
    section_num = 24,
    harvester_width = 30,
    plot_width = 30,
    headland_length = 30,
    side_length = 60
  )

# error 2: headland and sideland choices are not handled independently
# when sidelands are larger than headlands, all become the size of the sidelands
seed_plot_info <-
  prep_plot(
    input_name = "seed",
    unit_system = "imperial",
    machine_width = 60,
    section_num = 24,
    harvester_width = 30,
    plot_width = 30,
    headland_length = 30,
    side_length = 5
  )

input_plot_info <- list(seed_plot_info)

exp_data <-
  make_exp_plots(
    input_plot_info = input_plot_info,
    boundary_data = system.file("extdata", "boundary-simple1.shp", package = "ofpetrial"),
    abline_data = system.file("extdata", "ab-line-simple1.shp", package = "ofpetrial"),
    abline_type = "free"
  )

seed_rate_info <-
  prep_rate(
    plot_info = seed_plot_info,
    gc_rate = 32000,
    unit = "seed",
    min_rate = 16000,
    max_rate = 40000,
    num_rates = 4,
    design_type = "ls"
  )

trial_design <- assign_rates(exp_data, rate_info = list(seed_rate_info))

viz(trial_design)

make_trial_report(trial_design,
                  trial_name ="testing",
                  folder_path = "/Users/brittaniedge/Library/Mobile Documents/com~apple~CloudDocs/Documents/testing output")



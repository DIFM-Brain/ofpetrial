
#!===========================================================
#! One-time operations
#!===========================================================
usethis::use_package("data.table")
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("magrittr")
usethis::use_package("sf")
usethis::use_package("lwgeom")
usethis::use_package("stringr")
usethis::use_package("measurements")
usethis::use_package("purrr")
usethis::use_package("tibble")
usethis::use_package("tidyr")
usethis::use_package("ggpubr")
usethis::use_pipe() # can use %>% after this

usethis::use_build_ignore(c("dev_notes.md", "notes.md", "organize.R", "structure.rmd", "todo.md", "test.R", "docs", "README.html", "debug"))

usethis::use_readme_rmd()
usethis::use_news_md()
usethis::use_vignette("ab-line")
usethis::use_vignette("trial-design-options")
usethis::use_vignette("random-vs-orthogonal")

#!===========================================================
#! Check
#!===========================================================

devtools::document() # regenerate documents reflecting the changes and apply load_all()
devtools::load_all()

#--- build pkgdown website ---#
pkgdown::build_site()


#!===========================================================
#! Create datasets
#!===========================================================


plot_info <-
  prep_plot_fs(
    form = "NH3",
    machine_width = 30,
    section_num = 1,
    harvester_width = 20,
    headland_length = 30,
    side_length = 60
  )

usethis::use_data(plot_info)

exp_data <-
  make_exp_plots(
    input_plot_info = n_plot_info,
    boundary_file = system.file("extdata", "boundary-simple1.shp", package = "ofpetrial"),
    abline_file = system.file("extdata", "ab-line-simple1.shp", package = "ofpetrial"),
    abline_type = "free"
  )

usethis::use_data(exp_data)

rate_info <-
  prep_rates_s(
    plot_info = n_plot_info,
    gc_rate = 180,
    unit = "lb",
    rates = c(100, 140, 180, 220, 260),
    design_type = "ls",
    rank_seq_ws = c(1, 2, 3, 4, 5),
    rank_seq_as = c(1, 2, 3, 4, 5)
  )

usethis::use_data(rate_info)

trial_design <-
  assign_rates(
    exp_data = exp_data,
    rate_info = n_rate_info
  )

usethis::use_data(trial_design)


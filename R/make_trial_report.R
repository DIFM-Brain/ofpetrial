#' Create trial design report
#'
#' This function creates an html report describing the trial design created by the user with assign_rates() and includes figures showing machine alignment
#'
#' @param td trial design created by assign_rates()
#' @param trial_name (character) name of trial to be used in report
#' @param folder_path (character) path to the folder in which the report will be saved
#' @param keep_rmd (logical) If FALSE (Default), the original rmd file will be deleted upon creating an html report. Otherwise, the rmd file will be saved in the folder specified by `folder_path`.
#' @returns path to the resulting html file (invisible)
#' @import bookdown
#' @export
#' @examples
#' #--- load experiment made by assign_rates() ---#
#' \donttest{
#' data(td_single_input)
#' make_trial_report(
#'   td = td_single_input,
#'   folder_path = tempdir()
#' )
#' }
make_trial_report <- function(td, folder_path, trial_name = NA, keep_rmd = FALSE) {

  all_trial_info <-
    td %>%
    dplyr::mutate(land_unit = ifelse(unit_system == "metric", "hectares", "acres")) %>%
    dplyr::mutate(trial_name = trial_name) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(exp_plots = list(make_sf_utm(exp_plots))) %>%
    dplyr::mutate(headland = list(make_sf_utm(headland))) %>%
    dplyr::mutate(ab_lines = list(make_sf_utm(ab_lines))) %>%
    dplyr::mutate(harvest_ab_lines = list(make_sf_utm(harvest_ab_lines))) %>%
    dplyr::mutate(field_sf = list(make_sf_utm(field_sf))) %>%
    # dplyr::mutate(input_type = get_input_type(input_name)) %>%
    dplyr::mutate(field_size = get_field_size(trial_design, land_unit)) %>%
    dplyr::mutate(plot_number = get_plot_number(trial_design)) %>%
    dplyr::mutate(plot_length = list(get_plot_length(trial_design, plot_width, unit_system))) %>%
    dplyr::mutate(num_harv_pass_in_plot = plot_width / harvester_width) %>%
    dplyr::mutate(rate_number = get_rate_number(trial_design)) %>%
    dplyr::mutate(rates = list(get_trial_rates(trial_design))) %>%
    dplyr::mutate(machines_in_plot = plot_width / machine_width) %>%
    dplyr::mutate(headland_size = if (unit_system == "metric") {
      headland_length
    } else {
      conv_unit(headland_length, "meters", "feet")
    }) %>%
    dplyr::mutate(sideland_size = if (unit_system == "metric") {
      side_length
    } else {
      conv_unit(side_length, "meters", "feet")
    }) %>%
    dplyr::mutate(trial_design = list(trial_design %>%
      dplyr::mutate(area = as.numeric(st_area(.))) %>%
      dplyr::mutate(type = case_when(
        type == "headland" ~ "Border Buffer",
        type <= "experiment" ~ "Trial Area"
      )))) %>%
    dplyr::mutate(map_headlands = list(
      tmap::tm_shape(trial_design) +
        tmap::tm_polygons(
          fill = "type",
          fill.legend = tmap::tm_legend(title = "Type of Field Area"),
          fill.scale = tmap::tm_scale_categorical(values = c("red", "grey"))
        )
    )) %>%
    dplyr::mutate(total_input = list(
      trial_design %>%
        dplyr::mutate(area = as.numeric(st_area(.))) %>%
        dplyr::mutate(acres = area * 0.000247105) %>%
        dplyr::mutate(total_input = sum(acres * as.numeric(as.character(rate)))) %>%
        dplyr::pull(total_input) %>%
        unique()
    ))

  plots <- get_plots(all_trial_info)

  if (nrow(td) == 1) {
    machine_table <-
      data.table::data.table(
        width = c(td$harvester_width[1], td$machine_width),
        input_name = c(NA, td$input_name),
        machine_type = c("harvester", ifelse(td$input_name == "seed", "planter", "applicator")),
        ab_line = list(all_trial_info$harvest_ab_lines[[1]][1, ], all_trial_info$ab_lines[[1]])) %>%
      dplyr::mutate(height = max(width) / 4) %>%
      .[, machine_type := factor(machine_type, levels = c("applicator", "planter", "harvester"))] %>%
      dplyr::mutate(number_in_plot = c(max(all_trial_info$num_harv_pass_in_plot), ceiling(all_trial_info$machines_in_plot))) %>%
      dplyr::mutate(sections_used = c(1, (1 / all_trial_info$machines_in_plot))) %>%
      setorder(., cols = "machine_type") %>%
      dplyr::mutate(machine_id = dplyr::row_number()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(unit_system = td$unit_system[[1]]) %>%
      dplyr::mutate(trial_plot = list(plots)) %>%
      dplyr::mutate(move_vec = list(get_move_vec(ab_line))) %>%
      dplyr::mutate(center = list(find_center(ab_line, number_in_plot, trial_plot, move_vec, machine_id, width, height, all_trial_info))) %>%
      dplyr::mutate(machine_poly = list(make_machine_polygon(width, height, center, move_vec, st_crs(trial_plot)))) %>%
      dplyr::mutate(map_ab = list(tmap_abline(ab_line, machine_type, trial_plot))) %>%
      dplyr::mutate(map_poly = list(tmap_machine(machine_poly, machine_type, trial_plot))) %>%
      dplyr::mutate(width_line = list(make_plot_width_line(trial_plot, move_vec, unit_system, all_trial_info, height, input_name))) %>%
      dplyr::mutate(map_label = list(tmap_label(center, machine_type, trial_plot, number_in_plot, width, all_trial_info, move_vec))) %>%
      dplyr::mutate(map_plot = list(tmap_plot_all(trial_plot))) %>%
      dplyr::mutate(map_plot_indiv = list(tmap_plot_indiv(trial_plot, input_name, all_trial_info))) %>%
      dplyr::mutate(plot_legend = list(tmap_plot_legend(trial_plot)))
  } else {
    machine_table <-
      data.table::data.table(
        width = c(td$harvester_width[1], td$machine_width),
        input_name = c(NA, td$input_name),
        machine_type = c("harvester", ifelse(td$input_name == "seed", "planter", "applicator")),
        ab_line = list(all_trial_info$harvest_ab_lines[[1]][1, ], all_trial_info$ab_lines[[1]], all_trial_info$ab_lines[[2]])) %>%
      dplyr::mutate(number_in_plot = c(max(all_trial_info$num_harv_pass_in_plot), ceiling(all_trial_info$machines_in_plot))) %>%
      dplyr::mutate(sections_used = c(1, (1 / all_trial_info$machines_in_plot))) %>%
      dplyr::mutate(height = max(width) / 4) %>%
      .[, machine_type := factor(machine_type, levels = c("planter", "applicator", "harvester"))] %>%
      data.table::setorder(., cols = "machine_type") %>%
      dplyr::mutate(machine_id = dplyr::row_number()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(unit_system = td$unit_system[[1]]) %>%
      dplyr::mutate(trial_plot = list(plots)) %>%
      dplyr::mutate(move_vec = list(get_move_vec(ab_line))) %>%
      dplyr::mutate(center = list(find_center(ab_line, number_in_plot, trial_plot, move_vec, machine_id, width, height, all_trial_info))) %>%
      dplyr::mutate(machine_poly = list(make_machine_polygon(width, height, center, move_vec, st_crs(trial_plot)))) %>%
      dplyr::mutate(map_ab = list(tmap_abline(ab_line, machine_type, trial_plot))) %>%
      dplyr::mutate(map_poly = list(tmap_machine(machine_poly, machine_type, trial_plot))) %>%
      dplyr::mutate(width_line = list(make_plot_width_line(trial_plot, move_vec, unit_system, all_trial_info, height, input_name))) %>%
      dplyr::mutate(map_label = list(tmap_label(center, machine_type, trial_plot, number_in_plot, width, all_trial_info, move_vec))) %>%
      dplyr::mutate(map_plot = list(tmap_plot_all(trial_plot))) %>%
      dplyr::mutate(map_plot_indiv = list(tmap_plot_indiv(trial_plot, input_name, all_trial_info))) %>%
      dplyr::mutate(plot_legend = list(tmap_plot_legend(trial_plot)))
  }


  # if ((Sys.info()["sysname"] %in% c("windows", "Windows"))) {
  # if ((.Platform$OS.type %in% c("windows", "Windows"))) {
  temp_directory <- file.path(folder_path, "temp_make_trial_report")
  dir.create(temp_directory)
  # } else {
  #   #--- save all_trial_info and machine_table as temporary files ---#
  #   temp_directory <- tempdir()
  # }

  all_trial_info_path <- file.path(temp_directory, "all_trial_info.rds")
  machine_table_path <- file.path(temp_directory, "machine_table_path.rds")

  saveRDS(all_trial_info, all_trial_info_path)
  saveRDS(machine_table, machine_table_path)

  #++++++++++++++++++++++++++++++++++++
  #+ Create a report rmd file
  #++++++++++++++++++++++++++++++++++++
  td_rmd <-
    readLines(if (nrow(all_trial_info) > 1) {
      system.file("rmdtemplate", "make-trial-design-template-two-inputs.Rmd", package = "ofpetrial")
    } else {
      system.file("rmdtemplate", "make-trial-design-template-one-input.Rmd", package = "ofpetrial")
    }) %>%
    gsub("_all-trial-info-here_", all_trial_info_path, .) %>%
    gsub("_machine-table-here_", machine_table_path, .) %>%
    gsub(
      " for _trial-name_",
      ifelse(
        is.na(all_trial_info$trial_name[[1]]), # if no trial name specified
        "",
        paste0(" for ", all_trial_info$trial_name[[1]]) # if trial name specified
      ),
      .
    ) %>%
    gsub(
      " made for the _trial-name_ trial",
      ifelse(
        is.na(all_trial_info$trial_name[[1]]), # if no trial name specified
        "",
        paste0(" made for the ", all_trial_info$trial_name[[1]], " trial") # if trial name specified
      ),
      .
    ) %>%
    gsub("_length-unit_", ifelse(all_trial_info$unit_system[[1]] == "metric", "meter", "foot"), .) %>%
    gsub("_land-unit_", all_trial_info$land_unit[[1]], .) %>%
    gsub("_field-size_", all_trial_info$field_size[[1]], .) %>%
    gsub("_headland-size_", all_trial_info$headland_size[[1]], .) %>%
    gsub("_sideland-size_", all_trial_info$sideland_size[[1]], .)

  #++++++++++++++++++++++++++++++++++++
  #+ Write out to an rmd file
  #++++++++++++++++++++++++++++++++++++
  #--- create a temporary folder and file path ---#
  # Note: tempfile() does not work. td_rmd needs to be written explicitly
  # to an .rmd file. temporary files will be deleted upon quitting R
  # (https://stackoverflow.com/questions/58095164/what-happens-to-tempfiles-created-with-tempfile-in-r)
  report_rmd_path <- file.path(temp_directory, "temp.rmd")

  #--- write to a temporary rmd file ---#
  writeLines(td_rmd, con = report_rmd_path)

  #++++++++++++++++++++++++++++++++++++
  #+ Render the rmd to generate html report
  #++++++++++++++++++++++++++++++++++++
  #--- define html file name ---#
  html_output_file_path <- file.path(folder_path, "trial_design_report.html")

  #--- render ---#
  rmarkdown::render(
    input = report_rmd_path,
    output_file = html_output_file_path,
    quiet = TRUE
  )

  #++++++++++++++++++++++++++++++++++++
  #+ Remove all the temporary/intermediate fiels
  #++++++++++++++++++++++++++++++++++++
  unlink(temp_directory, recursive = TRUE)

  #++++++++++++++++++++++++++++++++++++
  #+ display the resulting html file on an web browser (or RStudio viewer pane)
  #++++++++++++++++++++++++++++++++++++
  viewer <- getOption("viewer")

  if (!is.null(viewer) && is.function(viewer)) {
    # (code to write some content to the file)
    viewer(html_output_file_path)
  } else {
    utils::browseURL(html_output_file_path)
  }

  return(invisible(html_output_file_path))
}

# !==================-=========================================
# ! Helper internal functions
# !===========================================================
#* +++++++++++++++++++++++++++++++++++
#* Creating text for trial design report
#* +++++++++++++++++++++++++++++++++++

text_plot_num_length <- function(all_trial_info, unit_system) {
  if (nrow(all_trial_info) == 1) {
    if (unit_system == "metric") {
      paste0(
        all_trial_info$plot_number[[1]], " rectangular plots, each ", all_trial_info$plot_width[[1]], " meters wide and between ",
        all_trial_info$plot_length[[1]][1], " and ", all_trial_info$plot_length[[1]][2], "meters long."
      )
    } else {
      paste0(
        all_trial_info$plot_number[[1]], " rectangular plots, each ", conv_unit(all_trial_info$plot_width[[1]], "meters", "feet"), " feet wide and between ",
        all_trial_info$plot_length[[1]][1], " and ", all_trial_info$plot_length[[1]][2], " feet long."
      )
    }
  } else {
    if (all_trial_info$plot_number[[1]] == all_trial_info$plot_number[[2]]) {
      if (unit_system == "metric") {
        paste0(
          all_trial_info$plot_number[[1]], " rectangular plots, each ", all_trial_info$plot_width[[1]], " meters wide and between ",
          all_trial_info$plot_length[[1]][1], " and ", all_trial_info$plot_length[[1]][2], "meters long."
        )
      } else {
        paste0(
          all_trial_info$plot_number[[1]], " rectangular plots, each ", conv_unit(all_trial_info$plot_width[[1]], "meters", "feet"), " feet wide and between ",
          all_trial_info$plot_length[[1]][1], " and ", all_trial_info$plot_length[[1]][2], " feet long."
        )
      }
    } else {
      if (unit_system == "metric") {
        paste0(
          all_trial_info$plot_number[[1]], " rectangular ", all_trial_info$input_name[[1]], " plots, each ", all_trial_info$plot_width[[1]], " meters wide and ",
          all_trial_info$plot_number[[2]], " rectangular ", all_trial_info$input_name[[2]], " plots, each ", all_trial_info$plot_width[[2]], " meters wide.",
          "The plots are between ",
          all_trial_info$plot_length[[1]][1], " and ", all_trial_info$plot_length[[1]][2], " meters long."
        )
      } else {
        paste0(
          all_trial_info$plot_number[[1]], " rectangular ", all_trial_info$input_name[[1]], " plots, each ", conv_unit(all_trial_info$plot_width[[1]], "meters", "feet"), " feet wide and ",
          all_trial_info$plot_number[[2]], " rectangular ", all_trial_info$input_name[[2]], " plots, each ", conv_unit(all_trial_info$plot_width[[2]], "meters", "feet"), " feet wide.",
          "The plots are between ",
          all_trial_info$plot_length[[1]][1], " and ", all_trial_info$plot_length[[1]][2], " feet long."
        )
      }
    }
  }
}

text_rate_number <- function(all_trial_info) {
  if (nrow(all_trial_info) == 1) {
    paste0(
      as.character(get_number_in_english(all_trial_info$rate_number)),
      " targeted ", all_trial_info$input_name, " rates."
    )
  } else {
    if (all_trial_info$rate_number[[1]] == all_trial_info$rate_number[[2]]) {
      paste0(as.character(get_number_in_english(all_trial_info$rate_number[[1]])), " targeted ", all_trial_info$input_name[[1]], " and ", all_trial_info$input_name[[2]], " rates.")
    } else {
      paste0(as.character(get_number_in_english(all_trial_info$rate_number[[1]])), " targeted ", all_trial_info$input_name[[1]], " rates and ", as.character(get_number_in_english(all_trial_info$rate_number[[2]])), " targeted ", all_trial_info$input_name[[2]], " rates.")
    }
  }
}

text_base_rate <- function(all_trial_info, unit_system) {
  if (nrow(all_trial_info) == 1) {
    if (all_trial_info$include_base_rate == TRUE) {
      if (unit_system == "metric") {
        paste0("In addition, there was a base application of ", all_trial_info$base_rate_equiv, " kilograms of ", all_trial_info$input_type, " equivalent.")
      } else {
        paste0("In addition, there was a base application of ", all_trial_info$base_rate_equiv, " pounds of ", input_type, " equivalent.")
      }
    }
  } else {
    if (all_trial_info$include_base_rate[[1]] == TRUE & all_trial_info$include_base_rate[[2]] != TRUE) {
      if (unit_system == "metric") {
        paste0("In addition, there was a base application of ", all_trial_info$base_rate_equiv[[1]], " kilograms of ", all_trial_info$input_type[[1]], " equivalent.")
      } else {
        paste0("In addition, there was a base application of ", all_trial_info$base_rate_equiv[[1]], " pounds of ", all_trial_info$input_type[[1]], " equivalent.")
      }
    } else if (all_trial_info$include_base_rate[[2]] == TRUE & all_trial_info$include_base_rate[[1]] != TRUE) {
      if (unit_system == "metric") {
        paste0("In addition, there was a base application of ", all_trial_info$base_rate_equiv[[2]], " kilograms of ", all_trial_info$input_type[[2]], " equivalent.")
      } else {
        paste0("In addition, there was a base application of ", all_trial_info$base_rate_equiv[[2]], " pounds of ", all_trial_info$input_type[[2]], " equivalent.")
      }
    } else {
      if (unit_system == "metric") {
        paste0("In addition, there was a base application of ", all_trial_info$base_rate_equiv[[1]], " kilograms of ", all_trial_info$input_type[[1]], " equivalent, and ", all_trial_info$base_rate_equiv[[2]], " kilograms of ", all_trial_info$input_type[[2]], " equivalent.")
      } else {
        paste0("In addition, there was a base application of ", all_trial_info$base_rate_equiv[[1]], " pounds of ", all_trial_info$input_type[[1]], " equivalent, and ", all_trial_info$base_rate_equiv[[1]], " pounds of ", all_trial_info$input_type[[1]], " equivalent.")
      }
    }
  }
}

trial_text_machinery_names_lower <- function(machine_table) {
  if (nrow(machine_table) > 2) {
    paste0(machine_table$machine_type[[1]], " and ", machine_table$machine_type[[2]])
  } else {
    paste0(machine_table$machine_type[[1]])
  }
}

text_plant_apply <- function(all_trial_info) {
  if (nrow(all_trial_info) > 1) {
    paste0(ifelse(all_trial_info$input_name[[1]] == "seed", "plant", "apply"), " and ", ifelse(all_trial_info$input_name[[2]] == "seed", "plant", "apply"))
  } else {
    paste0(ifelse(all_trial_info$input_name[[1]] == "seed", "plant", "apply"))
  }
}

trial_text_inputs <- function(all_trial_info) {
  if (nrow(all_trial_info) > 1) {
    paste0(all_trial_info$input_name[[1]], " and ", all_trial_info$input_name[[2]])
  } else {
    paste0(all_trial_info$input_name[[1]])
  }
}

trial_text_machinery_names_cap <- function(machine_table) {
  if (nrow(machine_table) > 2) {
    paste0(to_title(machine_table$machine_type[[1]]), " and ", to_title(machine_table$machine_type[[2]]))
  } else {
    paste0(to_title(machine_table$machine_type[[1]]))
  }
}

trial_text_ablines <- function(machine_table) {
  if (nrow(machine_table) > 2) {
    paste0(
      "The AB-lines for the ",
      machine_table$machine_type[[1]], " and ", machine_table$machine_type[[2]],
      " are ",
      ifelse(get_dot_product(machine_table$move_vec[[1]], machine_table$move_vec[[2]]) == 1, "identical due to the machinery specifications.", "not identical due to the difference in machine specifications.")
    )
  } else {
    ""
  }
}

trial_text_machine_sizes_and_plot_width <- function(machine_table, all_trial_info, unit_system) {
  if (unit_system == "metric") {
    if (nrow(machine_table) > 2) {
      if (all_trial_info$plot_width[[1]] == all_trial_info$plot_width[[2]]) {
        paste0(
          machine_table$width[[1]],
          "-meter ",
          machine_table$machine_type[[1]],
          ", ",
          machine_table$width[[2]],
          "-meter ",
          machine_table$machine_type[[2]],
          ", and ",
          all_trial_info$plot_width[[1]], "-meter plots."
        )
      } else {
        paste0(
          machine_table$width[[1]],
          "-meter ",
          machine_table$machine_type[[1]],
          ", ",
          machine_table$width[[2]],
          "-meter ",
          machine_table$machine_type[[2]],
          ", and ",
          all_trial_info$plot_width[[1]], "-meter ", all_trial_info$input_name[[1]], " plots and ",
          all_trial_info$plot_width[[2]], "-meter ", all_trial_info$input_name[[2]], " plots."
        )
      }
    } else {
      paste0(
        machine_table$width[[1]],
        "-meter ",
        machine_table$machine_type[[1]],
        " and ",
        paste0(all_trial_info$plot_width[[1]], "-meter plots.")
      )
    }
  } else {
    if (nrow(machine_table) > 2) {
      if (all_trial_info$plot_width[[1]] == all_trial_info$plot_width[[2]]) {
        paste0(
          conv_unit(machine_table$width[[1]], "meters", "feet"), "-foot ",
          machine_table$machine_type[[1]],
          ", ",
          conv_unit(machine_table$width[[2]], "meters", "feet"), "-foot ",
          machine_table$machine_type[[2]],
          ", and ",
          conv_unit(all_trial_info$plot_width[[1]], "meters", "feet"), "-foot ",
          all_trial_info$input_name[[1]], " and ", all_trial_info$input_name[[2]], " plots."
        )
      } else {
        paste0(
          conv_unit(machine_table$width[[1]], "meters", "feet"), "-foot ",
          machine_table$machine_type[[1]],
          ", ",
          conv_unit(machine_table$width[[2]], "meters", "feet"), "-foot ",
          machine_table$machine_type[[2]],
          " and ",
          conv_unit(all_trial_info$plot_width[[1]], "meters", "feet"), "-foot ", all_trial_info$input_name[[1]], " plots and ",
          conv_unit(all_trial_info$plot_width[[2]], "meters", "feet"), "-foot ", all_trial_info$input_name[[2]], " plots."
        )
      }
    } else {
      paste0(
        conv_unit(machine_table$width[[1]], "meters", "feet"),
        "-foot ",
        machine_table$machine_type[[1]],
        " and ",
        conv_unit(all_trial_info$plot_width[[1]], "meters", "feet"), "-foot plots."
      )
    }
  }
}

text_sections_used <- function(all_trial_info, machine_table, index, unit_system) {
  if (machine_table$sections_used[[index]] > 1) {
    if (unit_system == "metric") {
      paste0(
        "Although the ", machine_table$machine_type[[index]], " is ", machine_table$width[[index]],
        " meters wide, the plots are ",
        all_trial_info %>% dplyr::filter(input_name == machine_table$input_name[[index]]) %>% pull(plot_width),
        " meters wide, using ",
        as.character(get_number_in_english(machine_table$sections_used[[index]])),
        " sections of the machine in the trial plots."
      )
    } else {
      paste0(
        "Although the ", machine_table$machine_type[[index]], " is ", conv_unit(machine_table$width[[index]], "meters", "feet"),
        " feet wide, the plots are ",
        all_trial_info %>% dplyr::filter(input_name == machine_table$input_name[[index]]) %>% pull(plot_width) %>% conv_unit(., "meters", "feet"),
        " feet wide, using ",
        as.character(get_number_in_english(machine_table$sections_used[[index]])),
        " sections of the machine in the trial plots."
      )
    }
  }
}

text_plot_width <- function(all_trial_info, unit_system) {
  if (nrow(all_trial_info) == 1) {
    if (unit_system == "metric") {
      paste0(all_trial_info$plot_width[[1]], "-meter plots")
    } else {
      paste0(conv_unit(all_trial_info$plot_width[[1]], "meters", "feet"), "-foot plots")
    }
  } else {
    if (all_trial_info$plot_width[[1]] == all_trial_info$plot_width[[2]]) {
      if (unit_system == "metric") {
        paste0(all_trial_info$plot_width[[1]], "-meter ", all_trial_info$input_name[[1]], " and ", all_trial_info$input_name[[2]], " plots")
      } else {
        paste0(conv_unit(all_trial_info$plot_width[[1]], "meters", "feet"), "-foot ", all_trial_info$input_name[[1]], " and ", all_trial_info$input_name[[2]], " plots")
      }
    } else {
      if (unit_system == "metric") {
        paste0(all_trial_info$plot_width[[1]], "-meter ", all_trial_info$input_name[[1]], " plots and ", all_trial_info$plot_width[[2]], "-meter ", all_trial_info$input_name[[2]], " plots")
      } else {
        paste0(conv_unit(all_trial_info$plot_width[[1]], "meters", "feet"), "-foot ", all_trial_info$input_name[[1]], " plots and ", conv_unit(all_trial_info$plot_width[[2]], "meters", "feet"), "-foot ", all_trial_info$input_name[[2]], " plots")
      }
    }
  }
}

text_harvester_passes <- function(all_trial_info, unit_system) {
  if (nrow(all_trial_info) > 1) {
    if (all_trial_info$plot_width[[1]] == all_trial_info$plot_width[[2]]) {
      if (unit_system == "metric") {
        paste0(
          as.character(get_number_in_english(all_trial_info$num_harv_pass_in_plot[[1]])),
          " ",
          all_trial_info$harvester_width[[1]],
          "-meter harvester swath",
          if (all_trial_info$num_harv_pass_in_plot[[1]] > 1) {
            "s"
          } else {
            ""
          },
          " will lie neatly within each ",
          text_plot_width(all_trial_info, unit_system)
        )
      } else {
        paste0(
          as.character(get_number_in_english(all_trial_info$num_harv_pass_in_plot[[1]])),
          " ",
          conv_unit(all_trial_info$harvester_width[[1]], "meters", "feet"),
          "-foot harvester swath",
          if (all_trial_info$num_harv_pass_in_plot[[1]] > 1) {
            "s"
          } else {
            ""
          },
          " will lie neatly within each ",
          text_plot_width(all_trial_info, unit_system)
        )
      }
    } else {
      if (unit_system == "metric") {
        paste0(
          as.character(get_number_in_english(all_trial_info$num_harv_pass_in_plot[[1]])),
          " ",
          all_trial_info$harvester_width[[1]],
          "-meter harvester swath",
          if (all_trial_info$num_harv_pass_in_plot[[1]] > 1) {
            "s"
          } else {
            ""
          },
          " and ",
          as.character(get_number_in_english(all_trial_info$num_harv_pass_in_plot[[2]])),
          " ",
          all_trial_info$harvester_width[[2]],
          "-meter harvester swath",
          if (all_trial_info$num_harv_pass_in_plot[[2]] > 1) {
            "s"
          } else {
            ""
          },
          " will lie neatly within each ",
          text_plot_width(all_trial_info, unit_system),
          ", respectively"
        )
      } else {
        paste0(
          as.character(get_number_in_english(all_trial_info$num_harv_pass_in_plot[[1]])),
          " ",
          conv_unit(all_trial_info$harvester_width[[1]], "meters", "feet"),
          "-foot harvester swath",
          if (all_trial_info$num_harv_pass_in_plot[[1]] > 1) {
            "s"
          } else {
            ""
          },
          " and ",
          as.character(get_number_in_english(all_trial_info$num_harv_pass_in_plot[[2]])),
          " ",
          conv_unit(all_trial_info$harvester_width[[2]], "meters", "feet"),
          "-foot harvester swath",
          if (all_trial_info$num_harv_pass_in_plot[[2]] > 1) {
            "s"
          } else {
            ""
          },
          " will lie neatly within each ",
          text_plot_width(all_trial_info, unit_system),
          ", respectively"
        )
      }
    }
  } else {
    if (unit_system == "metric") {
      paste0(
        as.character(get_number_in_english(all_trial_info$num_harv_pass_in_plot[[1]])),
        " ",
        all_trial_info$harvester_width[[1]],
        "-meter harvester swath",
        if (all_trial_info$num_harv_pass_in_plot[[1]] > 1) {
          "s"
        } else {
          ""
        },
        " will lie neatly within each ",
        text_plot_width(all_trial_info, unit_system)
      )
    } else {
      paste0(
        as.character(get_number_in_english(all_trial_info$num_harv_pass_in_plot[[1]])),
        " ",
        conv_unit(all_trial_info$harvester_width[[1]], "meters", "feet"),
        "-foot harvester swath",
        if (all_trial_info$num_harv_pass_in_plot[[1]] > 1) {
          "s"
        } else {
          ""
        },
        " will lie neatly within each ",
        text_plot_width(all_trial_info, unit_system)
      )
    }
  }
}

get_input_type <- function(input) {
  match <-
    input_type_table %>%
    dplyr::filter(input_name == input) %>%
    dplyr::pull(input_type)

  if (length(match) == 0) {
    match <- input
  }
}

get_field_size <- function(trial_design, land_unit) {
  trial_design %>%
    dplyr::mutate(area = as.numeric(st_area(.))) %>%
    dplyr::pull(area) %>%
    sum(.) %>%
    conv_unit(., "m2", land_unit) %>%
    round(.)
}

get_plot_number <- function(trial_design) {
  trial_design %>%
    dplyr::filter(type == "experiment") %>%
    nrow()
}

get_plot_length <- function(trial_design, plot_width, unit_system) {
  trial_design %>%
    dplyr::mutate(length = as.numeric(st_area(.)) / plot_width) %>%
    dplyr::mutate(length = ifelse(unit_system == "imperial", conv_unit(length, "meters", "feet"), length)) %>%
    dplyr::pull(length) %>%
    quantile(., c(0.1, 0.9)) %>%
    round(.)
}

get_rate_number <- function(trial_design) {
  trial_design %>%
    dplyr::pull(rate) %>%
    unique(.) %>%
    length(.)
}

get_trial_rates <- function(trial_design) {
  trial_design %>%
    dplyr::pull(rate) %>%
    unique(.) %>%
    sort(.)
}

rotate_vec <- function(vec, angle) {
  rotate_mat <- matrix(
    c(
      cos(pi * angle / 180),
      sin(pi * angle / 180),
      -sin(pi * angle / 180),
      cos(pi * angle / 180)
    ),
    nrow = 2
  )
  return(vec %*% rotate_mat)
}

machine_polygon <- function(width, height, center, move_vec, crs) {
  normalized_move_vec <- move_vec / sqrt(sum(move_vec^2)) # normalized direction aka normal vector
  perp_move_vec <- rotate_vec(normalized_move_vec, 90) # perpendicular vector to the normal vector

  width_in <- width * 0.4
  height_up <- height * 0.4

  # Create vertices
  middle_bottom <- center - normalized_move_vec * height / 2
  left_bottom <- middle_bottom - perp_move_vec * width / 2
  left_up <- left_bottom + normalized_move_vec * height
  left_in_bottom <- left_up + perp_move_vec * width_in
  left_in_up <- left_in_bottom + normalized_move_vec * height_up
  left_in_left <- left_in_up - perp_move_vec * height_up
  top_point <- center + normalized_move_vec * (height + height_up / 2)
  right_bottom <- middle_bottom + perp_move_vec * width / 2
  right_up <- right_bottom + normalized_move_vec * height
  right_in_bottom <- right_up - perp_move_vec * width_in
  right_in_up <- right_in_bottom + normalized_move_vec * height_up
  right_in_right <- right_in_up + perp_move_vec * height_up

  # Create machine sf
  polygon_sf <-
    list(
      rbind(
        middle_bottom,
        left_bottom,
        left_up,
        left_in_bottom,
        left_in_up,
        left_in_left,
        top_point,
        right_in_right,
        right_in_up,
        right_in_bottom,
        right_up,
        right_bottom,
        middle_bottom
      )
    ) %>%
    st_polygon() %>%
    st_sfc(crs = crs) %>%
    st_sf()

  return(polygon_sf)
}

make_machine_polygon <- function(width, height, center, move_vec, crs) {
  if (length(center) > 2) {
    polys <- list()
    for (i in 1:nrow(center)) {
      if ((i %% 2) != 0) {
        move_vec_180 <- rotate_vec(move_vec, 180)
        polys[[i]] <- machine_polygon(width, height, center[i, ], move_vec_180, crs)
      } else {
        polys[[i]] <- machine_polygon(width, height, center[i, ], move_vec, crs)
      }
    }
    polygon_sf <- do.call(rbind, polys)
  } else {
    polygon_sf <- machine_polygon(width, height, center, move_vec, crs)
  }

  return(polygon_sf)
}

make_section_polygon <- function(width, machine_poly, sections_used, move_vec, crs) {
  if (sections_used > 1) {
    perp_move_vec <- rotate_vec(move_vec, 90)
    width_section <- width / sections_used

    polys <- list()
    for (i in 1:sections_used) {
      coords <- sf::st_coordinates(machine_poly)[c(2, 3, 11, 12, 2), 1:2]

      first_top <- coords[2, ] + perp_move_vec * width_section * (i - 1)
      first_bottom <- coords[1, ] + perp_move_vec * width_section * (i - 1)
      next_top <- coords[2, ] + perp_move_vec * width_section * i
      next_bottom <- coords[1, ] + perp_move_vec * width_section * i

      polys[[i]] <- list(
        rbind(
          first_bottom,
          first_top,
          next_top,
          next_bottom,
          first_bottom
        )
      ) %>%
        sf::st_polygon() %>%
        sf::st_sfc(crs = crs) %>%
        sf::st_sf()
    }
    polygon_sf <- do.call(rbind, polys)
  } else {
    polygon_sf <- NULL
  }

  return(polygon_sf)
}

# trial_plot <- machine_table$trial_plot[[1]]
# move_vec <- machine_table$move_vec[[1]]
# input_name <- machine_table$input_name[[1]]
# height <- machine_table$height[[1]]
# unit_system <- "imperial"
make_plot_width_line <- function(trial_plot, move_vec, unit_system, all_trial_info, height, input_name) {
  if (is.na(input_name) == FALSE) {
    # directions for figure creation
    perp_move_vec <- rotate_vec(move_vec, 90)
    opp_move_vec <- rotate_vec(move_vec, 180) %>%
      as.matrix() %>%
      .[1, ]

    # get plot width
    plot_width <- all_trial_info %>%
      dplyr::pull(plot_width) %>%
      max()

    # plot with maximum size
    trial_plot <- trial_plot %>%
      dplyr::mutate(area = as.numeric(st_area(.))) %>%
      dplyr::filter(area == max(area))

    trial_plot_coords <- trial_plot %>%
      st_coordinates() %>%
      .[, -(3:4)] %>% # remove the last two columns
      as.data.frame()

    # find starting side of polygon (opposite of direction of move_vec)
    if (move_vec[1] >= 0 & move_vec[2] > 0) { # when we are moving in a general NE or N direction
      point <- trial_plot_coords %>%
        data.frame(.) %>%
        dplyr::filter(X <= median(X)) %>%
        dplyr::filter(Y < median(Y)) %>%
        .[1, ]
    } else if (move_vec[1] < 0 & move_vec[2] <= 0) { # SW or S direction
      point <- trial_plot_coords %>%
        data.frame(.) %>%
        dplyr::filter(X >= median(X)) %>%
        dplyr::filter(Y > median(Y)) %>%
        .[1, ]
    } else if (move_vec[1] >= 0 & move_vec[2] < 0) { # SE or E direction
      point <- trial_plot_coords %>%
        data.frame(.) %>%
        dplyr::filter(X < median(X)) %>%
        dplyr::filter(Y >= median(Y)) %>%
        .[1, ]
    } else if (move_vec[1] < 0 & move_vec[2] >= 0) { # NW or W direction
      point <- trial_plot_coords %>%
        data.frame(.) %>%
        dplyr::filter(X > median(X)) %>%
        dplyr::filter(Y <= median(Y)) %>%
        .[1, ]
    }

    # find the two lines that are close to the width of the plot
    plot_lines <- data.table(
      line = 1:4,
      distance = c(
        st_distance(st_point(trial_plot_coords[1, ] %>% as.numeric(.)), st_point(trial_plot_coords[2, ] %>% as.numeric(.))),
        st_distance(st_point(trial_plot_coords[2, ] %>% as.numeric(.)), st_point(trial_plot_coords[3, ] %>% as.numeric(.))),
        st_distance(st_point(trial_plot_coords[3, ] %>% as.numeric(.)), st_point(trial_plot_coords[4, ] %>% as.numeric(.))),
        st_distance(st_point(trial_plot_coords[4, ] %>% as.numeric(.)), st_point(trial_plot_coords[1, ] %>% as.numeric(.)))
      )
    ) %>%
      dplyr::mutate(diff = abs(plot_width - distance)) %>%
      dplyr::arrange(diff) %>% # arrange in descending order
      dplyr::slice(1:2) %>%
      dplyr::pull(line)

    # find the line that is in the set of plot_lines and also contains the earlier point found
    if (1 %in% plot_lines & (setequal(point, trial_plot_coords[1, ]) | setequal(point, trial_plot_coords[2, ]))) {
      point1 <- st_point(trial_plot_coords[1, ] %>% as.numeric(.)) %>%
        as.matrix() %>%
        .[1, ]
      point1 <- point1 + move_vec * height * 1.4

      point2 <- st_point(trial_plot_coords[2, ] %>% as.numeric(.)) %>%
        as.matrix() %>%
        .[1, ]
      point2 <- point2 + move_vec * height * 1.4
    } else if (2 %in% plot_lines & (setequal(point, trial_plot_coords[2, ]) | setequal(point, trial_plot_coords[3, ]))) {
      point1 <- st_point(trial_plot_coords[2, ] %>% as.numeric(.)) %>%
        as.matrix() %>%
        .[1, ]
      point1 <- point1 + move_vec * height * 1.4

      point2 <- st_point(trial_plot_coords[3, ] %>% as.numeric(.)) %>%
        as.matrix() %>%
        .[1, ]
      point2 <- point2 + move_vec * height * 1.4
    } else if (3 %in% plot_lines & (setequal(point, trial_plot_coords[3, ]) | setequal(point, trial_plot_coords[4, ]))) {
      point1 <- st_point(trial_plot_coords[3, ] %>% as.numeric(.)) %>%
        as.matrix() %>%
        .[1, ]
      point1 <- point1 + move_vec * height * 1.4

      point2 <- st_point(trial_plot_coords[4, ] %>% as.numeric(.)) %>%
        as.matrix() %>%
        .[1, ]
      point2 <- point2 + move_vec * height * 1.4
    } else if (4 %in% plot_lines & (setequal(point, trial_plot_coords[4, ]) | setequal(point, trial_plot_coords[1, ]))) {
      point1 <- st_point(trial_plot_coords[4, ] %>% as.numeric(.)) %>%
        as.matrix() %>%
        .[1, ]
      point1 <- point1 + move_vec * height * 1.4

      point2 <- st_point(trial_plot_coords[1, ] %>% as.numeric(.)) %>%
        as.matrix() %>%
        .[1, ]
      point2 <- point2 + move_vec * height * 1.4
    }

    arrow_ll <- point1 - 1 * opp_move_vec
    arrow_lr <- point1 + 1 * opp_move_vec

    arrow_rl <- point2 - 1 * move_vec
    arrow_rr <- point2 + 1 * move_vec

    new_line <-
      sf::st_linestring(rbind(
        arrow_rl,
        point2,
        arrow_rr,
        point2,
        point1,
        arrow_ll,
        point1,
        arrow_lr
      )) %>%
      sf::st_sfc(crs = sf::st_crs(trial_plot$geometry)) %>%
      sf::st_sf()

    label_line <-
      rbind(
        (point1 - 3.2 * move_vec),
        (point2 - 3.2 * move_vec)
      ) %>%
      sf::st_linestring() %>%
      sf::st_sfc(crs = sf::st_crs(trial_plot$geometry)) %>%
      sf::st_sf() %>%
      dplyr::mutate(
        label = ifelse(
          unit_system == "metric",
          paste0(plot_width, " meters"),
          paste0(conv_unit(plot_width, "meters", "feet"), " feet")
        )
      )

    tmap::tm_shape(new_line, bbox = sf::st_bbox(trial_plot$geometry)) +
      tmap::tm_lines(
        col = "black",
        lwd = 2
      ) +
      tmap::tm_shape(label_line, bbox = sf::st_bbox(trial_plot$geometry)) +
      tmap::tm_labels("label",
                      col = "red",
                      size = 1,
                      fontface = "bold"
      )
  } else {
    NULL
  }
}

# ab_line <- machine_table$ab_line[[1]]
get_move_vec <- function(ab_line){
  lags <- st_coordinates(ab_line) %>%
    data.frame() %>%
    dplyr::mutate(dx = X - dplyr::lag(X, n = 1),
           dy = Y - dplyr::lag(Y, n = 1))

  vec <- c(lags$dx[2], lags$dy[2])
  move_vec <- vec / sqrt(sum(vec^2))

  return(move_vec)
}

# ab_line <- machine_table$ab_line[[1]]
# number_in_plot <- machine_table$number_in_plot[[1]]
# trial_plot <- machine_table$trial_plot[[1]]
# move_vec <- machine_table$move_vec[[1]]
# machine_id <- machine_table$machine_id[[1]]
# machine_width <- machine_table$width[[1]]
# height <- machine_table$height[[1]]

find_center <- function(ab_line, number_in_plot, trial_plot, move_vec, machine_id, machine_width, height, all_trial_info) {
  normalized_move_vec <- move_vec / sqrt(sum(move_vec^2)) # normalized direction aka normal vector
  perp_move_vec <- rotate_vec(normalized_move_vec, 90) # perpendicular vector to the normal vector

  # get max plot width
  plot_width <- all_trial_info %>%
    dplyr::pull(plot_width) %>%
    max()

  # use trial plot with maximum size
  trial_plot <- trial_plot %>%
    dplyr::mutate(area = as.numeric(st_area(.))) %>%
    dplyr::filter(area == max(area))

  # get coordinates of the plot vertices
  trial_plot_coords <- trial_plot %>%
    sf::st_coordinates() %>%
    .[, -(3:4)] %>% # remove the last two columns
    as.data.frame(.)

  # find starting side of polygon (opposite of direction of move_vec)
  if (move_vec[1] >= 0 & move_vec[2] > 0) { # when we are moving in a general NE or N direction
    point <- trial_plot_coords %>%
      data.frame(.) %>%
      dplyr::filter(X <= median(X)) %>%
      dplyr::filter(Y < median(Y)) %>%
      .[1, ]
  } else if (move_vec[1] < 0 & move_vec[2] <= 0) { # SW or S direction
    point <- trial_plot_coords %>%
      data.frame(.) %>%
      dplyr::filter(X >= median(X)) %>%
      dplyr::filter(Y > median(Y)) %>%
      .[1, ]
  } else if (move_vec[1] >= 0 & move_vec[2] < 0) { # SE or E direction
    point <- trial_plot_coords %>%
      data.frame(.) %>%
      dplyr::filter(X < median(X)) %>%
      dplyr::filter(Y >= median(Y)) %>%
      .[1, ]
  } else if (move_vec[1] < 0 & move_vec[2] >= 0) { # NW or W direction
    point <- trial_plot_coords %>%
      data.frame(.) %>%
      dplyr::filter(X > median(X)) %>%
      dplyr::filter(Y <= median(Y)) %>%
      .[1, ]
  }

  # find the two lines that are close to the width of the plot
  plot_lines <- data.table(
    line = 1:4,
    distance = c(
      st_distance(st_point(trial_plot_coords[1, ] %>% as.numeric(.)), st_point(trial_plot_coords[2, ] %>% as.numeric(.))),
      st_distance(st_point(trial_plot_coords[2, ] %>% as.numeric(.)), st_point(trial_plot_coords[3, ] %>% as.numeric(.))),
      st_distance(st_point(trial_plot_coords[3, ] %>% as.numeric(.)), st_point(trial_plot_coords[4, ] %>% as.numeric(.))),
      st_distance(st_point(trial_plot_coords[4, ] %>% as.numeric(.)), st_point(trial_plot_coords[1, ] %>% as.numeric(.)))
    )
  ) %>%
    dplyr::mutate(diff = abs(plot_width - distance)) %>%
    dplyr::arrange(diff) %>% # arrange in descending order
    dplyr::slice(1:2) %>%
    dplyr::pull(line)

  # get center of line
  if (1 %in% plot_lines & (setequal(point, trial_plot_coords[1, ]) | setequal(point, trial_plot_coords[2, ]))) {
    cent <- st_linestring(rbind(
      trial_plot_coords[1, ] %>% as.numeric(.),
      trial_plot_coords[2, ] %>% as.numeric(.)
    )) %>%
      st_sfc() %>%
      st_sf() %>%
      st_centroid()
  } else if (2 %in% plot_lines & (setequal(point, trial_plot_coords[2, ]) | setequal(point, trial_plot_coords[3, ]))) {
    cent <- st_linestring(rbind(
      trial_plot_coords[2, ] %>% as.numeric(.),
      trial_plot_coords[3, ] %>% as.numeric(.)
    )) %>%
      st_sfc() %>%
      st_sf() %>%
      st_centroid()
  } else if (3 %in% plot_lines & (setequal(point, trial_plot_coords[3, ]) | setequal(point, trial_plot_coords[4, ]))) {
    cent <- st_linestring(rbind(
      trial_plot_coords[3, ] %>% as.numeric(.),
      trial_plot_coords[4, ] %>% as.numeric(.)
    )) %>%
      st_sfc() %>%
      st_sf() %>%
      st_centroid()
  } else if (4 %in% plot_lines & (setequal(point, trial_plot_coords[4, ]) | setequal(point, trial_plot_coords[1, ]))) {
    cent <- st_linestring(rbind(
      trial_plot_coords[4, ] %>% as.numeric(.),
      trial_plot_coords[1, ] %>% as.numeric(.)
    )) %>%
      st_sfc() %>%
      st_sf() %>%
      st_centroid()
  }

  # now move the center to be where the first pass of the machine will be in the plot
  cent <- st_shift(
    cent,
    if ((normalized_move_vec[1] > 0) |
      (normalized_move_vec[1] == 0 & normalized_move_vec[2] > 0)) {
      -perp_move_vec * (plot_width / 2 - machine_width / 2)
    } else {
      perp_move_vec * (plot_width / 2 - machine_width / 2)
    }
  ) %>%
    sf::st_coordinates()

  cent <- cent + normalized_move_vec * (height * 2.8 * machine_id)

  if (number_in_plot > 1) {
    for (i in (2:number_in_plot)) {
      cent_2 <-
        st_shift(
          sf::st_point(cent[i - 1, ]) %>%
            sf::st_sfc() %>%
            sf::st_sf(),
          if ((normalized_move_vec[1] > 0) |
            (normalized_move_vec[1] == 0 & normalized_move_vec[2] > 0)) {
            perp_move_vec * machine_width
          } else {
            -perp_move_vec * machine_width
          }
        ) %>%
        sf::st_coordinates()

      cent <- rbind(cent, cent_2)
    }
  }

  return(cent)
}

get_plots <- function(all_trial_info) {
  if (length(all_trial_info$plot_width %>% unique()) == 1) {
    design <-
      all_trial_info$trial_design[[1]] %>%
      dplyr::mutate(plot_id = dplyr::row_number()) %>%
      dplyr::filter(type == "Trial Area") %>%
      st_transform_utm()

    abline <- all_trial_info$ab_lines[[1]]
    if (all_trial_info$machine_width[[1]] == 2 * all_trial_info$plot_width[[1]]) {
      move_vec <- get_move_vec(abline)
      normalized_move_vec <- move_vec / sqrt(sum(move_vec^2)) # normalized direction aka normal vector
      perp_move_vec <- rotate_vec(normalized_move_vec, 90) # perpendicular vector to the normal vector


      abline <- sf::st_as_sf(st_shift(
        abline,
        c(-1, 1) * perp_move_vec * (1),
        merge = FALSE
      ))
    }

    first_plot <-
      design %>%
      dplyr::mutate(
        plot_id =
          st_intersection_quietly(st_transform_utm(design), sf::st_centroid(abline)) %>%
            .$result %>%
            dplyr::pull(plot_id) %>%
            min(.)
      )

    if (nrow(all_trial_info) == 2) {
      plots <-
        rbind(
          design %>%
            dplyr::filter(plot_id == first_plot$plot_id) %>%
            st_transform_utm(.) %>%
            dplyr::mutate(input_name = all_trial_info$input_name[[1]]) %>%
            dplyr::select(rate, strip_id, plot_id, type, input_name),
          design %>%
            dplyr::filter(plot_id == first_plot$plot_id) %>%
            st_transform_utm(.) %>%
            dplyr::mutate(input_name = all_trial_info$input_name[[2]]) %>%
            dplyr::select(rate, strip_id, plot_id, type, input_name)
        )
    } else {
      plots <-
        design %>%
        dplyr::filter(plot_id == first_plot$plot_id) %>%
        st_transform_utm(.) %>%
        dplyr::mutate(input_name = all_trial_info$input_name)
      # %>%
      #   dplyr::select(rate, strip_id, plot_id, type, input_name)
    }
  } else {
    max_input <- all_trial_info %>%
      dplyr::filter(plot_width == max(all_trial_info$plot_width))

    min_input <- all_trial_info %>%
      dplyr::filter(plot_width != max(all_trial_info$plot_width))

    # abline of input with max plot width
    abline <- max_input$ab_lines[[1]]

    design1 <- max_input$trial_design[[1]] %>%
      dplyr::mutate(plot_id = dplyr::row_number()) %>%
      dplyr::filter(type == "Trial Area")

    design2 <- min_input$trial_design[[1]] %>%
      dplyr::mutate(plot_id = dplyr::row_number()) %>%
      dplyr::filter(type == "Trial Area")

    plot1 <-
      design1 %>%
      dplyr::filter(plot_id == st_intersection(st_transform_utm(design1), sf::st_centroid(abline)) %>%
        dplyr::pull(plot_id) %>%
        min(.)) %>%
      st_transform_utm(.) %>%
      dplyr::mutate(input_name = max_input$input_name) %>%
      dplyr::select(rate, strip_id, plot_id, type, input_name)

    plot2 <-
      sf::st_intersection(st_transform_utm(design2), plot1) %>%
      dplyr::mutate(area = st_area(.)) %>%
      dplyr::filter(area >= median(area)) %>%
      st_transform_utm(.) %>%
      dplyr::mutate(input_name = min_input$input_name) %>%
      dplyr::select(rate, strip_id, plot_id, type, input_name)

    plots <- rbind(plot1, plot2)
  }

  return(plots)
}

# ab_line <- machine_table$ab_line[[1]]
# machine_type <- machine_table$machine_type[[1]]
# trial_plot <- machine_table$trial_plot[[1]]
tmap_abline <- function(ab_line, machine_type, trial_plot) {
  tmap::tm_shape(ab_line, bbox = st_bbox(trial_plot)) +
    tmap::tm_lines(
      col = if (machine_type == "planter") {
        "lawngreen"
      } else if (machine_type == "applicator") {
        "#00A3FF"
      } else {
        "#E69F00"
      },
      lty = "dashed",
      lwd = 3
    )
}

# ab_line <- machine_table$ab_line[[1]]
# machine_type <- machine_table$machine_type[[1]]
# trial_plot <- machine_table$trial_plot[[1]]
tmap_machine <- function(machine_poly, machine_type, trial_plot) {
  tmap::tm_shape(machine_poly, bbox = sf::st_bbox(trial_plot)) +
    tmap::tm_borders(col = if (machine_type == "planter") {
      "lawngreen"
    } else if (machine_type == "applicator") {
      "#00A3FF"
    } else {
      "#E69F00"
    }, lwd = 3) +
    tmap::tm_fill(
      fill = if (machine_type == "planter") {
        "lawngreen"
      } else if (machine_type == "applicator") {
        "#00A3FF"
      } else {
        "#E69F00"
      },
      fill_alpha = 1
    )
}

# section_poly <- polygon_sf
# trial_plot <- machine_table$trial_plot[[1]]
tmap_sections <- function(section_poly, trial_plot) {
  if (is.null(section_poly) == FALSE) {
    tmap::tm_shape(section_poly, bbox = sf::st_bbox(trial_plot)) +
      tmap::tm_borders(col = "grey18", lwd = 2, lty = "dotted")
  } else {
    ""
  }
}

# trial_plot <- machine_table$trial_plot[[1]]
tmap_plot_all <- function(trial_plot) {
  if (trial_plot %>%
    dplyr::mutate(area = as.numeric(st_area(.))) %>%
    dplyr::pull(area) %>%
    unique(.) %>%
    length() == 1) {
    map <- tmap::tm_shape(trial_plot, bbox = st_bbox(trial_plot)) +
      tmap::tm_borders(col = "black")
  } else {
    plots <- trial_plot %>%
      dplyr::mutate(area = as.numeric(st_area(.))) %>%
      dplyr::arrange(desc(area))

    map_small_plots <- list()
    for (i in 2:nrow(plots)) {
      map_small_plots[[i]] <- paste0("tmap::tm_shape(plots[", i, ",], bbox = st_bbox(plots)) +
        tmap::tm_borders(col = \"gray\", lwd = 2, lty = \"dashed\")")
    }

    map <-
      tmap::tm_shape(plots[1, ], bbox = st_bbox(plots)) +
      tmap::tm_borders(col = "black", lwd = 5) +
      tmap::tm_fill(col = "white") +
      eval(parse(text = paste0(map_small_plots, collapse = " + ")))
  }

  return(map)
}

# trial_plot <- machine_table$trial_plot[[1]]
# input <- machine_table$input_name[[1]]
tmap_plot_indiv <- function(trial_plot, input, all_trial_info) {
  if (is.na(input) == TRUE) {
    map <- NA
  } else {
    plots <- trial_plot %>%
      dplyr::filter(input_name == input)

    n_rates <- plots %>%
      dplyr::pull(rate) %>%
      unique() %>%
      length()

    my_palette <- get_palette_grey(n_rates)

    map <-
      tmap::tm_shape(plots %>% dplyr::mutate(rate = as.factor(rate)),
        bbox = sf::st_bbox(plots)
      ) +
      tmap::tm_fill(
        fill = "rate",
        fill.scale = tmap::tm_scale_categorical(values = my_palette),
        fill.legend = tmap::tm_legend(title = paste0("Trial Plot ", to_title(input), " Rate")))
  }

  return(map)
}

# trial_plot <- machine_table$trial_plot[[1]]
tmap_plot_legend <- function(trial_plot) {
  if (trial_plot %>%
    dplyr::mutate(area = as.numeric(st_area(.))) %>%
    dplyr::pull(area) %>%
    unique(.) %>%
    length() == 1) {
    legend <- tmap::tm_add_legend(
      title = "Trial Plots",
      type = "symbols",
      labels = c("Trial Plot"),
      col = c("black"),
      shape = 0,
      size = 2
    )
  } else {
    plots <- trial_plot %>%
      dplyr::mutate(area = as.numeric(st_area(.))) %>%
      dplyr::arrange(desc(area)) %>%
      dplyr::pull(input_name) %>%
      unique()

    legend <-
      tmap::tm_add_legend(
        title = "Trial Plots",
        type = "symbols",
        labels = c(paste0(to_title(plots[1]), " Trial Plot"), paste0(to_title(plots[2]), " Trial Plot")),
        col = c("black", "gray"),
        shape = 0,
        size = 2
      )
  }
  return(legend)
}

# center = machine_table$center[[1]]
# machine_type = machine_table$machine_type[[1]]
# trial_plot = machine_table$trial_plot[[1]]
# number_in_plot = machine_table$number_in_plot[[1]]
# machine_width = machine_table$width[[1]]
# all_trial_info
# move_vec = machine_table$move_vec[[1]]
tmap_label <- function(center, machine_type, trial_plot, number_in_plot, machine_width, all_trial_info, move_vec) {
  max_plot_width <- all_trial_info %>%
    dplyr::pull(plot_width) %>%
    max()

  normalized_move_vec <- move_vec / sqrt(sum(move_vec^2)) # normalized direction aka normal vector
  perp_move_vec <- rotate_vec(normalized_move_vec, 90) # perpendicular vector to the normal vector

  if (max_plot_width / machine_width < 1) { # if the machine is larger than the plot, we need to shift the center over to see the text
    center <- st_shift(
      st_point(center) %>%
        st_sfc() %>%
        st_sf(),
      if ((normalized_move_vec[1] > 0) |
        (normalized_move_vec[1] == 0 & normalized_move_vec[2] > 0)) {
        perp_move_vec * (max_plot_width / 2 - machine_width / 2)
      } else {
        -perp_move_vec * (max_plot_width / 2 - machine_width / 2)
      }
    ) %>%
      sf::st_coordinates()
  }

  labels <- list()
  for (i in 1:nrow(center)) {
    labels[[i]] <- paste0("tmap::tm_shape(st_point(center[", i, ", ]) %>% sf::st_sfc(crs = sf::st_crs(trial_plot)) %>% sf::st_sf() %>% dplyr::mutate(label = if(machine_type == \"planter\"){\"Planter\"}else if(machine_type == \"applicator\"){\"Applicator\"}else{\"Harvester\"}), bbox = sf::st_bbox(trial_plot)) + tmap::tm_text(\"label\", size = ifelse(number_in_plot <= 2, 0.8, 0.7))")
  }
  tmap_label <- eval(parse(text = paste0(labels, collapse = " + ")))

  return(tmap_label)
}

#++++++++++++++++++++++++++++++++++++
#+ Ger number in english
#++++++++++++++++++++++++++++++++++++
get_number_in_english <- function(num) {
  return(number_english_dictionary[number == num, num_in_english])
}

#++++++++++++++++++++++++++++++++++++
#+ Calculate dot product
#++++++++++++++++++++++++++++++++++++
get_dot_product <- function(vec_1, vec_2) {
  return(sum(vec_1 * vec_2))
}

#++++++++++++++++++++++++++++++++++++
#+ Get palette
#++++++++++++++++++++++++++++++++++++
get_palette_grey <- function(num_rates) {
  return(my_palettes_grey[n_rates == num_rates, my_palette][[1]])
}

#++++++++++++++++++++++++++++++++++++
#+ Make title for figure
#++++++++++++++++++++++++++++++++++++
to_title <- function(string) {
  if (string %in% c("UAN28", "UAN32", "uan32", "uan28")) {
    title <- toupper(as.character(string))
  } else {
    title <- tools::toTitleCase(as.character(string))
  }
  return(title)
}

text_total_input_amounts <- function(all_trial_info) {
  if (nrow(all_trial_info) == 1) {
    paste0(
      "The total amount of ",
      all_trial_info$input_name[[1]],
      " applied on the field will be ",
      round(all_trial_info$total_input[[1]]),
      " ",
      all_trial_info$unit[1],
      "."
    )
  } else if (nrow(all_trial_info) == 2 & length(all_trial_info$unit %>% unique()) == 1) {
    paste0(
      "The total amount of ",
      all_trial_info$input_name[[1]],
      " and ",
      all_trial_info$input_name[[2]],
      " applied on the field will be ",
      round(all_trial_info$total_input[[1]]),
      " and ",
      round(all_trial_info$total_input[[2]]),
      " ",
      all_trial_info$unit[[1]],
      ", respectively."
    )
  } else {
    paste0(
      "The total amount of ",
      all_trial_info$input_name[[1]],
      " and ",
      all_trial_info$input_name[[2]],
      " applied on the field will be ",
      round(all_trial_info$total_input[[1]]),
      " ",
      all_trial_info$unit[[1]],
      " and ",
      round(all_trial_info$total_input[[2]]),
      " ",
      all_trial_info$unit[[2]],
      ", respectively."
    )
  }
}

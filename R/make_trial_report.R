#' Create trial design report
#'
#' This function creates a report describing the trial design created by the user with assign_rates() and includes figures showing machine alignment
#'
#' @param td trial design created by assign_rates()
#' @param land_unit unit of land area in report ("acres" or "hectares")
#' @param units units for report ("imperial" or "metric")
#' @param trial_name (character) name of trial to be used in report
#' @param folder_path (character) path to the folder in which the report will be saved
#' @returns html document with trial design description and figures to guide trial implementation based on your machinery and plot sizes
#' @import data.table
#' @import knitr
#' @import rmarkdown
#' @import dplyr
#' @import sf
#' @import stringr
#' @import measurements
#' @import tmap
#' @import english
#' @examples
#' #--- load experiment made by assign_rates() ---#
#' data(td_single_input)
#' td_single_input
#' \dontrun{
#' make_trial_report(
#'   td_single_input,
#'   "acre",
#'   "imperial",
#'   "Test Trial",
#'   folder_path
#' )
#' }
#'
make_trial_report <- function(td, land_unit, units, trial_name, folder_path = getwd()) {
  all_trial_info <- td %>%
    mutate(land_unit = land_unit) %>%
    mutate(trial_name = trial_name) %>%
    rowwise() %>%
    mutate(field_size = get_field_size(trial_design, land_unit)) %>%
    mutate(plot_number = get_plot_number(trial_design)) %>%
    mutate(plot_length = list(get_plot_length(trial_design, plot_width))) %>%
    mutate(num_harv_pass_in_plot = plot_width / harvester_width) %>%
    mutate(rate_number = get_rate_number(trial_design)) %>%
    mutate(rates = list(get_trial_rates(trial_design))) %>%
    mutate(machines_in_plot = ceiling(plot_width/machine_width)) %>%
    mutate(headland_size = if (units == "metric") {
      headland_length
    } else {
      conv_unit(headland_length, "m", "feet")
    }) %>%
    mutate(sideland_size = if (units == "metric") {
      side_length
    } else {
      conv_unit(side_length, "m", "feet")
    }) %>%
    mutate(map_design = list(
      tm_shape(trial_design) +
        tm_polygons(
          col = "rate",
          title = input_name,
          palette = ifelse(input_name == "seed", "Greens", "Greys")
        )
    )) %>%
    mutate(trial_design = list(trial_design %>%
      mutate(area = as.numeric(st_area(.))) %>%
      mutate(type = case_when(
        type == "headland" ~ "Border Buffer",
        type <= "experiment" ~ "Trial Area"
      )))) %>%
    mutate(map_headlands = list(
      tm_shape(trial_design) +
        tm_polygons(
          col = "type",
          title = "Type of Field Area",
          palette = c("red", "grey")
        )
    ))

  plots = get_plots(all_trial_info)

  if (nrow(td) == 1) {
    machine_table <- data.table(
      width = c(td$harvester_width[1], td$machine_width),
      machine_type = c("harvester", ifelse(td$input_name == "seed", "planter", "applicator")),
      ab_line = list(td$harvest_ab_lines[[1]][1, ], td$ab_lines[[1]])
    ) %>%
      mutate(height = max(width) / 4) %>%
      .[, machine_type := factor(machine_type, levels = c("applicator", "planter", "harvester"))] %>%
      setorder(., cols = "machine_type") %>%
      mutate(machine_id = row_number()) %>%
      rowwise() %>%
      mutate(number_in_plot = c(max(all_trial_info$num_harv_pass_in_plot), all_trial_info$machines_in_plot)) %>%
      mutate(trial_plot = list(plots)) %>%
      mutate(move_vec = list(get_move_vec(ab_line))) %>%
      mutate(center = list(find_center(ab_line, number_in_plot, trial_plot, move_vec, machine_id, width, height))) %>%
      mutate(machine_poly = list(make_machine_polygon(width, height, center, move_vec, st_crs(trial_plot)))) %>%
      mutate(map_ab = list(tmap_abline(ab_line, machine_type, trial_plot))) %>%
      mutate(map_poly = list(tmap_machine(machine_poly, machine_type, trial_plot))) %>%
      mutate(map_label = list(tmap_label(center, machine_type, trial_plot))) %>%
      mutate(map_plot = list(tmap_plot(trial_plot))) %>%
      mutate(plot_legend = list(tmap_plot_legend(trial_plot)))

  } else {
    machine_table <- data.table(
      width = c(td$harvester_width[1], td$machine_width),
      machine_type = c("harvester", ifelse(td$input_name == "seed", "planter", "applicator")),
      ab_line = list(td$harvest_ab_lines[[1]][1, ], td$ab_lines[[1]], td$ab_lines[[2]])
    ) %>%
      mutate(number_in_plot = c(max(all_trial_info$num_harv_pass_in_plot), all_trial_info$machines_in_plot)) %>%
      mutate(height = max(width) / 4) %>%
      .[, machine_type := factor(machine_type, levels = c("planter", "applicator", "harvester"))] %>%
      setorder(., cols = "machine_type") %>%
      mutate(machine_id = row_number()) %>%
      rowwise() %>%
      mutate(trial_plot = list(plots)) %>%
      mutate(move_vec = list(get_move_vec(ab_line))) %>%
      mutate(center = list(find_center(ab_line, number_in_plot, trial_plot, move_vec, machine_id, width, height))) %>%
      mutate(machine_poly = list(make_machine_polygon(width, height, center, move_vec, st_crs(trial_plot)))) %>%
      mutate(map_ab = list(tmap_abline(ab_line, machine_type, trial_plot))) %>%
      mutate(map_poly = list(tmap_machine(machine_poly, machine_type, trial_plot))) %>%
      mutate(map_label = list(tmap_label(center, machine_type, trial_plot))) %>%
      mutate(map_plot = list(tmap_plot(trial_plot))) %>%
      mutate(plot_legend = list(tmap_plot_legend(trial_plot)))
  }

  dir.create(file.path(folder_path, "ofpe_temp_folder"))

  saveRDS(all_trial_info, file.path(folder_path, "ofpe_temp_folder", "all_trial_info.rds"))
  saveRDS(machine_table, file.path(folder_path, "ofpe_temp_folder", "machine_table.rds"))

  # /*=================================================*/
  #' # Rmd
  # /*=================================================*/
  td_rmd <-
    readLines(if (nrow(all_trial_info) > 1) {
      system.file("rmdtemplate", "make-trial-design-template-one-input.Rmd", package = "ofpetrial")
    } else {
      system.file("rmdtemplate", "make-trial-design-template-two-inputs.Rmd", package = "ofpetrial")
    }) %>%
    gsub("_all-trial-info-here_", file.path(folder_path, "ofpe_temp_folder", "all_trial_info.rds"), .) %>%
    gsub("_machine-table-here_", file.path(folder_path, "ofpe_temp_folder", "machine_table.rds"), .) %>%
    gsub("_trial-name_", all_trial_info$trial_name[[1]], .) %>%
    gsub("_length-unit_", ifelse(units == "metric", "meter", "foot"), .) %>%
    gsub("_land-unit_", land_unit, .) %>%
    gsub("_field-size_", all_trial_info$field_size[[1]], .) %>%
    gsub("_headland-size_", all_trial_info$headland_size[[1]], .) %>%
    gsub("_sideland-size_", all_trial_info$sideland_size[[1]], .)

  # /*=================================================*/
  #' # Wrapping up
  # /*=================================================*/
  td_file_name <- file.path(folder_path, "trial_design_report.Rmd")

  writeLines(td_rmd, con = td_file_name)

  #--- render ---#
  render(input = td_file_name, output_file = file.path(folder_path, "trial_design_report.html"))

  unlink(file.path(folder_path, "ofpe_temp_folder"), recursive = TRUE)

  viewer <- getOption("viewer")
  viewer(file.path(folder_path, "trial_design_report.html"))
}

# !==================-=========================================
# ! Helper internal functions
# !===========================================================
#* +++++++++++++++++++++++++++++++++++
#* Creating text for trial design report
#* +++++++++++++++++++++++++++++++++++

text_plot_num_length <- function(all_trial_info, units) {
  if (nrow(all_trial_info) == 1) {
    if (units == "metric") {
      paste0(
        all_trial_info$plot_number[[1]], " rectangular plots, each ", all_trial_info$plot_width[[1]], " meters wide and between ",
        all_trial_info$plot_length[[1]][1], " and ", all_trial_info$plot_length[[1]][2], "meters long."
      )
    } else {
      paste0(
        all_trial_info$plot_number[[1]], " rectangular plots, each ", conv_unit(all_trial_info$plot_width[[1]], "m", "ft"), " feet wide and between ",
        all_trial_info$plot_length[[1]][1], " and ", all_trial_info$plot_length[[1]][2], " feet long."
      )
    }
  } else {
    if (all_trial_info$plot_number[[1]] == all_trial_info$plot_number[[2]]) {
      if (units == "metric") {
        paste0(
          all_trial_info$plot_number[[1]], " rectangular plots, each ", all_trial_info$plot_width[[1]], " meters wide and between ",
          all_trial_info$plot_length[[1]][1], " and ", all_trial_info$plot_length[[1]][2], "meters long."
        )
      } else {
        paste0(
          all_trial_info$plot_number[[1]], " rectangular plots, each ", conv_unit(all_trial_info$plot_width[[1]], "m", "ft"), " feet wide and between ",
          all_trial_info$plot_length[[1]][1], " and ", all_trial_info$plot_length[[1]][2], " feet long."
        )
      }
    } else {
      if (units == "metric") {
        paste0(
          all_trial_info$plot_number[[1]], " rectangular ", all_trial_info$input_name[[1]], " plots, each ", all_trial_info$plot_width[[1]], " meters wide and ",
          all_trial_info$plot_number[[2]], " rectangular ", all_trial_info$input_name[[2]], " plots, each ", all_trial_info$plot_width[[2]], " meters wide.",
          "The plots are between ",
          all_trial_info$plot_length[[1]][1], " and ", all_trial_info$plot_length[[1]][2], " meters long."
        )
      } else {
        paste0(
          all_trial_info$plot_number[[1]], " rectangular ", all_trial_info$input_name[[1]], " plots, each ", conv_unit(all_trial_info$plot_width[[1]], "m", "ft"), " feet wide and ",
          all_trial_info$plot_number[[2]], " rectangular ", all_trial_info$input_name[[2]], " plots, each ", conv_unit(all_trial_info$plot_width[[2]], "m", "ft"), " feet wide.",
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
      as.character(english(all_trial_info$rate_number)),
      " targeted ", all_trial_info$input_name, " rates."
    )
  } else {
    if (all_trial_info$rate_number[[1]] == all_trial_info$rate_number[[2]]) {
      paste0(as.character(english(all_trial_info$rate_number[[1]])), " targeted ", all_trial_info$input_name[[1]], " and ", all_trial_info$input_name[[2]], " rates.")
    } else {
      paste0(as.character(english(all_trial_info$rate_number[[1]])), " targeted ", all_trial_info$input_name[[1]], " rates and ", as.character(english(all_trial_info$rate_number[[2]])), " targeted ", all_trial_info$input_name[[2]], " rates.")
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

text_plant_apply <- function(machine_table) {
  if (nrow(machine_table) > 2) {
    paste0(ifelse(machine_table$input_name[[1]] == "seed", "plant", "apply"), " and ", ifelse(machine_table$input_name[[2]] == "seed", "plant", "apply"))
  } else {
    paste0(ifelse(machine_table$input_name[[1]] == "seed", "plant", "apply"))
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
    paste0(str_to_title(machine_table$machine_type[[1]]), " and ", str_to_title(machine_table$machine_type[[2]]))
  } else {
    paste0(str_to_title(machine_table$machine_type[[1]]))
  }
}

trial_text_ablines <- function(machine_table) {
  if (nrow(machine_table) > 2) {
    paste0(
      "The AB-lines for the ",
      machine_table$machine_type[[1]], " and ", machine_table$machine_type[[2]],
      " are ",
      ifelse(identical(machine_table$ab_line[[1]], machine_table$ab_line[[2]]) == TRUE, "identical due to the machinery specifications.", "not identical due to the difference in machine specifications.")
    )
  } else {
    ""
  }
}

trial_text_machine_sizes_and_plot_width <- function(machine_table, all_trial_info, units) {
  if (units == "metric") {
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
          conv_unit(machine_table$width[[1]], "m", "ft"), "-foot ",
          machine_table$machine_type[[1]],
          ", ",
          conv_unit(machine_table$width[[2]], "m", "ft"), "-foot ",
          machine_table$machine_type[[2]],
          ", and ",
          conv_unit(all_trial_info$plot_width[[1]], "m", "ft"), "-foot ",
          all_trial_info$input_name[[1]], " and ", all_trial_info$input_name[[2]], " plots."
        )
      } else {
        paste0(
          conv_unit(machine_table$width[[1]], "m", "ft"), "-foot ",
          machine_table$machine_type[[1]],
          ", ",
          conv_unit(machine_table$width[[2]], "m", "ft"), "-foot ",
          machine_table$machine_type[[2]],
          " and ",
          conv_unit(all_trial_info$plot_width[[1]], "m", "ft"), "-foot ", all_trial_info$input_name[[1]], " plots and ",
          conv_unit(all_trial_info$plot_width[[2]], "m", "ft"), "-foot ", all_trial_info$input_name[[2]], " plots."
        )
      }
    } else {
      paste0(
        conv_unit(machine_table$width[[1]], "m", "ft"),
        "-foot ",
        machine_table$machine_type[[1]],
        " and ",
        conv_unit(all_trial_info$plot_width[[1]], "m", "ft"), "-foot plots."
      )
    }
  }
}

text_plot_width <- function(all_trial_info, units) {
  if (nrow(all_trial_info) == 1) {
    if (units == "metric") {
      paste0(all_trial_info$plot_width[[1]], "-meter plots.")
    } else {
      paste0(conv_unit(all_trial_info$plot_width[[1]], "m", "ft"), "-foot plots.")
    }
  } else {
    if (all_trial_info$plot_width[[1]] == all_trial_info$plot_width[[2]]) {
      if (units == "metric") {
        paste0(all_trial_info$plot_width[[1]], "-meter ", all_trial_info$input_name[[1]], " and ", all_trial_info$input_name[[2]], " plots.")
      } else {
        paste0(conv_unit(all_trial_info$plot_width[[1]], "m", "ft"), "-foot ", all_trial_info$input_name[[1]], " and ", all_trial_info$input_name[[2]], " plots.")
      }
    } else {
      if (units == "metric") {
        paste0(all_trial_info$plot_width[[1]], "-meter ", all_trial_info$input_name[[1]], " plots and ", all_trial_info$plot_width[[2]], "-meter ", all_trial_info$input_name[[2]], " plots.")
      } else {
        paste0(conv_unit(all_trial_info$plot_width[[1]], "m", "ft"), "-foot ", all_trial_info$input_name[[1]], " plots and ", conv_unit(all_trial_info$plot_width[[2]], "m", "ft"), "-foot ", all_trial_info$input_name[[2]], " plots.")
      }
    }
  }
}

text_harvester_passes <- function(all_trial_info, units) {
  if (nrow(all_trial_info) > 1) {
    if (all_trial_info$plot_width[[1]] == all_trial_info$plot_width[[2]]) {
      if (units == "metric") {
        paste0(
          as.character(english(all_trial_info$num_harv_pass_in_plot[[1]])),
          " ",
          all_trial_info$harvester_width[[1]],
          "-meter harvester swath",
          if (all_trial_info$num_harv_pass_in_plot[[1]] > 1) {
            "s"
          } else {
            ""
          },
          " will lie neatly within each ",
          text_plot_width(all_trial_info, units)
        )
      } else {
        paste0(
          as.character(english(all_trial_info$num_harv_pass_in_plot[[1]])),
          " ",
          conv_unit(all_trial_info$harvester_width[[1]], "m", "ft"),
          "-foot harvester swath",
          if (all_trial_info$num_harv_pass_in_plot[[1]] > 1) {
            "s"
          } else {
            ""
          },
          " will lie neatly within each ",
          text_plot_width(all_trial_info, units)
        )
      }
    } else {
      if (units == "metric") {
        paste0(
          as.character(english(all_trial_info$num_harv_pass_in_plot[[1]])),
          " ",
          all_trial_info$harvester_width[[1]],
          "-meter harvester swath",
          if (all_trial_info$num_harv_pass_in_plot[[1]] > 1) {
            "s"
          } else {
            ""
          },
          " and ",
          as.character(english(all_trial_info$num_harv_pass_in_plot[[2]])),
          " ",
          all_trial_info$harvester_width[[2]],
          "-meter harvester swath",
          if (all_trial_info$num_harv_pass_in_plot[[2]] > 1) {
            "s"
          } else {
            ""
          },
          " will lie neatly within each ",
          text_plot_width(all_trial_info, units),
          ", respectively"
        )
      } else {
        paste0(
          as.character(english(all_trial_info$num_harv_pass_in_plot[[1]])),
          " ",
          conv_unit(all_trial_info$harvester_width[[1]], "m", "ft"),
          "-foot harvester swath",
          if (all_trial_info$num_harv_pass_in_plot[[1]] > 1) {
            "s"
          } else {
            ""
          },
          " and ",
          as.character(english(all_trial_info$num_harv_pass_in_plot[[2]])),
          " ",
          conv_unit(all_trial_info$harvester_width[[2]], "m", "ft"),
          "-foot harvester swath",
          if (all_trial_info$num_harv_pass_in_plot[[2]] > 1) {
            "s"
          } else {
            ""
          },
          " will lie neatly within each ",
          text_plot_width(all_trial_info, units),
          ", respectively"
        )
      }
    }
  } else {
    if (units == "metric") {
      paste0(
        as.character(english(all_trial_info$num_harv_pass_in_plot[[1]])),
        " ",
        all_trial_info$harvester_width[[1]],
        "-meter harvester swath",
        if (all_trial_info$num_harv_pass_in_plot[[1]] > 1) {
          "s"
        } else {
          ""
        },
        " will lie neatly within each ",
        text_plot_width(all_trial_info, units)
      )
    } else {
      paste0(
        as.character(english(all_trial_info$num_harv_pass_in_plot[[1]])),
        " ",
        conv_unit(all_trial_info$harvester_width[[1]], "m", "ft"),
        "-foot harvester swath",
        if (all_trial_info$num_harv_pass_in_plot[[1]] > 1) {
          "s"
        } else {
          ""
        },
        " will lie neatly within each ",
        text_plot_width(all_trial_info, units)
      )
    }
  }
}

get_field_size <- function(trial_design, land_unit) {
  trial_design %>%
    mutate(area = as.numeric(st_area(.))) %>%
    pull(area) %>%
    sum(.) %>%
    conv_unit(., "m2", land_unit) %>%
    round(.)
}

get_plot_number <- function(trial_design) {
  trial_design %>%
    filter(type == "experiment") %>%
    nrow()
}

get_plot_length <- function(trial_design, plot_width) {
  trial_design %>%
    mutate(area = as.numeric(st_area(.)) / plot_width) %>%
    pull(area) %>%
    quantile(., c(0.1, 0.9)) %>%
    round(.)
}

get_rate_number <- function(trial_design) {
  trial_design %>%
    pull(rate) %>%
    unique(.) %>%
    length(.)
}

get_trial_rates <- function(trial_design) {
  trial_design %>%
    pull(rate) %>%
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

get_move_vec <- function(ab_line) {
  lags <- st_coordinates(ab_line) %>%
    data.frame() %>%
    mutate(
      dx = X - lag(X, n = 1),
      dy = Y - lag(Y, n = 1)
    )

  return(c(lags$dx[2], lags$dy[2]))
}

find_center <- function(ab_line, number_in_plot, plot, move_vec, machine_id, machine_width, height) {
  normalized_move_vec <- move_vec / sqrt(sum(move_vec^2)) # normalized direction aka normal vector
  perp_move_vec <- rotate_vec(normalized_move_vec, 90) # perpendicular vector to the normal vector

  # intersect the ab_line and plot polygon
  line_coords <- st_intersection(plot, ab_line) %>%
    st_coordinates()
  cent <- line_coords[1, 1:2] %>%
    matrix(., nrow = 1, ncol = 2)

  cent <- cent + normalized_move_vec * (height * 2.2) * (machine_id)

  if (number_in_plot > 1) {
    for (i in (2:number_in_plot)) {
      cent_2 <- st_shift(
        st_point(cent[i - 1, ]) %>% st_sfc() %>% st_sf(),
        if ((normalized_move_vec[1] > 0) |
          (normalized_move_vec[1] == 0 & normalized_move_vec[2] > 0)) {
          perp_move_vec * machine_width
        } else {
          -perp_move_vec * machine_width
        }
      ) %>%
        st_coordinates()

      cent <- rbind(cent, cent_2)
    }
  }

  return(cent)
}

get_plots <- function(all_trial_info){
  if (length(all_trial_info$plot_width %>% unique()) == 1){
    design <- all_trial_info$trial_design[[1]] %>%
      mutate(plot_id = row_number()) %>%
      filter(type == "Trial Area")

    first_plot <- all_trial_info$trial_design[[1]][1, ] %>%
      mutate(plot_id = st_intersection(st_transform_utm(design), all_trial_info$ab_lines[[1]]) %>%
               pull(plot_id) %>%
               min(.))

    plots <- design %>%
      filter(plot_id == first_plot$plot_id) %>%
      st_transform_utm(.) %>%
      mutate(input_name = input_name) %>%
      dplyr::select(rate, strip_id, plot_id, type, input_name)

  }else{
    max_input <- all_trial_info %>%
      filter(plot_width == max(all_trial_info$plot_width))

    min_input <- all_trial_info %>%
      filter(plot_width != max(all_trial_info$plot_width))

    design1 <- max_input$trial_design[[1]] %>%
      mutate(plot_id = row_number()) %>%
      filter(type == "Trial Area")

    design2 <- min_input$trial_design[[1]] %>%
      mutate(plot_id = row_number()) %>%
      filter(type == "Trial Area")

    plot1 <- design %>%
      filter(plot_id == st_intersection(st_transform_utm(design1), max_input$ab_lines[[1]]) %>%
               pull(plot_id) %>%
               min(.)) %>%
      st_transform_utm(.) %>%
      mutate(input_name = max_input$input_name) %>%
      dplyr::select(rate, strip_id, plot_id, type, input_name)

    plot2 = st_intersection(st_transform_utm(design2), plot1) %>%
      mutate(area = st_area(.)) %>%
      filter(area >= median(area)) %>%
      st_transform_utm(.) %>%
      mutate(input_name = min_input$input_name) %>%
      dplyr::select(rate, strip_id, plot_id, type, input_name)

    plots = rbind(plot1, plot2)

  }

  return(plots)
}

tmap_abline <- function(ab_line, machine_type, trial_plot) {
  tm_shape(ab_line, bbox = st_bbox(trial_plot)) +
    tm_lines(
      col = if (machine_type == "planter") {
        "#009E73"
      } else if (machine_type == "applicator") {
        "#0072B2"
      } else {
        "#E69F00"
      },
      lty = if (machine_type == "applicator") {
        "solid"
      } else {
        "dashed"
      }
    )
}

tmap_machine <- function(machine_poly, machine_type, trial_plot) {
  tm_shape(machine_poly, bbox = st_bbox(trial_plot)) +
    tm_borders(col = if (machine_type == "planter") {
      "#009E73"
    } else if (machine_type == "applicator") {
      "#0072B2"
    } else {
      "#E69F00"
    }, lwd = 3) +
    tm_fill(col = "white")
}

# test_plot <- machine_table$trial_plot[[1]]
tmap_plot <- function(trial_plot) {
  if (trial_plot %>%
    mutate(area = as.numeric(st_area(.))) %>%
    pull(area) %>%
    unique(.) %>%
    length() == 1){

    map <- tm_shape(trial_plot, bbox = st_bbox(trial_plot)) +
      tm_borders(col = "black")
  }else{
    plots <- trial_plot %>%
        mutate(area = as.numeric(st_area(.))) %>%
        arrange(desc(area))

    map_small_plots <- list()
    for (i in 2:nrow(plots)) {
      map_small_plots[[i]] <- paste0("tm_shape(plots[", i, ",], bbox = st_bbox(plots)) +
        tm_borders(col = \"gray\", lwd = 2, lty = \"dashed\")")

    }


    map <- tm_shape(plots[1,], bbox = st_bbox(plots)) +
      tm_borders(col = "black", lwd = 5) +
      tm_fill(col = "white") +
      eval(parse(text = paste0(map_small_plots, collapse = " + ")))

  }

  return(map)
}

# trial_plot <- machine_table$trial_plot[[1]]
tmap_plot_legend <- function(trial_plot) {
  if (trial_plot %>%
      mutate(area = as.numeric(st_area(.))) %>%
      pull(area) %>%
      unique(.) %>%
      length() == 1){

    legend = tm_add_legend(
      type = "symbol",
      labels = c("Trial Plot"),
      col = c("black"),
      shape = 0
    )
  }else{
    plots <- trial_plot %>%
      mutate(area = as.numeric(st_area(.))) %>%
      arrange(desc(area)) %>%
      pull(input_name) %>%
      unique()

    legend = tm_add_legend(
      type = "symbol",
      labels = c(paste0(str_to_title(plots[1]), " Trial Plot")),
      col = c("black"),
      shape = 0
    ) +
      tm_add_legend(
        type = "symbol",
        labels = c(paste0(str_to_title(plots[2]), " Trial Plot")),
        col = c("gray"),
        shape = 0
      )
  }
  return(legend)
}

tmap_label <- function(center, machine_type, trial_plot) {
  labels <- list()
  for (i in 1:nrow(center)) {
    labels[[i]] <- paste0("tm_shape(st_point(center[", i, ", ]) %>% st_sfc(crs = st_crs(trial_plot)) %>% st_sf() %>% mutate(label = if(machine_type == \"planter\"){\"P\"}else if(machine_type == \"applicator\"){\"A\"}else{\"H\"}), bbox = st_bbox(trial_plot)) + tm_text(\"label\")")
  }
  tmap_label <- eval(parse(text = paste0(labels, collapse = " + ")))

  return(tmap_label)
}

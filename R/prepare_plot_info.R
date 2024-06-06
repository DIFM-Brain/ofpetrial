#!===========================================================
# ! Exported functions
# !===========================================================
#' Prepare plot information for a single-input experiment (length in meter)
#'
#' Prepare plot information for a single-input experiment case. All the length values need to be specified in meter.
#'
#' @param input_name (character) Input name
#' @param unit_system (character) A character of either 'metric' or 'imperial' indicating the system of measurement used
#' @param machine_width (numeric) A numeric number in units specified in unit_system that indicates the width of the applicator or planter of the input
#' @param section_num (numeric) A numeric number that indicates the number of sections of the applicator or planter of the input
#' @param harvester_width (numeric) A numeric number that indicates the width of the harvester
#' @param plot_width (numeric) Default is c(NA, NA).
#' @param headland_length (numeric) A numeric number that indicates the length of the headland (how long the non-experimental space is in the direction machines drive). Default is NA.
#' @param side_length (numeric) A numeric number that indicates the length of the two sides of the field (how long the non-experimental space is in the direction perpendicular to the direction of machines). Default is NA.
#' @param max_plot_width (numeric) Maximum width of the plots. Default is 36.576 meter (120 feet).
#' @param min_plot_length (numeric) Minimum length of the plots. Default is 73.152 meter (240 feet).
#' @param max_plot_length (numeric) Maximum length of the plots. Default is 91.440 meter (300 feet)
#' @returns a tibble with plot information necessary to create experiment plots
#' @import data.table
#' @export
#' @examples
#' input_name <- "seed"
#' unit_system <- "metric"
#' machine_width <- 12
#' section_num <- 12
#' plot_width <- NA
#' harvester_width <- 24
#' prep_plot(input_name, unit_system, machine_width, section_num, harvester_width)
#'
prep_plot <- function(input_name,
                      unit_system,
                      machine_width,
                      section_num,
                      harvester_width,
                      plot_width = NA,
                      headland_length = NA,
                      side_length = NA,
                      max_plot_width = NA,
                      min_plot_length = NA,
                      max_plot_length = NA) {
  #--- dimension check ---#
  fms_ls <- c(length(input_name), length(machine_width), length(section_num), length(plot_width))
  if (any(fms_ls != 1)) {
    stop("Inconsistent numbers of elements in input_name, machine_width, section_num, and plot_width. Check if all of them have a single element.")
  }

  section_width <- machine_width / section_num

  #++++++++++++++++++++++++++++++++++++
  #+ Setting plot width and length
  #++++++++++++++++++++++++++++++++++++
  if (is.na(max_plot_width)) {
    max_plot_width <-
      ifelse(
        unit_system == "imperial",
        120,
        conv_unit(120, "feet", "meters") # 36.4576 meter
      )
  }
  if (is.na(min_plot_length)) {
    min_plot_length <-
      ifelse(
        unit_system == "imperial",
        240,
        conv_unit(240, "feet", "meters") # 73.152 meter
      )
  }
  if (is.na(max_plot_length)) {
    max_plot_length <-
      ifelse(
        unit_system == "imperial",
        300,
        conv_unit(300, "feet", "meters") # 91.440 meter
      )
  }
  if (is.na(side_length)) {
    # side length needs to be at least 30 feet
    side_length <-
      max(
        section_width,
        ifelse(
          unit_system == "imperial",
          30,
          conv_unit(30, "feet", "meters")
        )
      )
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Check and notify the mixed treatment problems (with potential suggestions)
  #++++++++++++++++++++++++++++++++++++
  lcm_found <- !is.na(get_lcm(section_width, harvester_width, max_plot_width))

  proposed_plot_width <- find_plotwidth(section_width, harvester_width, max_plot_width)

  if (is.na(plot_width)) {
    plot_width <- proposed_plot_width
    if (lcm_found) {
      message(
        "Since plot width was not provided via the `plot_with` option`, it was set to a least common multiplier of the widths of the machines."
      )
    } else {
      message(
        paste0(
          "Since plot width was not provided via the `plot_with` option`, we searched for a least common multiplier of the widths of the machines. Unfortunately, there is no such plot width. This means that you will inevitably face mixed-treatment problems to a certain extent. The plot width of ", proposed_plot_width, " we selected ensures that at least one harvest path within the path of ", input_name, " does not have the problems."
        )
      )
    }
  } else {
    if (plot_width %% section_width != 0) {
      warning_message <- "Please note that the plot width specified is not a multiple of the machine (section) width. This means that you will inevitably face mixed-treatment problems to a certain extent."
    }
    warning_message <- NULL
    if (lcm_found & plot_width %% proposed_plot_width == 0 & proposed_plot_width < plot_width) {
      warning_message <-
        paste0(
          "For ", input_name, ", there is a plot width that is smaller than the plot width you suggested and avoids mixed treatement problem. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
    } else if (lcm_found & plot_width %% proposed_plot_width != 0) {
      warning_message <-
        paste0(
          "For ", input_name, ", the plot width you specified would cause mixed treatment problems. However, there is a plot width that avoids them. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
    } else if (!lcm_found & plot_width != proposed_plot_width) {
      warning_message <-
        paste0(
          "For ", input_name, ", the plot width you specified would cause mixed treatment problems. Unfortunately, there is no plot width that avoids them. Plot width of ", proposed_plot_width, " ensures that at least one harvest path within the path of ", input_name, " does not have the problems."
        )
    }
    #--- notify the user of potential problems and improvements ---#
    message(warning_message)
  }


  #++++++++++++++++++++++++++++++++++++
  #+ Headland and side lengths
  #++++++++++++++++++++++++++++++++++++
  #--- head distance ---#
  if (is.na(headland_length)) {
    headland_length <- 2 * machine_width
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Put together the data
  #++++++++++++++++++++++++++++++++++++

  if (unit_system == "metric") {
    plot_data <-
      tibble::tibble(
        input_name = input_name,
        unit_system = unit_system,
        machine_width = machine_width,
        section_num = section_num,
        section_width = section_width,
        harvester_width = harvester_width,
        plot_width = plot_width,
        headland_length = headland_length,
        side_length = side_length,
        min_plot_length = min_plot_length,
        max_plot_length = max_plot_length
      )
  } else {
    plot_data <-
      tibble::tibble(
        input_name = input_name,
        unit_system = unit_system,
        machine_width = conv_unit(machine_width, "feet", "meters"),
        section_num = section_num,
        section_width = conv_unit(section_width, "feet", "meters"),
        harvester_width = conv_unit(harvester_width, "feet", "meters"),
        plot_width = conv_unit(plot_width, "feet", "meters"),
        headland_length = conv_unit(headland_length, "feet", "meters"),
        side_length = conv_unit(side_length, "feet", "meters"),
        min_plot_length = conv_unit(min_plot_length, "feet", "meters"),
        max_plot_length = conv_unit(max_plot_length, "feet", "meters")
      )
  }

  return(plot_data)
}

# !===========================================================
# ! Helper functions
# !===========================================================
find_plotwidth <- function(section_width, harvester_width, max_plot_width) {
  #--- least common multiple (lcm) ---#
  # lcm would return max_plot_width + 1 if it cannot find lcm below max_plot_width
  plot_width <- get_lcm(section_width, harvester_width, max_plot_width)

  #--- if the lcm is not found within max_plot_width ---#
  if (is.na(plot_width)) {
    width_ratio <- section_width / harvester_width

    if (width_ratio == 1) {
      plot_width <- harvester_width
    } else if (1 < width_ratio & width_ratio < 2) {
      plot_width <- 2 * section_width
    } else if (width_ratio == 2) {
      plot_width <- harvester_width
    } else if (2 < width_ratio) {
      plot_width <- section_width
    } else if (width_ratio < 1) {
      plot_width <- ceiling(2 / width_ratio) * section_width
    }
  }

  return(plot_width)
}

#--- find the least common multiple of two numbers ---#
get_lcm <- function(section_width, harvester_width, max_plot_width) {
  lcm <- NA
  greater <- max(section_width, harvester_width)
  lcm_ls <- seq(greater, max_plot_width, greater)

  get_abs_dif <- function(a, b) {
    quotient <- a %/% b
    pmin(abs(a - quotient * b), abs(a - (quotient + 1) * b))
  }

  dif_section <- get_abs_dif(lcm_ls, section_width)
  dif_harvest <- get_abs_dif(lcm_ls, harvester_width)

  #--- as long as the difference is less 0.05 ---#
  lcm_true <- (dif_section <= 0.05) & (dif_harvest <= 0.05)

  if (any(lcm_true)) {
    lcm <- lcm_ls[min(which(lcm_true))]
  }

  # #++++++++++++++++++++++++++++++++++++
  # #+ Second try with looser condition
  # #++++++++++++++++++++++++++++++++++++
  # if (is.na(lcm)) {
  #   pmin((lcm_ls %% section_width), (abs((lcm_ls %% section_width) - section_width)))

  #   lcm_approx_true <- ((lcm_ls %% section_width) < 0.03 * harvester_width) & (lcm_ls %% harvester_width < 0.03 * harvester_width)

  #   if (any(lcm_approx_true)) {
  #     lcm <- lcm_ls[min(which(lcm_approx_true))]
  #   }
  # }

  return(lcm)
}

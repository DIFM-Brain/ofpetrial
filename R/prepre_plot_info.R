#!===========================================================
# ! Exported functions
# !===========================================================
#' Prepare plot information for a single-input experiment (length in meter)
#'
#' Prepare plot information for a single-input experiment case. All the length values need to be specified in meter.
#'
#' @param input_name (character) Input name
#' @param machine_width (numeric) A numeric number in meter that indicates the width of the applicator or planter of the input 
#' @param section_num (numeric) A numeric number that indicates the number of sections of the applicator or planter of the input
#' @param harvester_width (numeric) A numeric number in meter that indicates the width of the harvester
#' @param plot_width (numeric) Default is c(NA, NA).
#' @param headland_length (numeric) A numeric number in meter that indicates the length of the headland (how long the non-experimental space is in the direction machines drive). Default is NA.
#' @param side_length (numeric) A numeric number in meter that indicates the length of the two sides of the field (how long the non-experimental space is in the direction perpendicular to the direction of machines). Default is NA.
#' @param max_plot_width (numeric) Maximum width of the plots in meter. Default is 36.576 meter (120 feet).
#' @param min_plot_length (numeric) Minimum length of the plots in meter. Default is 73.152 meter (240 feet).
#' @param max_plot_length (numeric) Maximum length of the plots in meter. Default is 91.440 meter (300 feet)
#' @returns a tibble with plot information necessary to create experiment plots
#' @import data.table
#' @export
#' @examples
#' input_name <- "seed"
#' machine_width <- 12
#' section_num <- 12
#' plot_width <- NA
#' harvester_width <- 24
#' prep_plot_m(input_name, machine_width, section_num, harvester_width)
#'
prep_plot_m <- function(input_name,
                         machine_width,
                         section_num,
                         harvester_width,
                         plot_width = NA,
                         headland_length = NA,
                         side_length = NA,
                         max_plot_width = measurements::conv_unit(120, "ft", "m"), # 36.4576 meter
                         min_plot_length = measurements::conv_unit(240, "ft", "m"), # 73.152 feet
                         max_plot_length = measurements::conv_unit(300, "ft", "m") # 91.440 meter
) {

  #--- dimension check ---#
  fms_ls <- c(length(input_name), length(machine_width), length(section_num), length(plot_width))
  if (any(fms_ls != 1)) {
    stop("Inconsistent numbers of elements in input_name, machine_width, section_num, and plot_width. Check if all of them have a single element.")
  }

  section_width <- machine_width / section_num

  #++++++++++++++++++++++++++++++++++++
  #+ Check and notify the mixed treatment problems (with potential suggestions)
  #++++++++++++++++++++++++++++++++++++
  lcm_found <- lcm_check(section_width, harvester_width, max_plot_width)
  proposed_plot_width <- find_plotwidth(section_width, harvester_width, max_plot_width)

  if (is.na(plot_width)) {
    plot_width <- proposed_plot_width
  } else {
    if (plot_width %% section_width != 0) {
      stop("Plot width provided is not a multiple of the machine (section) width.")
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

  #--- side distance ---#
  if (is.na(side_length)) {
    side_length <- max(section_width, measurements::conv_unit(30, "ft", "m"))
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Put together the data
  #++++++++++++++++++++++++++++++++++++
  plot_data <-
    tibble::tibble(
      input_name = input_name,
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
  return(plot_data)
}

#' Prepare plot information for a single-input experiment (feet)
#'
#' Prepare plot information for a single-input experiment case. All the length values need to be specified in feet.
#'
#' @param input_name (character) Input name
#' @param machine_width (numeric) A numeric number in feet that indicates the width of the applicator or planter of the input 
#' @param section_num (numeric) A numeric number that indicates the number of sections of the applicator or planter of the input
#' @param harvester_width (numeric) A numeric number in feet that indicates the width of the harvester
#' @param plot_width (numeric) Default is c(NA, NA).
#' @param headland_length (numeric) A numeric number in feet that indicates the length of the headland (how long the non-experimental space is in the direction machines drive). Default is NA.
#' @param side_length (numeric) A numeric number in feet that indicates the length of the two sides of the field (how long the non-experimental space is in the direction perpendicular to the direction of machines). Default is NA.
#' @param max_plot_width (numeric) Maximum width of the plots in feet. Default is (36.576 meter).
#' @param min_plot_length (numeric) Minimum length of the plots in feet. Default is 240 feet (73.152 meter).
#' @param max_plot_length (numeric) Maximum length of the plots in feet. Default is 300 feet (91.440 meter).
#' @returns a tibble with plot information necessary to create experiment plots
#' @import data.table
#' @export
#' @examples
#' input_name <- "seed"
#' machine_width <- 60
#' section_num <- 24
#' plot_width <- NA
#' harvester_width <- 30
#' prep_plot_f(input_name, machine_width, section_num, harvester_width)
#'
prep_plot_f <- function(input_name,
                         machine_width,
                         section_num,
                         harvester_width,
                         plot_width = NA,
                         headland_length = NA,
                         side_length = NA,
                         max_plot_width = 120, # 120 feet
                         min_plot_length = 240, # 240 feet
                         max_plot_length = 300 # 300 feet
) {

  #--- dimension check ---#
  fms_ls <- c(length(input_name), length(machine_width), length(section_num), length(plot_width))
  if (any(fms_ls != 1)) {
    stop("Inconsistent numbers of elements in input_name, machine_width, section_num, and plot_width. Check if all of them have a single element.")
  }

  section_width <- machine_width / section_num

  #++++++++++++++++++++++++++++++++++++
  #+ Check and notify the mixed treatment problems (with potential suggestions)
  #++++++++++++++++++++++++++++++++++++
  lcm_found <- lcm_check(section_width, harvester_width, max_plot_width)
  proposed_plot_width <- find_plotwidth(section_width, harvester_width, max_plot_width)

  if (is.na(plot_width)) {
    plot_width <- proposed_plot_width
  } else {
    if (plot_width %% section_width != 0) {
      stop("Plot width provided is not a multiple of the machine (section) width.")
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

  #--- side distance ---#
  if (is.na(side_length)) {
    side_length <- max(section_width, 30)
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Put together the data
  #++++++++++++++++++++++++++++++++++++
  plot_data <-
    tibble::tibble(
      input_name = input_name,
      machine_width = measurements::conv_unit(machine_width, "ft", "m"),
      section_num = section_num,
      section_width = measurements::conv_unit(section_width, "ft", "m"),
      harvester_width = measurements::conv_unit(harvester_width, "ft", "m"),
      plot_width = measurements::conv_unit(plot_width, "ft", "m"),
      headland_length = measurements::conv_unit(headland_length, "ft", "m"),
      side_length = measurements::conv_unit(side_length, "ft", "m"),
      min_plot_length = measurements::conv_unit(min_plot_length, "ft", "m"),
      max_plot_length = measurements::conv_unit(max_plot_length, "ft", "m")
    )
  return(plot_data)
}

# !===========================================================
# ! Helper functions
# !===========================================================
find_plotwidth <- function(section_width, harvester_width, max_plot_width) {
  #--- least common multiple (lcm) ---#
  plot_width <- lcm(section_width, harvester_width)

  #--- if the lcm is greater than the maximum plot width ---#
  if (plot_width > max_plot_width) {
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

lcm_check <- function(section_width, harvester_width, max_plot_width) {
  plot_width <- lcm(section_width, harvester_width)

  #--- if the lcm is greater than the maximum plot width ---#
  if (plot_width <= max_plot_width) {
    lcm_found <- TRUE
  } else {
    lcm_found <- FALSE
  }

  return(lcm_found)
}

#--- find the least common multiple of two numbers ---#
lcm <- function(x, y) {
  if (x > y) {
    greater <- x
  } else {
    greater <- y
  }

  while (TRUE) {
    if ((greater %% x == 0) && (greater %% y == 0)) {
      lcm <- greater
      break
    }
    greater <- greater + 1
  }
  return(lcm)
}

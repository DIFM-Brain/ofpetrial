#!===========================================================
# ! Exported functions
# !===========================================================
#' Prepare plot information for a two-input experiment (meter)
#'
#' Prepare plot information for a two-input experiment case. All the length values need to be specified in meter.
#'
#' @param form (vector of two elements)
#' @param machine_width (vector of two elements)
#' @param section_num (vector of two elements)
#' @param harvester_width (numeric)
#' @param plot_width (vector of two elements) Default is c(NA, NA)
#' @param headland_length (numeric) Default is NA.
#' @param side_length (numeric) Default is NA.
#' @param max_plot_width (numeric) Default is 36.576 meter (120 feet).
#' @param max_plot_length (numeric) Default is 73.152 meter (240 feet).
#' @param min_plot_length (numeric) Default is 79.248 meter (260 feet).
#' @returns a tibble with plot information necessary to create experiment plots
#' @import data.table
#' @export
#' @examples
#' form <- c("seed", "NH3")
#' machine_width <- c(12, 9)
#' section_num <- c(12, 1)
#' plot_width <- c(9.5, 36)
#' harvester_width <- 12
#' prep_plot_md(form, machine_width, section_num, harvester_width, plot_width)
#'
prep_plot_md <- function(form,
                         machine_width,
                         section_num,
                         harvester_width,
                         plot_width = c(NA, NA),
                         headland_length = NA,
                         side_length = NA,
                         max_plot_width = measurements::conv_unit(120, "ft", "m"), # 36.4576 meter
                         min_plot_length = measurements::conv_unit(240, "ft", "m"), # 73.152 feet
                         max_plot_length = measurements::conv_unit(260, "ft", "m") # 79.248 meter
) {

  #--- dimension check ---#
  fms_ls <- c(length(form), length(machine_width), length(section_num), length(plot_width))
  if (any(fms_ls != 2)) {
    stop("Inconsistent numbers of elements in form, machine_width, section_num, and plot_width. Check if all of them have two elements.")
  }

  section_width <- machine_width / section_num

  plot_data <-
    data.frame(
      form = form,
      machine_width = machine_width,
      section_num = section_num,
      harvester_width = harvester_width,
      plot_width = plot_width
    ) %>%
    dplyr::mutate(section_width = machine_width / section_num) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      lcm_found =
        lcm_check(section_width, harvester_width, max_plot_width)
    ) %>%
    dplyr::mutate(
      proposed_plot_width =
        find_plotwidth(section_width, harvester_width, max_plot_width)
    ) %>%
    dplyr::mutate(plot_width = ifelse(is.na(plot_width), proposed_plot_width, plot_width))


  #++++++++++++++++++++++++++++++++++++
  #+ Check and notify the mixed treatment problems (with potential suggestions)
  #++++++++++++++++++++++++++++++++++++
  warning_message <-
    plot_data %>%
    dplyr::mutate(messages = list(
      if (lcm_found & plot_width %% proposed_plot_width == 0 & proposed_plot_width < plot_width) {
        paste0(
          "For ", form, ", there is a plot width that is smaller than the plot width you suggested and avoids mixed treatement problem. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
      } else if (lcm_found & plot_width %% proposed_plot_width != 0) {
        paste0(
          "For ", form, ", the plot width you specified would cause mixed treatment problems. However, there is a plot width that avoids them. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
      } else if (!lcm_found & plot_width != proposed_plot_width) {
        paste0(
          "For ", form, ", the plot width you specified would cause mixed treatment problems. Unfortunately, there is no plot width that avoids them. Plot width of ", proposed_plot_width, " ensures that at least one harvest path within the path of ", form, " does not have the problems."
        )
      } else {
        NULL
      }
    )) %>%
    dplyr::pull(messages)

  #--- notify the user of potential problems and improvements ---#
  message(unlist(warning_message))

  #--- warnd the user that they may have serious mixed treatment problems   ---#
  if (all(!plot_data$lcm_found)) {
    message(paste0(
      "Neither of ", form[1], " and ", form[2], " does not have a plot width without mixed treatment problems. Please consider running experiments with a single input instead of two."
    ))
  }

  #++++++++++++++++++++++++++++++++++++
  #+ head and side lengths
  #++++++++++++++++++++++++++++++++++++
  #--- head distance ---#
  if (is.na(headland_length)) {
    headland_length <- 2 * max(machine_width)
  }

  #--- side distance ---#
  if (is.na(side_length)) {
    side_length <- max(max(section_width), measurements::conv_unit(30, "ft", "m"))
  }

  #++++++++++++++++++++++++++++++++++++
  #+ put together the data
  #++++++++++++++++++++++++++++++++++++
  plot_data <-
    plot_data %>%
    dplyr::mutate(
      headland_length = headland_length,
      side_length = side_length,
      min_plot_length = min_plot_length,
      max_plot_length = max_plot_length
    ) %>%
    dplyr::select(
      form, machine_width, section_num, section_width, harvester_width, plot_width, proposed_plot_width, headland_length, side_length, min_plot_length, max_plot_length
    ) %>%
    dplyr::ungroup()

  return(plot_data)
}

#' Prepare plot information for a two-input experiment (feet)
#'
#' Prepare plot information for a two-input experiment case. All the length values need to be specified in feet.
#'
#' @param form (vector of two elements)
#' @param machine_width (vector of two elements)
#' @param section_num (vector of two elements)
#' @param harvester_width (numeric)
#' @param plot_width (vector of two elements) Default is c(NA, NA).
#' @param headland_length (numeric) Default is NA.
#' @param side_length (numeric) Default is NA.
#' @param max_plot_width (numeric) Default is 120 feet.
#' @param max_plot_length (numeric) Default is 240 feet.
#' @param min_plot_length (numeric) Default is 260 feet.
#' @returns a tibble with plot information necessary to create experiment plots
#' @import data.table
#' @export
#' @examples
#' form <- c("seed", "NH3")
#' machine_width <- c(12, 9)
#' section_num <- c(12, 1)
#' plot_width <- c(9.5, 36)
#' harvester_width <- 12
#' prep_plot_fd(form, machine_width, section_num, harvester_width, plot_width)
#'
prep_plot_fd <- function(form,
                         machine_width,
                         section_num,
                         harvester_width,
                         plot_width = c(NA, NA),
                         headland_length = NA,
                         side_length = NA,
                         max_plot_width = 120, # 120 feet
                         min_plot_length = 240, # 240 feet
                         max_plot_length = 260 # 260 feet
) {

  #--- dimension check ---#
  fms_ls <- c(length(form), length(machine_width), length(section_num), length(plot_width))
  if (any(fms_ls != 2)) {
    stop("Inconsistent numbers of elements in form, machine_width, section_num, and plot_width. Check if all of them have two elements.")
  }

  section_width <- machine_width / section_num

  plot_data <-
    data.frame(
      form = form,
      machine_width = machine_width,
      section_num = section_num,
      harvester_width = harvester_width,
      plot_width = plot_width
    ) %>%
    dplyr::mutate(section_width = machine_width / section_num) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      lcm_found =
        lcm_check(section_width, harvester_width, max_plot_width)
    ) %>%
    dplyr::mutate(
      proposed_plot_width =
        find_plotwidth(section_width, harvester_width, max_plot_width)
    ) %>%
    dplyr::mutate(plot_width = ifelse(is.na(plot_width), proposed_plot_width, plot_width))


  #++++++++++++++++++++++++++++++++++++
  #+ Check and notify the mixed treatment problems (with potential suggestions)
  #++++++++++++++++++++++++++++++++++++
  warning_message <-
    plot_data %>%
    dplyr::mutate(messages = list(
      if (lcm_found & plot_width %% proposed_plot_width == 0 & proposed_plot_width < plot_width) {
        paste0(
          "For ", form, ", there is a plot width that is smaller than the plot width you suggested and avoids mixed treatement problem. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
      } else if (lcm_found & plot_width %% proposed_plot_width != 0) {
        paste0(
          "For ", form, ", the plot width you specified would cause mixed treatment problems. However, there is a plot width that avoids them. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
      } else if (!lcm_found & plot_width != proposed_plot_width) {
        paste0(
          "For ", form, ", the plot width you specified would cause mixed treatment problems. Unfortunately, there is no plot width that avoids them. Plot width of ", proposed_plot_width, " ensures that at least one harvest path within the path of ", form, " does not have the problems."
        )
      } else {
        NULL
      }
    )) %>%
    dplyr::pull(messages)

  #--- notify the user of potential problems and improvements ---#
  message(unlist(warning_message))

  #--- warnd the user that they may have serious mixed treatment problems   ---#
  if (all(!plot_data$lcm_found)) {
    message(paste0(
      "Neither of ", form[1], " and ", form[2], " does not have a plot width without mixed treatment problems. Please consider running experiments with a single input instead of two."
    ))
  }

  #++++++++++++++++++++++++++++++++++++
  #+ head and side lengths
  #++++++++++++++++++++++++++++++++++++
  #--- head distance ---#
  if (is.na(headland_length)) {
    headland_length <- 2 * max(machine_width)
  }

  #--- side distance ---#
  if (is.na(side_length)) {
    side_length <- max(max(section_width), 30)
  }
  #++++++++++++++++++++++++++++++++++++
  #+ put together the data
  #++++++++++++++++++++++++++++++++++++
  plot_data <-
    plot_data %>%
    dplyr::mutate(
      headland_length = headland_length,
      side_length = side_length,
      min_plot_length = min_plot_length,
      max_plot_length = max_plot_length
    ) %>%
    dplyr::select(
      form, machine_width, section_num, section_width, harvester_width, plot_width, proposed_plot_width, headland_length, side_length, min_plot_length, max_plot_length
    )

  #++++++++++++++++++++++++++++++++++++
  #+ Unit converstion (feet to meter)
  #++++++++++++++++++++++++++++++++++++
  cols_conv <- c("machine_width", "section_width", "harvester_width", "plot_width", "proposed_plot_width", "headland_length", "side_length", "min_plot_length", "max_plot_length")

  plot_data <- dplyr::mutate(plot_data, dplyr::across(dplyr::all_of(cols_conv), ~ measurements::conv_unit(.x, "ft", "m")))

  # plot_data[, (cols_conv) := lapply(.SD, function(x) measurements::conv_unit(x, "ft", "m")), .SDcols = cols_conv]

  return(plot_data)
}

#' Prepare plot information for a single-input experiment (meter)
#'
#' Prepare plot information for a single-input experiment case. All the length values need to be specified in meter.
#'
#' @param form (character)
#' @param machine_width (numeric)
#' @param section_num (numeric)
#' @param harvester_width (numeric)
#' @param plot_width (numeric) Default is c(NA, NA).
#' @param headland_length (numeric) Default is NA.
#' @param side_length (numeric) Default is NA.
#' @param max_plot_width (numeric) Default is 36.576 meter (120 feet).
#' @param max_plot_length (numeric) Default is 73.152 meter (240 feet).
#' @param min_plot_length (numeric) Default is 79.248 meter (260 feet).
#' @returns a tibble with plot information necessary to create experiment plots
#' @import data.table
#' @export
#' @examples
#' form <- "seed"
#' machine_width <- 12
#' section_num <- 12
#' plot_width <- NA
#' harvester_width <- 24
#' prep_plot_ms(form, machine_width, section_num, harvester_width)
#'
prep_plot_ms <- function(form,
                         machine_width,
                         section_num,
                         harvester_width,
                         plot_width = NA,
                         headland_length = NA,
                         side_length = NA,
                         max_plot_width = measurements::conv_unit(120, "ft", "m"), # 36.4576 meter
                         min_plot_length = measurements::conv_unit(240, "ft", "m"), # 73.152 feet
                         max_plot_length = measurements::conv_unit(260, "ft", "m") # 79.248 meter
) {

  #--- dimension check ---#
  fms_ls <- c(length(form), length(machine_width), length(section_num), length(plot_width))
  if (any(fms_ls != 1)) {
    stop("Inconsistent numbers of elements in form, machine_width, section_num, and plot_width. Check if all of them have a single element.")
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
    warning_message <- NULL
    if (lcm_found & plot_width %% proposed_plot_width == 0 & proposed_plot_width < plot_width) {
      warning_message <-
        paste0(
          "For ", form, ", there is a plot width that is smaller than the plot width you suggested and avoids mixed treatement problem. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
    } else if (lcm_found & plot_width %% proposed_plot_width != 0) {
      warning_message <-
        paste0(
          "For ", form, ", the plot width you specified would cause mixed treatment problems. However, there is a plot width that avoids them. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
    } else if (!lcm_found & plot_width != proposed_plot_width) {
      warning_message <-
        paste0(
          "For ", form, ", the plot width you specified would cause mixed treatment problems. Unfortunately, there is no plot width that avoids them. Plot width of ", proposed_plot_width, " ensures that at least one harvest path within the path of ", form, " does not have the problems."
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
    data.frame(
      form = form,
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
#' @param form (character)
#' @param machine_width (numeric)
#' @param section_num (numeric)
#' @param harvester_width (numeric)
#' @param plot_width (numeric) Default is c(NA, NA).
#' @param headland_length (numeric) Default is NA.
#' @param side_length (numeric) Default is NA.
#' @param max_plot_width (numeric) Default is 120 feet.
#' @param max_plot_length (numeric) Default is 240 feet.
#' @param min_plot_length (numeric) Default is 260 feet.
#' @returns a tibble with plot information necessary to create experiment plots
#' @import data.table
#' @export
#' @examples
#' form <- "seed"
#' machine_width <- 60
#' section_num <- 24
#' plot_width <- NA
#' harvester_width <- 30
#' prep_plot_fs(form, machine_width, section_num, harvester_width)
#'
prep_plot_fs <- function(form,
                         machine_width,
                         section_num,
                         harvester_width,
                         plot_width = NA,
                         headland_length = NA,
                         side_length = NA,
                         max_plot_width = 120, # 120 feet
                         min_plot_length = 240, # 240 feet
                         max_plot_length = 260 # 260 feet
) {

  #--- dimension check ---#
  fms_ls <- c(length(form), length(machine_width), length(section_num), length(plot_width))
  if (any(fms_ls != 1)) {
    stop("Inconsistent numbers of elements in form, machine_width, section_num, and plot_width. Check if all of them have a single element.")
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
    warning_message <- NULL
    if (lcm_found & plot_width %% proposed_plot_width == 0 & proposed_plot_width < plot_width) {
      warning_message <-
        paste0(
          "For ", form, ", there is a plot width that is smaller than the plot width you suggested and avoids mixed treatement problem. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
    } else if (lcm_found & plot_width %% proposed_plot_width != 0) {
      warning_message <-
        paste0(
          "For ", form, ", the plot width you specified would cause mixed treatment problems. However, there is a plot width that avoids them. It is suggested that you use ", proposed_plot_width, " as the plot width."
        )
    } else if (!lcm_found & plot_width != proposed_plot_width) {
      warning_message <-
        paste0(
          "For ", form, ", the plot width you specified would cause mixed treatment problems. Unfortunately, there is no plot width that avoids them. Plot width of ", proposed_plot_width, " ensures that at least one harvest path within the path of ", form, " does not have the problems."
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
    data.frame(
      form = form,
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

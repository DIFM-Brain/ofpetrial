#' Create a data frame of trial information for a single input
#'
#' Create a data frame of trial information for a single input
#'
#' @param form (character) type of input (e.g., UAN32, seed)
#' @param plot_width (numeric) width of experimental plots in meter
#' @param machine_width (numeric) machine width (applicator, planter)
#' @param section_num (numeric) number of sections of the machine
#' @param length_unit ("feet" or "meter") unit of length for machine_width and plot_width (default is "meter")
#' @returns data frame of trial information for a single input
#' @import data.table
#' @examples
#'
#' seed_plot_info <-
#'   make_input_plot_data(
#'     form = "seed",
#'     plot_width = 30,
#'     machine_width = 60,
#'     section_num = 24,
#'     length_unit = "feet"
#'   )
#'
#' seed_plot_info
#'
#' n_plot_info <-
#'   make_input_plot_data(
#'     form = "NH3",
#'     plot_width = measurements::conv_unit(60, "ft", "m"),
#'     machine_width = measurements::conv_unit(60, "ft", "m"),
#'     section_num = 1
#'   )
#'
#' n_plot_info
#' @export

make_input_plot_data <- function(form, plot_width, machine_width, section_num, length_unit = "meter") {
  if (!length_unit %in% c("meter", "feet")) {
    stop('Error: length_unit specified is neither "meter" or "feet".')
  } else if (length_unit == "feet") {
    message(
      'Note: machine_width and plot_width are converted to meter as length_unit is specified as "feet". '
    )
  }

  input_trial_data <-
    data.frame(
      form = form,
      plot_width = plot_width,
      machine_width = machine_width,
      section_num = section_num
    ) %>%
    dplyr::mutate(
      plot_width = ifelse(
        length_unit == "feet",
        measurements::conv_unit(plot_width, "ft", "m"),
        plot_width
      )
    ) %>%
    dplyr::mutate(
      machine_width = ifelse(
        length_unit == "feet",
        measurements::conv_unit(machine_width, "ft", "m"),
        machine_width
      )
    ) %>%
    dplyr::mutate(section_width = machine_width / section_num)

  return(input_trial_data)
}

#' Make experimental plots/strips inside the field boundary
#'
#' Make experimental plots/strips inside the field boundary, harvester ab-line, and applicator/planter ab-line.
#'
#' @param input_plot_info (data.fram or a list of two data.frames) list of plot information created by make_input_plot()
#' @param boundary_file (string) path of the field boundary file
#' @param harvester_width (numeric) width of the harvester
#' @param abline_file (string) path of the ab-line file
#' @param abline_type (string) the type of ab-line generation. Select from "free", "lock", and "none"
#' @param headland_length (numeric) Default = NA. Length of the headland
#' @param side_length (numeric) Default = NA. Length of the sides
#' @param min_plot_length (numeric) Default = 61 meter (200 feet) minimum plot length
#' @param max_plot_length (numeric) Default = 91 meter (300 feet) maximum plot length
#' @param length_unit ("meter" or "feet") Default = "meter", unit of length for harvester_width, headland_length, side_length, min_plot_length, and max_plot_length
#' @param perpendicular (logical) Default = FALSE.
#' @returns a tibble that include experimental plots as sf
#' @import data.table
#' @import sf
#' @examples
#' seed_plot_info <-
#'   make_input_plot_data(
#'     form = "seed",
#'     plot_width = 40,
#'     machine_width = 60,
#'     section_num = 24,
#'     length_unit = "feet"
#'   )
#'
#' n_plot_info <-
#'   make_input_plot_data(
#'     form = "NH3",
#'     plot_width = measurements::conv_unit(30, "ft", "m"),
#'     machine_width = measurements::conv_unit(30, "ft", "m"),
#'     section_num = 1
#'   )
#'
#' input_plot_info <- list(seed_plot_info, n_plot_info)
#'
#' exp_data <-
#'   make_exp_plots(
#'     input_plot_info = input_plot_info,
#'     boundary_file = system.file("extdata", "boundary-simple1.shp", package = "ofpetrial"),
#'     abline_file = system.file("extdata", "ab-line-simple1.shp", package = "ofpetrial"),
#'     harvester_width = 30,
#'     abline_type = "free",
#'     headland_length = 30,
#'     side_length = 60,
#'     min_plot_length = 200,
#'     max_plot_length = 300,
#'     length_unit = "feet",
#'     perpendicular = FALSE
#'   )
#'
#' exp_data$exp_plots
#' @export

make_exp_plots <- function(input_plot_info,
                           boundary_file,
                           harvester_width,
                           abline_file = NA,
                           abline_type = "free", # one of "free", "lock", "non"
                           headland_length = NA,
                           side_length = NA,
                           min_plot_length = 61, # about 200 feet
                           max_plot_length = 91, # about 300 feet
                           length_unit = "meter",
                           perpendicular = FALSE) {

  # !============================================================
  # ! Set up
  # !============================================================
  #++++++++++++++++++++++++++++++++++++
  #+ Create trial data
  #++++++++++++++++++++++++++++++++++++
  if (class(input_plot_info) == "data.frame") {
    input_trial_data <- input_plot_info
  } else if (class(input_plot_info) == "list") {
    input_trial_data <-
      input_plot_info %>%
      rbindlist()
  }

  #++++++++++++++++++++++++++++++++++++
  #+Unit conversion (feet to meter) of global parameters
  #++++++++++++++++++++++++++++++++++++
  #--- head distance ---#
  if (is.na(headland_length)) {
    headland_length <- 2 * max(input_trial_data$machine_width)
  }

  #--- side distance ---#
  if (is.na(side_length)) {
    side_length <- max(max(input_trial_data$section_width), measurements::conv_unit(30, "ft", "m"))
  }

  if (!length_unit %in% c("meter", "feet")) {
    stop('Error: length_unit specified is neither "meter" or "feet".')
  } else if (length_unit == "feet") {
    message(
      'Note: length arguments (e.g., harvester_width) are converted to meter as length_unit is specified as "feet". '
    )
    harvester_width <- measurements::conv_unit(harvester_width, "ft", "m")
    min_plot_length <- measurements::conv_unit(min_plot_length, "ft", "m")
    max_plot_length <- measurements::conv_unit(max_plot_length, "ft", "m")
    headland_length <- measurements::conv_unit(headland_length, "ft", "m")
    side_length <- measurements::conv_unit(side_length, "ft", "m")
  }

  # !============================================================
  # ! Get the data ready (field boundary and plot-heading)
  # !============================================================
  #++++++++++++++++++++++++++++++++++++
  #+ Field boundary
  #++++++++++++++++++++++++++++++++++++
  field_sf <-
    sf::st_read(boundary_file, quiet = TRUE) %>%
    make_sf_utm() %>%
    sf::st_combine()

  #--- get the boundary box of the field ---#
  field_bbox <- sf::st_bbox(field_sf)

  #--- the length of the diagonal line of the boundary box ---#
  #* this is used for modifying ab-line later
  field_len_cross <- sqrt(
    (field_bbox["xmax"] - field_bbox["xmin"])^2 +
      (field_bbox["ymax"] - field_bbox["ymin"])^2
  )

  #++++++++++++++++++++++++++++++++++++
  #+ Plot-heading and other parameter
  #++++++++++++++++++++++++++++++++++++
  trial_data_pa <-
    input_trial_data %>%
    #--- whether to lock or not ---#
    dplyr::mutate(lock_ab_line = (abline_type == "lock")) %>%
    #--- input with ab-line lock first ---#
    dplyr::arrange(desc(lock_ab_line)) %>%
    dplyr::mutate(input_id = seq_len(nrow(.))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(line_edge_id = NA) %>%
    #--- heading sf ---#
    dplyr::mutate(ab_sf = list(sf::st_read(abline_file, quiet = TRUE) %>% make_sf_utm())) %>%
    dplyr::mutate(ab_sf = list(
      if ("POINT" %in% sf::st_geometry_type(ab_sf)) {
        #--- as-applied file should be point sf instead line sf ---#
        make_heading_from_past_asapplied(ab_sf, field_sf)
      } else {
        #--- pick only the first one if there are multiple lines ---#
        ab_sf[1, ]
      }
    )) %>%
    #--- make ab-line longer ---#
    dplyr::mutate(ab_sf = list(
      st_extend_line(
        sf::st_geometry(ab_sf),
        field_len_cross / as.numeric(sf::st_length(ab_sf))
      )
    ))

  if (nrow(trial_data_pa) > 1 & abline_type != "lock") {
    create_plot_edge_line <- TRUE
  } else {
    create_plot_edge_line <- FALSE
  }

  # !============================================================
  # ! Create experimental plots
  # !============================================================
  num_unique_plot_width <-
    trial_data_pa$plot_width %>%
    unique() %>%
    length()

  #++++++++++++++++++++++++++++++++++++
  #+ First input
  #++++++++++++++++++++++++++++++++++++
  # ab_sf <- trial_data_first$ab_sf[[1]]
  # ab_lines_data <- trial_data_first$ab_lines_data[[1]]
  # base_ab_lines_data <- trial_data_first$base_ab_lines_data[[1]]
  # plot_width <- trial_data_first$plot_width[[1]]
  # machine_width <- trial_data_first$machine_width[[1]]
  # abline_type <- "free"
  # field_sf <- field_sf

  # #--- by default uses the first one ---#
  trial_data_first <-
    trial_data_pa[1, ] %>%
    dplyr::mutate(base_ab_lines_data = list(
      prepare_ablines(
        ab_line = ab_sf,
        field = field_sf,
        plot_width = plot_width
      )
    )) %>%
    dplyr::mutate(exp_data = list(
      make_trial_plots_by_input(
        field = field_sf,
        #--- by default uses the first one ---#
        ab_lines_data = base_ab_lines_data,
        abline_type = abline_type,
        plot_width = plot_width,
        machine_width = machine_width,
        harvester_width = harvester_width,
        section_num = section_num,
        headland_length = headland_length,
        side_length = side_length,
        min_plot_length = min_plot_length,
        max_plot_length = max_plot_length,
        perpendicular = perpendicular,
        second_input = FALSE
      )
    )) %>%
    dplyr::mutate(exp_plots = list(
      exp_data$exp_plots
    )) %>%
    dplyr::mutate(ab_lines_data = list(
      exp_data$ab_lines_data
    )) %>%
    #--- make ab-lines ---#
    dplyr::mutate(ab_lines = list(
      ab_lines <- make_ablines(
        ab_sf = ab_sf,
        ab_lines_data = ab_lines_data,
        base_ab_lines_data = base_ab_lines_data,
        plot_width = plot_width,
        machine_width = machine_width,
        abline_type = abline_type,
        field_sf = field_sf
      )
    )) %>%
    #--- make ab-lines for the harvester ---#
    dplyr::mutate(harvest_ab_lines = list(
      harvest_ab_lines <- make_ablines(
        ab_sf = ab_sf,
        ab_lines_data = ab_lines_data,
        base_ab_lines_data = base_ab_lines_data,
        plot_width = plot_width,
        machine_width = harvester_width,
        abline_type = abline_type,
        field_sf = field_sf
      )
    )) %>%
    dplyr::mutate(line_edge = list(
      make_plot_edge_line(
        ab_lines_data = ab_lines_data,
        create_plot_edge_line = create_plot_edge_line,
        base_ab_lines_data = base_ab_lines_data,
        plot_width = plot_width
      )
    ))

  line_edge <- trial_data_first$line_edge[[1]]

  trial_data_first <- dplyr::select(trial_data_first, -line_edge)

  #++++++++++++++++++++++++++++++++++++
  #+ Second input if it is a two-input case
  #++++++++++++++++++++++++++++++++++++
  if (num_unique_plot_width == 1 & nrow(trial_data_pa) == 1) {
    #* one-input case
    trial_data_e <- trial_data_first
  } else if (num_unique_plot_width == 1 & nrow(trial_data_pa) == 2) {
    #* two-input case, but the same plot width for both inputs
    trial_data_second <-
      trial_data_pa[2, ] %>%
      dplyr::mutate(base_ab_lines_data = list(
        prepare_ablines(
          ab_line = ifelse(
            abline_type == "lock",
            sf::st_as_sf(ab_sf), # ab-sf provided
            sf::st_as_sf(line_edge) # the edge of the experiment plots of the first input
          ) %>% .[[1]],
          field = field_sf,
          plot_width = plot_width
        )
      )) %>%
      dplyr::mutate(exp_data = NA) %>%
      #--- use the same experiment plots as the first one ---#
      dplyr::mutate(exp_plots = list(
        trial_data_first$exp_plots[[1]]
      )) %>%
      #--- use the ab-lines data as the first one ---#
      dplyr::mutate(ab_lines_data = list(
        trial_data_first$ab_lines_data[[1]]
      )) %>%
      #--- make ab-lines ---#
      dplyr::mutate(ab_lines = list(
        ab_lines <- make_ablines(
          ab_sf = ab_sf,
          ab_lines_data = ab_lines_data,
          base_ab_lines_data = base_ab_lines_data,
          plot_width = plot_width,
          machine_width = machine_width,
          abline_type = abline_type,
          field_sf = field_sf
        )
      )) %>%
      #--- make ab-lines ---#
      dplyr::mutate(harvest_ab_lines = list(
        harvest_ab_lines <- make_ablines(
          ab_sf = ab_sf,
          ab_lines_data = ab_lines_data,
          base_ab_lines_data = base_ab_lines_data,
          plot_width = plot_width,
          machine_width = harvester_width,
          abline_type = abline_type,
          field_sf = field_sf
        )
      ))

    trial_data_e <- rbind(trial_data_first, trial_data_second)
    # trial_data_e$exp_plots
  } else {
    #* if two inputs and they have different plot widths
    trial_data_second <-
      trial_data_pa[2, ] %>%
      dplyr::mutate(base_ab_lines_data = list(
        prepare_ablines(
          ab_line = ifelse(
            abline_type == "lock",
            sf::st_as_sf(ab_sf), # ab-sf provided
            sf::st_as_sf(line_edge) # the edge of the experiment plots of the first input
          ) %>% .[[1]],
          field = field_sf,
          plot_width = plot_width
        )
      )) %>%
      dplyr::mutate(exp_data = list(
        make_trial_plots_by_input(
          field = field_sf,
          #--- by default uses the first one ---#
          ab_lines_data = base_ab_lines_data,
          abline_type = abline_type,
          plot_width = plot_width,
          machine_width = machine_width,
          harvester_width = harvester_width,
          section_num = section_num,
          headland_length = headland_length,
          side_length = side_length,
          min_plot_length = min_plot_length,
          max_plot_length = max_plot_length,
          perpendicular = perpendicular,
          second_input = TRUE
        )
      )) %>%
      dplyr::mutate(exp_plots = list(
        exp_data$exp_plots
      )) %>%
      dplyr::mutate(ab_lines_data = list(
        exp_data$ab_lines_data
      )) %>%
      #--- make ab-lines ---#
      dplyr::mutate(ab_lines = list(
        ab_lines <- make_ablines(
          ab_sf = ab_sf,
          ab_lines_data = ab_lines_data,
          base_ab_lines_data = base_ab_lines_data,
          plot_width = plot_width,
          machine_width = machine_width,
          abline_type = abline_type,
          field_sf = field_sf
        )
      )) %>%
      #--- make ab-lines ---#
      dplyr::mutate(harvest_ab_lines = list(
        harvest_ab_lines <- make_ablines(
          ab_sf = ab_sf,
          ab_lines_data = ab_lines_data,
          base_ab_lines_data = base_ab_lines_data,
          plot_width = plot_width,
          machine_width = harvester_width,
          abline_type = abline_type,
          field_sf = field_sf
        )
      ))

    trial_data_e <- rbind(trial_data_first, trial_data_second)
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Finalize ab-lines and headland
  #++++++++++++++++++++++++++++++++++++
  trial_data_eh <-
    trial_data_e %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ab_lines = list(
      if (!is.null(ab_lines)) {
        sf::st_transform(ab_lines, 4326)
      }
    )) %>%
    dplyr::mutate(harvest_ab_lines = list(
      if (!is.null(harvest_ab_lines)) {
        sf::st_transform(harvest_ab_lines, 4326)
      }
    )) %>%
    #--- dissolve the experimental plots as a single polygon ---#
    dplyr::mutate(experiment_plots_dissolved = list(
      exp_plots %>%
        sf::st_buffer(0.01) %>% # this avoids tiny tiny gaps between plots
        lwgeom::st_snap_to_grid(size = 0.0001) %>%
        sf::st_make_valid() %>%
        dplyr::summarize(plot_id = min(plot_id))
    )) %>%
    #--- Create headland ---#
    # experiment_plots_dissolved <- trial_data_eh$experiment_plots_dissolved
    # trial_data_eh$headland
    dplyr::mutate(headland = list(
      st_difference(field_sf, experiment_plots_dissolved) %>%
        sf::st_as_sf() %>%
        dplyr::rename(., geometry = attr(., "sf_column")) %>%
        dplyr::select(geometry)
    ))

  trial_data_eh$field_sf <- field_sf

  trial_data_return <-
    dplyr::select(
      trial_data_eh,
      form,
      plot_width,
      field_sf,
      headland,
      exp_plots,
      ab_lines,
      harvest_ab_lines
    ) %>%
    dplyr::mutate(abline_type = abline_type) %>%
    dplyr::ungroup()

  return(trial_data_return)
}

# !===========================================================
# ! Helper internal functions
# !===========================================================
#++++++++++++++++++++++++++++++++++++
#+ Make trial plots for a single input
#++++++++++++++++++++++++++++++++++++
make_trial_plots_by_input <- function(field,
                                      ab_lines_data,
                                      abline_type,
                                      plot_width,
                                      machine_width,
                                      harvester_width,
                                      section_num,
                                      headland_length,
                                      side_length,
                                      min_plot_length,
                                      max_plot_length,
                                      perpendicular,
                                      second_input = FALSE) {

  #--- conversion ---#
  # plot_width <- conv_unit(plot_width, "ft", "m")
  # machine_width <- conv_unit(machine_width, "ft", "m")
  # harvester_width <- conv_unit(harvester_width, "ft", "m")
  # headland_length <- conv_unit(headland_length, "ft", "m")
  # side_length <- conv_unit(side_length, "ft", "m")

  #--- ab-line tilted by harvester angle ---#
  plot_heading <- ab_lines_data$plot_heading
  #--- unit vector pointing in the direction the machine moves ---#
  ab_xy_nml <- ab_lines_data$ab_xy_nml
  #--- unit vector pointing in the direction PERPENDICULAR to the direction the machine moves ---#
  ab_xy_nml_p90 <- ab_lines_data$ab_xy_nml_p90

  #++++++++++++++++++++++++++++++++++++
  #+ Create strips
  #++++++++++++++++++++++++++++++++++++
  f_bbox <- sf::st_bbox(field)

  #--- maximum distance ---#
  radius <-
    sqrt(
      (f_bbox["xmax"] - f_bbox["xmin"])^2 +
        (f_bbox["ymax"] - f_bbox["ymin"])^2
    ) / 2 + 100

  strips <- create_strips(field, plot_heading, plot_width, radius)

  # ggplot() +
  #   geom_sf(data = strips, aes(fill = "group")) +
  #   geom_sf(data = field, col = "black", fill = NA) +
  #   geom_sf(data = plot_heading, col = "red")

  #++++++++++++++++++++++++++++++++++++
  #+ Shift the polygons
  #++++++++++++++++++++++++++++++++++++
  #--- find the group id for the cells that are intersecting with the ab-line  ---#
  ab_int_group <-
    suppressWarnings(sf::st_intersection(strips, plot_heading)) %>%
    dplyr::pull(group) %>%
    unique()

  #--- get the sf of the intersecting group ---#
  int_group <- dplyr::filter(strips, group == ab_int_group)

  # ggplot() +
  #   geom_sf(data = int_group, fill = "blue", color = NA) +
  #   geom_sf(data = plot_heading, color = "red", size = 0.3)

  #--- the distance between the ab-line and the line that connect the centroids of the intersecting sf ---#
  correction_dist <-
    sf::st_distance(
      get_through_line(int_group, radius, ab_xy_nml),
      plot_heading
    ) %>%
    as.numeric()

  #--- shift the intersecting sf  ---#
  int_group_corrected <-
    st_shift(
      int_group,
      correction_dist * ab_xy_nml_p90,
      merge = FALSE
    )

  # ggplot() +
  #   geom_sf(data = int_group_corrected, fill = "blue", color = NA) +
  #   geom_sf(data = plot_heading, color = "red", size = 0.3)

  new_dist <-
    sf::st_distance(
      get_through_line(int_group_corrected, radius, ab_xy_nml),
      plot_heading
    ) %>%
    as.numeric()

  if (second_input == FALSE & abline_type == "lock") {

    # move the intersecting strip so the ab-line goes through the center
    if (new_dist > correction_dist) {
      #--- if moved further away ---#
      strips_shifted <- st_shift(strips, -correction_dist * ab_xy_nml_p90)
    } else {
      #--- if get close ---#
      strips_shifted <- st_shift(strips, correction_dist * ab_xy_nml_p90)
    }

    # ggplot() +
    #   geom_sf(data = strips_shifted, aes(fill = "group")) +
    #   geom_sf(data = field, col = "black", fill = NA) +
    #   geom_sf(data = plot_heading, col = "red")

    #--- round is for weird cases like harvester width = 62.5 ---#
    # there is no hope for aligning things correctly in such a case
    section_width <- machine_width / section_num
    num_sections_in_plot <- round(plot_width / section_width)

    # Note: if odd, the center of the machine is in the middle of the section
    is_sec_in_machine_odd <- section_num %% 2 == 1
    # Note: if odd, the center of the plot is in the middle of the section
    is_sec_in_plot_odd <- num_sections_in_plot %% 2 == 1 # odd

    if ((!is_sec_in_machine_odd & is_sec_in_plot_odd) | (is_sec_in_machine_odd & !is_sec_in_plot_odd)) {
      # if odd, then no need to shift
      strips_shifted <-
        st_shift(
          strips_shifted,
          section_width * ab_xy_nml_p90 / 2
        )
    }
  } else if (second_input == FALSE & abline_type != "lock") {
    #--- if the first input ---#
    # Note: for the first input, the cell center is aligned to the
    # supplied ab-line (which is not the final ab-line)

    if (new_dist > correction_dist) {
      #--- if moved further away ---#
      strips_shifted <- st_shift(strips, -correction_dist * ab_xy_nml_p90)
    } else {
      #--- if get close ---#
      strips_shifted <- st_shift(strips, correction_dist * ab_xy_nml_p90)
    }
  } else if (second_input == TRUE) {
    #--- if the second input ---#
    # Note: line_edge is used as the ab-line for the second input
    # the left (right) edge of the cells is shifted so that it is
    # aligned with the line_edge
    if (new_dist > correction_dist) {
      #--- if moved further away ---#
      strips_shifted <-
        strips %>%
        st_shift(., -correction_dist * ab_xy_nml_p90) %>%
        st_shift(., -plot_width * ab_xy_nml_p90 / 2)
    } else {
      #--- if get close ---#
      strips_shifted <-
        strips %>%
        st_shift(., correction_dist * ab_xy_nml_p90) %>%
        st_shift(., plot_width * ab_xy_nml_p90 / 2)
    }
  }

  # ggplot() +
  #   geom_sf(data = strips_shifted, fill = "blue", color = NA) +
  #   geom_sf(data = plot_heading, col = "red", size = 0.3)

  #++++++++++++++++++++++++++++++++++++
  #+ Create experiment plots
  #++++++++++++++++++++++++++++++++++++
  min_length <- measurements::conv_unit(min_plot_length, "ft", "m") # (200 feet)
  max_length <- measurements::conv_unit(max_plot_length, "ft", "m") #  (300 feet)
  mean_length <- (min_length + max_length) / 2

  side_length <- 1.5 * side_length

  # ggplot(final_exp_plots) +
  #   geom_sf(aes(fill = factor(strip_id)))

  # ggplot() +
  #   geom_sf(data = field) +
  #   geom_sf(data = sf::st_buffer(field, - side_length)) +
  #   geom_sf(data = dplyr::filter(final_exp_plots, group == 157) %>% pull(through_line) %>% .[[1]]) +
  #   coord_sf(datum = sf::st_crs(field))

  #--- this function is created to just suppress warnings from st_intersection ---#
  st_intersection_q <- purrr::quietly(sf::st_intersection)

  int_lines <-
    field %>%
    #--- create an inner buffer ---#
    sf::st_buffer(-side_length) %>%
    #--- intersect strips and the field ---#
    st_intersection_q(strips_shifted, .) %>%
    .$result %>%
    dplyr::select(group) %>%
    dplyr::rowwise() %>%
    #--- split multipolygons to individual polygons ---#
    dplyr::mutate(indiv_polygon = list(
      sf::st_cast(geometry, "POLYGON") %>%
        sf::st_as_sf() %>%
        data.table() %>%
        .[, group := group]
    )) %>%
    purrr::pluck("indiv_polygon") %>%
    purrr::reduce(rbind) %>%
    .[, poly_id := 1:.N, by = group] %>%
    sf::st_as_sf() %>%
    dplyr::rowwise() %>%
    #--- get the original strip geometry by group ---#
    dplyr::left_join(., as.data.frame(strips_shifted[, c("group", "geometry")]), by = "group") %>%
    #--- draw a line that goes through the middle of the strips ---#
    dplyr::mutate(through_line = list(
      get_through_line(geometry, radius, ab_xy_nml)
    )) %>%
    dplyr::mutate(int_line = list(
      #--- multistring can be created here ---#
      # Note: when there is a hole in the field, we can have
      # a multilinestring.
      suppressWarnings(sf::st_intersection(x, through_line)) %>%
        #--- separate multiline string into to individual linestring ---#
        sf::st_cast("LINESTRING") %>%
        sf::st_as_sf() %>%
        dplyr::mutate(group = group) %>%
        dplyr::mutate(poly_id = poly_id) %>%
        dplyr::mutate(line_id = seq_len(nrow(.)))
    )) %>%
    dplyr::filter(length(int_line) != 0) %>%
    purrr::pluck("int_line") %>%
    purrr::reduce(rbind)

  final_exp_plots <- int_lines %>%
    dplyr::rowwise() %>%
    #--- move int_points inward by (head_dist - side_distance) ---#
    dplyr::mutate(new_center_line = list(
      move_points_inward(
        x,
        max(headland_length - side_length, 0),
        ab_xy_nml
      )
    )) %>%
    dplyr::filter(!is.null(new_center_line)) %>%
    dplyr::mutate(tot_plot_length = list(
      as.numeric(sf::st_length(new_center_line))
    )) %>%
    dplyr::mutate(plot_data = list(
      get_plot_data(
        tot_plot_length,
        min_length,
        mean_length
      )
    )) %>%
    dplyr::filter(!is.null(plot_data)) %>%
    dplyr::mutate(plots = list(
      create_plots_in_strip(
        plot_data,
        new_center_line,
        plot_width,
        ab_xy_nml,
        ab_xy_nml_p90
      ) %>%
        dplyr::mutate(group = group) %>%
        dplyr::mutate(poly_line = paste0(poly_id, "_", line_id))
    )) %>%
    purrr::pluck("plots") %>%
    purrr::reduce(rbind) %>%
    dplyr::rename(strip_id = group) %>%
    dplyr::mutate(strip_id = strip_id - min(strip_id) + 1) %>%
    sf::st_set_crs(sf::st_crs(field))

  if (perpendicular) {
    # Notes:
    # This is for the case of harvesting and application being perpendicular.
    # All the plots must have the same length (specified by min_plot_length and max_plot_length)
    # Plots are "misaligned" by the exact multiple of harvester width to avoid harvester having to straddle

    # ggplot() +
    #   geom_sf(data = final_exp_plots_hadjsuted, col = "red", fill = NA) +
    #   geom_sf(data = final_exp_plots, col = "blue", fill = NA)
    # ggplot() +
    #   geom_sf(data = final_exp_plots, aes(fill = strip_id))

    # ggplot() +
    #   geom_sf(data = purrr::reduce(final_exp_plots$shifted_plots, rbind), col = "red", fill = NA) +
    #   geom_sf(data = purrr::reduce(final_exp_plots$shifted_line, rbind), col = "red", fill = NA) +
    #   geom_sf(data = purrr::reduce(final_exp_plots$data, rbind), col = "blue", fill = NA) +
    #   geom_sf(data = final_exp_plots$shifted_line[[13]], col = "blue", fill = NA)

    final_exp_plots <- final_exp_plots %>%
      dplyr::nest_by(strip_id, poly_line) %>%
      dplyr::mutate(first_plot = list(
        dplyr::filter(data, plot_id == 1)
      )) %>%
      dplyr::mutate(perpendicular_line = list(
        get_through_line(first_plot$geometry, radius, ab_xy_nml_p90)
      )) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(base_line = .[1, ]$perpendicular_line) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dist_to_base = sf::st_distance(perpendicular_line, base_line) %>%
        as.numeric()) %>%
      dplyr::mutate(remainder = dist_to_base %% harvester_width) %>%
      dplyr::mutate(correction_dist = min(remainder, harvester_width - remainder)) %>%
      dplyr::mutate(shifted_first_plot = list(
        st_shift(first_plot, correction_dist * ab_xy_nml)
      )) %>%
      dplyr::mutate(shifted_line = list(
        get_through_line(shifted_first_plot$geometry, radius, ab_xy_nml_p90)
      )) %>%
      dplyr::mutate(
        new_remainder =
          as.numeric(sf::st_distance(base_line, shifted_line)) %% harvester_width
      ) %>%
      # if the distance is close enough moving in the wrong
      # direction does not hurt
      dplyr::mutate(is_close_enough = min(new_remainder, harvester_width - new_remainder) < 1e-6) %>%
      dplyr::mutate(shift_direction = list(
        ifelse(is_close_enough, 1, -1)
      )) %>%
      dplyr::mutate(shifted_plots = list(
        st_shift(data, shift_direction * correction_dist * ab_xy_nml) %>%
          dplyr::mutate(strip_id = strip_id)
      )) %>%
      purrr::pluck("shifted_plots") %>%
      purrr::reduce(rbind)
  }


  #* +++++++++++++++++++++++++++++++++++
  #* ab-lines data
  #* +++++++++++++++++++++++++++++++++++

  ab_lines_data <-
    rbind(
      get_through_line(
        dplyr::filter(
          final_exp_plots,
          strip_id == min(strip_id) & plot_id == 1
        ) %>% dplyr::slice(1),
        radius,
        ab_xy_nml
      ),
      get_through_line(
        dplyr::filter(
          final_exp_plots,
          strip_id == max(strip_id) & plot_id == 1
        ) %>% dplyr::slice(1),
        radius,
        ab_xy_nml
      )
    ) %>%
    dplyr::mutate(ab_id = seq_len(nrow(.))) %>%
    expand_grid_df(tibble::tibble(dir_p = c(-1, 1)), .) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(geometry = list(x)) %>%
    dplyr::mutate(ab_line_for_direction_check = list(
      sf::st_as_sf(st_shift(
        geometry,
        dir_p * ab_xy_nml_p90 * (5 * plot_width),
        merge = FALSE
      ))
    )) %>%
    dplyr::mutate(intersection = list(
      sf::st_as_sf(ab_line_for_direction_check[final_exp_plots, ])
    )) %>%
    dplyr::mutate(int_check = nrow(intersection))

  return(list(
    exp_plots = final_exp_plots,
    ab_lines_data = ab_lines_data
  ))
}


#* +++++++++++++++++++++++++++++++++++
#* Make ab-line
#* +++++++++++++++++++++++++++++++++++

make_ablines <- function(ab_sf,
                         ab_lines_data,
                         base_ab_lines_data,
                         plot_width,
                         machine_width,
                         abline_type,
                         field_sf) {
  ab_xy_nml_p90 <- base_ab_lines_data$ab_xy_nml_p90

  if (abline_type == "non") {
    return(NULL)
  } else if (abline_type == "lock") {
    ab_lines <-
      ab_sf %>%
      sf::st_as_sf() %>%
      dplyr::mutate(ab_id = 1)
    return(ab_lines)
  } else if (abline_type == "free") {
    if (machine_width == plot_width) {
      ab_lines <- ab_lines_data %>%
        dplyr::select(ab_id, x) %>%
        unique(by = "ab_id") %>%
        sf::st_as_sf() %>%
        dplyr::ungroup()
    } else {
      #--- ab-line re-centering when machine width > plot_width ---#
      ab_lines <- ab_lines_data %>%
        #--- which direction to go ---#
        # Notes: go inward (intersecting) if machine_width > plot_width, otherwise outward
        dplyr::filter(int_check == ifelse(machine_width > plot_width, 1, 0)) %>%
        dplyr::mutate(ab_recentered = list(
          st_shift(
            geometry,
            dir_p * ab_xy_nml_p90 * abs(machine_width - plot_width) / 2,
            merge = FALSE
          )
        )) %>%
        purrr::pluck("ab_recentered") %>%
        purrr::reduce(c) %>%
        sf::st_as_sf() %>%
        dplyr::mutate(ab_id = seq_len(nrow(.)))
    }

    # ggplot() +
    #   geom_sf(data = field, fill = NA) +
    #   geom_sf(data = exp_plot, aes(fill = type), color = NA) +
    #   geom_sf(data = line_edge_f, col = "red", size = 1) +
    #   geom_sf(data = line_edge_s, col = "darkgreen", size = 1)

    # ggplot() +
    #   geom_sf(data = field, fill = NA) +
    #   geom_sf(data = exp_plot, fill = "blue", color = NA) +
    #   geom_sf(data = ab_lines, aes(col = factor(ab_id)), size = 1)

    # ggplot() +
    #   geom_sf(data = field_sf, fill = NA) +
    #   geom_sf(data = ab_lines, size = 1)

    ab_lines <- suppressWarnings(sf::st_intersection(ab_lines, sf::st_buffer(field_sf, dist = 20)))
    
    return(ab_lines)
  }
}

make_plot_edge_line <- function(ab_lines_data,
                                create_plot_edge_line,
                                base_ab_lines_data,
                                plot_width) {

  # Note 1: this is used to align the left (or) right edges of the first input experiment plot
  # Note 2: even if the starting point is locked, this still applies

  #--- which way to move for the first to go inward ---#
  # ab_lines_data$int_check

  ab_xy_nml_p90 <- base_ab_lines_data$ab_xy_nml_p90

  if (create_plot_edge_line) {
    line_edge <-
      ab_lines_data %>%
      #--- the direction that goes off of the field ---#
      dplyr::filter(int_check == 0) %>%
      #--- use only the first one ---#
      .[1, ] %>%
      dplyr::mutate(line_edge = list(
        st_shift(geometry, dir_p * ab_xy_nml_p90 * plot_width / 2, merge = FALSE)
      )) %>%
      purrr::pluck("line_edge") %>%
      .[[1]]

    return(line_edge)
  } else {
    return(NULL)
  }
}

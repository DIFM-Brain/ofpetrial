#' Make experimental plots/strips inside the field boundary
#'
#' Make experimental plots/strips inside the field boundary, harvester ab-line, and applicator/planter ab-line.
#'
#' @param input_plot_info (data.fram or a list of two data.frames) list of plot information created by make_input_plot()
#' @param boundary_data (character) path of the field boundary file or boundary as an sf
#' @param abline_data (character or sf) path of the ab-line file or ab-line as an sf
#' @param abline_type (character) the type of ab-line generation. Select from "free", "lock", and "none"
#' @returns a tibble that include experimental plots as sf
#' @import data.table
#' @import sf
#' @examples
#' n_plot_info <-
#'   prep_plot(
#'     input_name = "NH3",
#'     unit_system = "imperial",
#'     machine_width = 30,
#'     section_num = 1,
#'     harvester_width = 20,
#'     headland_length = 30,
#'     side_length = 60
#'   )
#'
#' exp_data <-
#'   make_exp_plots(
#'     input_plot_info = n_plot_info,
#'     boundary_data = system.file("extdata", "boundary-simple1.shp", package = "ofpetrial"),
#'     abline_data = system.file("extdata", "ab-line-simple1.shp", package = "ofpetrial"),
#'     abline_type = "free"
#'   )
#'
#' exp_data$exp_plots
#' @export

make_exp_plots <- function(input_plot_info,
                           boundary_data,
                           abline_data = NA,
                           abline_type = "free" # one of "free", "lock", "non"
) {
  # !===========================================================
  # ! Check and modify input_plot_info if necessary
  # !===========================================================
  if ("list" %in% class(input_plot_info)) {
    input_plot_info <- rbindlist(input_plot_info)

    check_length_consistency <-
      lapply(
        input_plot_info[, .(harvester_width, min_plot_length, max_plot_length)],
        function(x) length(unique(x))
      ) %>%
      unlist() %>%
      all(. == 1)
    if (!check_length_consistency) {
      stop("You specified inconsistent length for at least one of harvester_width, min_plot_length, and max_plot_length. Please make sure they are the same when preparing plot information individually. Or, please use prep_plot() to avoid these inconsistencies.")
    }

    input_plot_info$headland_length <- max(input_plot_info$headland_length)
    input_plot_info$side_length <- max(input_plot_info$side_length)
  }

  # !============================================================
  # ! Get the data ready (field boundary and plot-heading)
  # !============================================================
  #++++++++++++++++++++++++++++++++++++
  #+ Field boundary
  #++++++++++++++++++++++++++++++++++++
  boundary_class <- class(boundary_data)

  if ("sf" %in% boundary_class) {
    field_sf <-
      boundary_data %>%
      .[-c(1:ncol(.))] %>%
      make_sf_utm() %>%
      sf::st_as_sf()
  } else if ("character" %in% boundary_class) {
    field_sf <-
      sf::st_read(boundary_data, quiet = TRUE) %>%
      .[-c(1:ncol(.))] %>%
      make_sf_utm() %>%
      sf::st_combine() %>%
      sf::st_as_sf()
  }

  #--- remove linestring if any ---#
  field_sf <- dplyr::filter(field_sf, st_geometry_type(field_sf) != "LINESTRING")

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
  abline_class <- class(abline_data)

  trial_data_pa <-
    input_plot_info %>%
    #--- whether to lock or not ---#
    dplyr::mutate(lock_ab_line = (abline_type == "lock")) %>%
    #--- input with ab-line lock first ---#
    dplyr::arrange(desc(lock_ab_line)) %>%
    dplyr::mutate(input_id = seq_len(nrow(.))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(line_edge_id = NA) %>%
    #--- heading sf ---#
    dplyr::mutate(ab_sf = list(
      if ("sf" %in% abline_class) {
        abline_data %>%
          .[-c(1:ncol(.))] %>%
          make_sf_utm()
      } else if ("character" %in% abline_class) {
        sf::st_read(abline_data, quiet = TRUE) %>%
          .[-c(1:ncol(.))] %>%
          make_sf_utm()
      }
    )) %>%
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

  # ggplot() +
  #   geom_sf(data = field_sf) +
  #   geom_sf(data = trial_data_pa$ab_sf[[1]], color = "red")
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
  # base_ab_lines_data <- trial_data_first$base_ab_lines_data[[1]]
  # abline_type <- "free"
  # plot_width <- trial_data_first$plot_width[[1]]
  # field <- field_sf
  # machine_width <- trial_data_first$machine_width[[1]]
  # harvester_width <- trial_data_first$harvester_width[[1]]
  # section_num <- trial_data_first$section_num[[1]]
  # headland_length <- trial_data_first$headland_length[[1]]
  # side_length <- trial_data_first$side_length[[1]]
  # min_plot_length <- trial_data_first$min_plot_length[[1]]
  # max_plot_length <- trial_data_first$max_plot_length[[1]]
  # second_input <- FALSE

  #--- by default uses the first one ---#
  trial_data_first <-
    trial_data_pa[1, ] %>%
    dplyr::mutate(base_ab_lines_data = list(
      prepare_ablines(
        ab_line = ab_sf,
        field = field_sf,
        plot_width = plot_width
      )
    )) %>%
    dplyr::mutate(exp_plots = list(
      make_trial_plots_by_input(
        field = field_sf,
        #--- by default uses the first one ---#
        base_ab_lines_data = base_ab_lines_data,
        abline_type = abline_type,
        plot_width = plot_width,
        machine_width = machine_width,
        harvester_width = harvester_width,
        section_num = section_num,
        headland_length = headland_length,
        side_length = side_length,
        min_plot_length = min_plot_length,
        max_plot_length = max_plot_length,
        second_input = FALSE
      )
    )) %>%
    dplyr::mutate(ablines_data = list(
      make_ablines_data(
        exp_plots = exp_plots,
        base_ab_lines_data = base_ab_lines_data,
        plot_width = plot_width,
        field_sf = field_sf
      )
    )) %>%
    #--- make ab-lines ---#
    dplyr::mutate(ab_lines = list(
      ab_lines <- make_ablines(
        ab_sf = ab_sf,
        ablines_data = ablines_data,
        base_ab_lines_data = base_ab_lines_data,
        plot_width = plot_width,
        machine_width = machine_width,
        abline_type = abline_type,
        field_sf = field_sf
      )
    )) %>%
    #--- make harvester ab-lines ---#
    dplyr::mutate(harvest_ab_lines = list(
      harvest_ab_lines <- make_ablines(
        ab_sf = ab_sf,
        ablines_data = ablines_data,
        base_ab_lines_data = base_ab_lines_data,
        plot_width = plot_width,
        machine_width = harvester_width,
        abline_type = "free",
        field_sf = field_sf
      )
    )) %>%
    dplyr::mutate(line_edge = list(
      make_plot_edge_line(
        ablines_data = ablines_data,
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
      dplyr::mutate(ablines_data = list(
        trial_data_first$ablines_data[[1]]
      )) %>%
      #--- make ab-lines ---#
      dplyr::mutate(ab_lines = list(
        ab_lines <- make_ablines(
          ab_sf = ab_sf,
          ablines_data = ablines_data,
          base_ab_lines_data = base_ab_lines_data,
          plot_width = plot_width,
          machine_width = machine_width,
          abline_type = abline_type,
          field_sf = field_sf
        )
      )) %>%
      #--- make harvester ab-lines ---#
      dplyr::mutate(harvest_ab_lines = list(
        trial_data_first$harvest_ab_lines[[1]]
      )) %>%
      dplyr::select(-exp_data)


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
      dplyr::mutate(exp_plots = list(
        make_trial_plots_by_input(
          field = field_sf,
          #--- by default uses the first one ---#
          base_ab_lines_data = base_ab_lines_data,
          abline_type = abline_type,
          plot_width = plot_width,
          machine_width = machine_width,
          harvester_width = harvester_width,
          section_num = section_num,
          headland_length = headland_length,
          side_length = side_length,
          min_plot_length = min_plot_length,
          max_plot_length = max_plot_length,
          second_input = TRUE
        )
      )) %>%
      dplyr::mutate(ablines_data = list(
        make_ablines_data(
          exp_plots = exp_plots,
          base_ab_lines_data = base_ab_lines_data,
          plot_width = plot_width,
          field_sf = field_sf
        )
      )) %>%
      #--- make ab-lines ---#
      dplyr::mutate(ab_lines = list(
        ab_lines <- make_ablines(
          ab_sf = ab_sf,
          ablines_data = ablines_data,
          base_ab_lines_data = base_ab_lines_data,
          plot_width = plot_width,
          machine_width = machine_width,
          abline_type = abline_type,
          field_sf = field_sf
        )
      )) %>%
      #--- assign harvester ab-lines ---#
      dplyr::mutate(harvest_ab_lines = list(
        trial_data_first$harvest_ab_lines[[1]]
      ))

    trial_data_e <- rbind(trial_data_first, trial_data_second)
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Finalize ab-lines and headland
  #++++++++++++++++++++++++++++++++++++
  trial_data_eh <-
    trial_data_e %>%
    dplyr::rowwise() %>%
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
      suppressWarnings(st_difference(field_sf, experiment_plots_dissolved)) %>%
        sf::st_as_sf() %>%
        dplyr::rename(., geometry = attr(., "sf_column")) %>%
        dplyr::select(geometry)
    ))

  trial_data_eh$field_sf <- list(field_sf)

  trial_data_return <-
    dplyr::select(
      trial_data_eh,
      input_name,
      unit_system,
      field_sf,
      headland,
      exp_plots,
      ab_lines,
      harvest_ab_lines,
      plot_width,
      harvester_width,
      machine_width,
      section_num,
      headland_length,
      side_length
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
                                      base_ab_lines_data,
                                      abline_type,
                                      plot_width,
                                      machine_width,
                                      harvester_width,
                                      section_num,
                                      headland_length,
                                      side_length,
                                      min_plot_length,
                                      max_plot_length,
                                      second_input = FALSE) {
  #--- conversion ---#
  # plot_width <- measurements::conv_unit(plot_width, "ft", "m")
  # machine_width <- measurements::conv_unit(machine_width, "ft", "m")
  # harvester_width <- measurements::conv_unit(harvester_width, "ft", "m")
  # headland_length <- measurements::conv_unit(headland_length, "ft", "m")
  # side_length <- measurements::conv_unit(side_length, "ft", "m")

  #--- ab-line tilted by harvester angle ---#
  plot_heading <- base_ab_lines_data$plot_heading

  #--- unit vector pointing in the direction the machine moves ---#
  ab_xy_nml <- base_ab_lines_data$ab_xy_nml

  #--- unit vector pointing in the direction PERPENDICULAR to the direction the machine moves ---#
  ab_xy_nml_p90 <- base_ab_lines_data$ab_xy_nml_p90

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

  #--- create strips ---#
  #* only the angle of plot is used from plot_heading
  strips <- 
    create_strips(field, plot_heading, plot_width, radius) %>%
    st_make_valid()

  # ggplot(filter(strips, group == 1)) +
  #   geom_sf()
  # ggplot() +
  #   geom_sf(data = strips, aes(fill = group)) +
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

  #--- get the sf of the intersecting sf ---#
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
    # Note: line_edge is being used as the ab-line for the second input
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
  #   geom_sf(data = strips_shifted, fill = "blue", alpha = 0.3) +
  #   geom_sf(data = plot_heading, col = "red", size = 0.3)

  #++++++++++++++++++++++++++++++++++++
  #+ Create experiment plots
  #++++++++++++++++++++++++++++++++++++
  # ggplot(final_exp_plots) +
  #   geom_sf(aes(fill = factor(strip_id)))

  # ggplot() +
  #   geom_sf(data = field) +
  #   geom_sf(data = sf::st_buffer(field, - side_length)) +
  #   geom_sf(data = dplyr::filter(final_exp_plots, group == 157) %>% pull(through_line) %>% .[[1]]) +
  #   coord_sf(datum = sf::st_crs(field))

  # ggplot(strips_shifted) +
  #   geom_sf(aes(fill = factor(group))) +
  #   geom_sf(data = dplyr::filter(strips_shifted, group == 30), fill = "red") +
  #   geom_sf(data = field)

  # ggplot(strips) +
  #   geom_sf(aes(fill = factor(group))) +
  #   geom_sf(data = dplyr::filter(strips, group == 30), fill = "red") +
  #   geom_sf(data = field)

  # ggplot(int_lines) +
  #   geom_sf(data = field) +
  #   geom_sf(aes(fill = factor(group)))

  # ggplot() +
  #   geom_sf(data = field) +
  #   geom_sf(data = int_lines$through_line)

  # ggplot() +
  #   geom_sf(data = field) +
  #   geom_sf(data = int_lines$x)

  inner_buffer_dist <- side_length + plot_width / 2

  int_lines <-
    field %>%
    #--- create an inner buffer ---#
    sf::st_buffer(-inner_buffer_dist) %>%
    #--- intersect strips and the inner buffer of the field ---#
    # there will be incomplete strips at the edge
    st_intersection_quietly(strips_shifted, .) %>%
    .$result %>%
    dplyr::select(group) %>%
    dplyr::rowwise() %>%
    #--- split multipolygons to individual polygons ---#
    # multipolygons may exist when there are holes in the field
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
    #--- draw a line that goes through the middle of the strips based on the original strip geometry ---#
    # this lines goes beyond the field boundary
    dplyr::mutate(through_line = list(
      get_through_line(geometry, radius, ab_xy_nml)
    )) %>%
    dplyr::mutate(int_line = list(
      #--- multistring can be created here ---#
      # Note (1): when there is a hole in the field, we can have
      # a multilinestring.
      # Note (2): "x" in st_intersection refers to the geometry of the strips intersected with the inner buffer of the field, not that of strips_shifted
      # Note (3): through_lines at the edge may not be intersecting with any of the intersected polygons, which then will be ignored. This means that at least half (if field is rectangular) of the edge strips must be intersecting with the inner buffer.
      suppressWarnings(sf::st_intersection(x, through_line)) %>%
        #--- separate multiline string into to individual linestring ---#
        sf::st_cast("LINESTRING") %>%
        sf::st_as_sf() %>%
        dplyr::mutate(group = group) %>%
        dplyr::mutate(poly_id = poly_id) %>%
        dplyr::mutate(line_id = seq_len(nrow(.)))
    )) %>%
    dplyr::filter(nrow(int_line) != 0) %>%
    purrr::pluck("int_line") %>%
    purrr::reduce(rbind)

  # ggplot() +
  #   geom_sf(data = field_sf) +
  #   geom_sf(data = int_lines)

  # ggplot() +
  #   geom_sf(data = final_exp_plots) +
  #   geom_sf(data = filter(final_exp_plots, poly_line == "1_2"), fill = "red")

  # tot_plot_length <- final_exp_plots$tot_plot_length[[1]]

# final_exp_plots$tot_plot_length
# final_exp_plots$min_plot_length
# final_exp_plots$max_plot_length

  final_exp_plots <-
    int_lines %>%
    dplyr::rowwise() %>%
    #--- move int_points inward or outward in the moving direction by (head_dist - side_distance) ---#
    # when strips were intersected with the inner buffer of the field earlier, the int_lines are too short if inner_buffer_dist > headland_length and too long if inner_buffer_dist < headland_length. This operation extends or shorten the int_lines to respect the headland length specified by the user.
    dplyr::mutate(new_center_line = list(
      extend_or_shorten_line(
        x,
        headland_length - inner_buffer_dist,
        ab_xy_nml
      )
    )) %>%
    dplyr::filter(!is.null(new_center_line)) %>%
    dplyr::mutate(tot_plot_length = list(
      as.numeric(sf::st_length(new_center_line))
    )) %>%
    dplyr::mutate(plot_data = list(
      get_trial_plot_data(
        tot_plot_length, # inside the data
        min_plot_length, # inherited as an argument of make_trial_plots_by_input() 
        max_plot_length # inherited as an argument of make_trial_plots_by_input() 
      )
    )) %>%
    #--- remove plot with null plot data ---#
    # this happens when the total strip length is too short relative to the min_plot_length
    dplyr::filter(!is.null(plot_data)) %>%
    #--- create plots for each strip ---#
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
    left_join(
      ., 
      data.table(
        group = unique(.$group)
      ) %>%
      .[order(group), ] %>%
      .[, strip_id := 1:.N]
      ,
      by = "group"
    ) %>%
    dplyr::select(- group) %>%
    sf::st_set_crs(sf::st_crs(field))

  # ggplot() +
  #   geom_sf(data = field_sf) +
  #   geom_sf(data = final_exp_plots) +
  #   geom_sf(data = int_lines)

  return(final_exp_plots)
}

#++++++++++++++++++++++++++++++++++++
#+Prepare data for creating ab-lines and edge line
#++++++++++++++++++++++++++++++++++++
make_ablines_data <- function(exp_plots, base_ab_lines_data, plot_width, field_sf) {
  #--- prepare vectors ---#
  ab_xy_nml <- base_ab_lines_data$ab_xy_nml
  ab_xy_nml_p90 <- base_ab_lines_data$ab_xy_nml_p90

  #--- get length ---#
  f_bbox <- sf::st_bbox(field_sf)
  ab_length <-
    sqrt(
      (f_bbox["xmax"] - f_bbox["xmin"])^2 +
        (f_bbox["ymax"] - f_bbox["ymin"])^2
    ) / 2 + 100

  ablines_data <-
    rbind(
      #--- line the goes through the center of the first plot of the first
      #--- strip
      get_through_line(
        dplyr::filter(
          exp_plots,
          strip_id == min(strip_id) & plot_id == 1
        ) %>% dplyr::slice(1),
        ab_length,
        ab_xy_nml
      ),
      #--- line the goes through the center of the first plot of the last
      #--- strip
      get_through_line(
        dplyr::filter(
          exp_plots,
          strip_id == max(strip_id) & plot_id == 1
        ) %>% dplyr::slice(1),
        ab_length,
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
      sf::st_as_sf(ab_line_for_direction_check[exp_plots, ])
    )) %>%
    dplyr::mutate(int_check = nrow(intersection))

  return(ablines_data)
}

#* +++++++++++++++++++++++++++++++++++
#* Make ab-line
#* +++++++++++++++++++++++++++++++++++

make_ablines <- function(ab_sf,
                         ablines_data,
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
  } else if (abline_type == "free") {
    if (machine_width == plot_width) {
      #--- no further adjustment necessary ---#
      ab_lines <-
        ablines_data %>%
        dplyr::select(ab_id, x) %>%
        unique(by = "ab_id") %>%
        sf::st_as_sf() %>%
        dplyr::ungroup() %>%
        dplyr::slice(1)
    } else {
      #--- ab-line re-centering ---#
      ab_lines <-
        ablines_data %>%
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
        dplyr::mutate(ab_id = seq_len(nrow(.))) %>%
        dplyr::slice(1)
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

make_plot_edge_line <- function(ablines_data,
                                create_plot_edge_line,
                                base_ab_lines_data,
                                plot_width) {
  # Note 1: this is used to align the left (or) right edges of the first input experiment plot
  # Note 2: even if the starting point is locked, this still applies

  #--- which way to move for the first to go inward ---#
  # ablines_data$int_check

  ab_xy_nml_p90 <- base_ab_lines_data$ab_xy_nml_p90

  if (create_plot_edge_line) {
    line_edge <-
      ablines_data %>%
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

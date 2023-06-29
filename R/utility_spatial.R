#!===========================================================
# ! Expansion on sf operations
# !===========================================================
#++++++++++++++++++++++++++++++++++++
#+ Shift an sf by the user-specified shift (x, y)
#++++++++++++++++++++++++++++++++++++
#' Shift an sf (spatial) object
#'
#' Shift an sf object by user-specified amount in X and Y
#'
#' @param data_sf sf # the sf object to be shifted
#' @param shift vector # 2 by 1 vector with (x, y), where x is how much to shift in easting and y is how much to shift in northing
#' @param merge logical #  if true, then the shifted geometry replaces the existing geometry of the input sf (data_sf), otherwise only the geometry as sfc is returned
#' @returns Either sf or sfc depending on the merge parameter
st_shift <- function(data_sf, shift, merge = TRUE) {
  #--- retrieve the geometry ---#
  data_geom <- st_geometry(data_sf)
  #--- get the CRS ---#
  temp_crs <- st_crs(data_sf)

  #--- convert a shift in vector to sfc ---#
  shift_sfc <- st_point(shift) %>% st_sfc()

  #--- shift ---#
  geom_shifted <-
    (data_geom + shift_sfc) %>%
    st_set_crs(temp_crs)

  if (merge == TRUE) {
    data_sf <- st_drop_geometry(data_sf)
    data_sf$geometry <- geom_shifted
    return(st_as_sf(data_sf))
  } else {
    return(geom_shifted)
  }
}

#++++++++++++++++++++++++++++++++++++
#+ Tilt an sf by the user-specified angle
#++++++++++++++++++++++++++++++++++++
# data_sf = ab_line
# angle = harvest_angle

st_tilt <- function(data_sf, angle, base_sf = FALSE, merge = TRUE) {
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  if ("sf" %in% class(base_sf)) {
    wf_bbox <- st_bbox(base_sf) %>% st_as_sfc()
  } else {
    wf_bbox <- st_bbox(data_sf) %>% st_as_sfc()
  }

  base_point <- st_centroid(wf_bbox)
  data_geom <- st_geometry(data_sf)

  data_tilted <- ((data_geom - base_point) * rot(angle / 180 * pi) + base_point) %>%
    st_set_crs(st_crs(data_sf))

  if (merge == TRUE) {
    data_sf$geometry <- data_tilted
    return(data_sf)
  } else {
    return(data_tilted)
  }

  # ggplot() +
  #   geom_sf(data_sf, fill = "red", alpha = 0.4) +
  #   geom_sf(data_tilted, fill = "blue", alpha = 0.4)
}

#++++++++++++++++++++++++++++++++++++
#+ Extend a line sf by the user-specified multiplier
#++++++++++++++++++++++++++++++++++++
#' Extend a line
#'
#' Elongate a line by the user-specified multiplier
#'
#' @param line sf # sf line object to be extended
#' @param multiplier scalar
#' @returns sfc of the extended line
st_extend_line <- function(line, multiplier) {
  new_line <- st_geometry(line)[[1]]
  starting_point <- new_line[1, ]
  direction_vec <- new_line[2, ] - new_line[1, ]
  new_line[2, ] <- starting_point + multiplier * direction_vec

  return_line <-
    st_sfc(new_line) %>%
    st_set_crs(st_crs(line))

  return(return_line)
}

# line <- ab_line_recentered$geometry[[1]]
# multiplier <- 3
# st_extend_line(line, multiplier)

#++++++++++++++++++++++++++++++++++++
#+ Create a line that goes through the middle of a strip
#++++++++++++++++++++++++++++++++++++
get_through_line <- function(geometry, radius, ab_xy_nml) {
  centroid <- st_coordinates(st_centroid(geometry))
  end_point <- centroid + radius * ab_xy_nml
  start_point <- centroid - radius * ab_xy_nml
  return_line <- st_linestring(rbind(start_point, end_point)) %>%
    st_sfc() %>%
    st_set_crs(st_crs(geometry)) %>%
    st_as_sf()

  return(return_line)
}

# ============================================================
# = Convert to utm
# ============================================================
make_sf_utm <- function(data_sf) {
  return_sf <- data_sf %>%
    # st_set_4326() %>%
    st_make_valid() %>%
    st_transform(4326) %>%
    st_transform_utm()

  return(return_sf)
}

# =================================================
# = Get an ab-line
# =================================================

make_heading_from_past_asapplied <- function(past_aa_input, field) {
  temp_sf <- dplyr::select(past_aa_input, geometry)

  # tm_shape(past_aa_input) +
  #   tm_dots()

  #--- polygons? ---#
  inlude_polygon <- "POLYGON" %in% st_geometry_type(past_aa_input)

  if (inlude_polygon) {
    return(NULL)
  } else {
    dominant_slope <-
      group_points_sc(temp_sf, angle_threshold = 30) %>%
      nest_by(group) %>%
      rowwise() %>%
      mutate(
        slope =
          lm(Y ~ X, data = data)$coef[2]
      ) %>%
      filter(!is.na(slope)) %>%
      unnest() %>%
      mutate(cluster = kmeans(slope, 6)$cluster) %>%
      data.table() %>%
      .[, .(slope, cluster)] %>%
      .[, num_obs := .N, by = cluster] %>%
      .[num_obs == max(num_obs), ] %>%
      .[, mean(slope)]
  }

  ab_start <- st_geometry(st_centroid(field))[[1]]
  ab_end <- ab_start + c(1, dominant_slope)

  ab_line <-
    list(
      st_linestring(c(ab_start, ab_end))
    ) %>%
    st_as_sfc() %>%
    st_set_crs(st_crs(field))

  return(ab_line)
}

#++++++++++++++++++++++++++++++++++++
#+ Get harvester angle relative to input ab-line
#++++++++++++++++++++++++++++++++++++
# line_1 <- plot_heading
# line_2 <- through_line

get_angle_lines <- function(line_1, line_2) {
  rotate <- function(angle) {
    matrix(
      c(cos(angle), sin(angle), -sin(angle), cos(angle)), 2, 2
    )
  }

  h_mat <- st_geometry(line_1)[[1]]
  h_vec <- h_mat[2, ] - h_mat[1, ]
  h_vec_n <- h_vec / sqrt(sum(h_vec^2))

  i_mat <- st_geometry(line_2)[[1]]
  i_vec <- i_mat[2, ] - i_mat[1, ]
  i_vec_n <- i_vec / sqrt(sum(i_vec^2))

  angle <- acos(sum(i_vec_n * h_vec_n)) / pi * 180

  angle <-
    tibble(angle = c(angle, 180 - angle)) %>%
    rowwise() %>%
    mutate(i_vect_rotated = list(
      i_vec_n %*% rotate(angle / 180 * pi)
    )) %>%
    mutate(dot_product = sum(i_vect_rotated * h_vec_n)) %>%
    mutate(dist = abs(dot_product) - 1) %>%
    arrange(abs(dist)) %>%
    ungroup() %>%
    slice(1) %>%
    pull(angle)

  return(angle)
}

#++++++++++++++++++++++++++++++++++++
#+ Create strips
#++++++++++++++++++++++++++++++++++++
# ggplot() +
#   geom_sf(data = circle, fill = NA) +
#   geom_sf(data = field) +
#   geom_sf(data = strips, aes(fill = group), color = NA, alpha = 0.4)

create_strips <- function(field, plot_heading, plot_width, radius) {
  circle <- st_buffer(st_centroid(field), radius)

  strips <-
    st_make_grid(circle, cellsize = c(plot_width, radius * 2 + 50)) %>%
    st_as_sf() %>%
    cbind(., st_coordinates(st_centroid(.))) %>%
    data.table() %>%
    .[order(X), ] %>%
    .[, group := .GRP, by = X] %>%
    setnames("x", "geometry") %>%
    st_as_sf()

  vertical_line <-
    rbind(
      c(0, 0),
      c(0, 10)
    ) %>%
    st_linestring() %>%
    st_sfc() %>%
    st_set_crs(st_crs(field)) %>%
    st_as_sf()

  strips <-
    st_tilt(
      data_sf = strips,
      angle = get_angle_lines(plot_heading, vertical_line),
      base_sf = circle,
      merge = TRUE
    )

  return(strips)
}

#++++++++++++++++++++++++++++++++++++
#+ Transform to the appropriate UTM
#++++++++++++++++++++++++++++++++++++
st_transform_utm <- function(sf) {
  if (str_detect(st_crs(sf)$wkt, "longitude") != TRUE) {
    print("Not in lat/long. Returning original object.")
    return(sf)
  } else {
    utmzone <- utm_zone(mean(st_bbox(sf)[c(1, 3)]))
    projutm <- as.numeric(paste0("326", utmzone))
    new_sf <- st_transform(sf, projutm)
    return(new_sf)
  }
}

#++++++++++++++++++++++++++++++++++++
#+ Find the UTM zone
#++++++++++++++++++++++++++++++++++++
utm_zone <- function(long) {
  utm <- (floor((long + 180) / 6) %% 60) + 1
  return(utm)
}

#++++++++++++++++++++++++++++++++++++
#+ Move points inward
#++++++++++++++++++++++++++++++++++++
move_points_inward <- function(line, dist, ab_xy_nml) {

  #--- in case the intersected line is multi-linestring ---#
  temp_lines <- st_cast(line, "LINESTRING")
  line <- temp_lines[[length(temp_lines)]]

  if (as.numeric(st_length(line)) > 2 * dist) {
    start_point <- line[1, ]
    end_point <- line[2, ]

    new_start_point <- line[1, ] + ab_xy_nml * dist
    new_end_point <- line[2, ] - ab_xy_nml * dist

    new_through_line <-
      st_linestring(
        rbind(
          new_start_point,
          new_end_point
        )
      ) %>%
      st_sfc() %>%
      st_set_crs(st_crs(line))

    return(new_through_line)
  } else {
    return(NULL)
  }
}

#++++++++++++++++++++++++++++++++++++
#+ Get plot data
#++++++++++++++++++++++++++++++++++++
get_plot_data <- function(tot_plot_length, min_length, mean_length) {
  num_comp_plots <- tot_plot_length %/% mean_length
  remainder <- tot_plot_length %% mean_length

  return_data <- data.table(plot_id = seq_len(num_comp_plots + 1))

  if (num_comp_plots == 0) { # if no complete plots
    if (remainder < min_length) {
      return(NULL)
    } else {
      return_data[, plot_length := remainder]
    }
  } else if (min_length == mean_length) { # no flexibility in plot length allowed
    return_data <- return_data %>%
      .[seq_len(num_comp_plots), ] %>%
      .[, plot_length := mean_length]
  } else if (remainder >= (2 * min_length - mean_length)) {
    # make the last two short
    return_data[, plot_length := c(
      rep(mean_length, num_comp_plots - 1),
      rep((mean_length + remainder) / 2, 2)
    )]
  } else if (
    num_comp_plots >= 2 &
      remainder >= (3 * min_length - 2 * mean_length)
  ) {
    # make the last three short
    return_data[, plot_length := c(
      rep(mean_length, num_comp_plots - 2),
      rep((2 * mean_length + remainder) / 3, 3)
    )]
  } else if (
    num_comp_plots >= 2
  ) {
    # make the 2nd and 3rd last longer
    return_data <- return_data[, plot_length := c(
      rep(mean_length, num_comp_plots - 2),
      rep((2 * mean_length + remainder) / 2, 2),
      NA
    )] %>%
      .[!is.na(plot_length), ]
  } else {
    # only 1 complete plot
    return_data <- return_data[, plot_length := mean_length + remainder] %>%
      .[1, ]
  }

  return(return_data)
}

#++++++++++++++++++++++++++++++++++++
#+ Create plots in a strip
#++++++++++++++++++++++++++++++++++++
create_plots_in_strip <- function(plot_data,
                                  new_center_line,
                                  plot_width,
                                  ab_xy_nml,
                                  ab_xy_nml_p90) {
  make_polygon <- function(base_point,
                           start_length,
                           plot_length,
                           plot_width,
                           ab_xy_nml,
                           ab_xy_nml_p90) {
    point_0 <- base_point + ab_xy_nml * start_length
    point_1 <- point_0 + (plot_width / 2) * ab_xy_nml_p90
    point_2 <- point_1 + ab_xy_nml * plot_length
    point_3 <- point_2 - plot_width * ab_xy_nml_p90
    point_4 <- point_3 - ab_xy_nml * plot_length

    temp_polygon <- rbind(
      point_1,
      point_2,
      point_3,
      point_4,
      point_1
    ) %>%
      list() %>%
      st_polygon()

    return(temp_polygon)
  }

  base_point <- st_geometry(new_center_line)[[1]][1, ]

  return_polygons <-
    plot_data %>%
    .[, plot_start := data.table::shift(plot_length, type = "lag")] %>%
    .[is.na(plot_start), plot_start := 0] %>%
    .[, start_length := cumsum(plot_start)] %>%
    rowwise() %>%
    mutate(geometry = list(
      make_polygon(
        base_point = base_point,
        start_length = start_length,
        plot_length = plot_length,
        plot_width = plot_width,
        ab_xy_nml = ab_xy_nml,
        ab_xy_nml_p90 = ab_xy_nml_p90
      )
    )) %>%
    data.table() %>%
    .[, .(plot_id, geometry)] %>%
    st_as_sf()

  return(return_polygons)
}

# !===========================================================
# ! ab-line
# !===========================================================

#++++++++++++++++++++++++++++++++++++
#+ Prepare data for ab-line creation
#++++++++++++++++++++++++++++++++++++

prepare_ablines <- function(ab_line, field, plot_width) {

  # ab_line <- ab_sf
  rotate_mat_p90 <- matrix(
    c(
      cos(pi / 2),
      sin(pi / 2),
      -sin(pi / 2),
      cos(pi / 2)
    ),
    nrow = 2
  )

  #--- get the vector (direction machines run)  ---#
  ab_xy <- st_geometry(ab_line)[[1]][2, ] - st_geometry(ab_line)[[1]][1, ]
  #--- distance of the vector ---#
  ab_length <- sqrt(sum(ab_xy^2))
  #--- normalize (distance == 1) ---#
  ab_xy_nml <- ab_xy / ab_length
  #--- create a vector that is perpendicular to ab_xy ---#
  ab_xy_nml_p90 <- ab_xy_nml %*% rotate_mat_p90

  # === if ab-line is outside of the field boundary ===#
  if (nrow(st_as_sf(st_intersection(field, ab_line))) == 0) {
    b <- t(
      st_coordinates(st_centroid(field)) -
        st_geometry(ab_line)[[1]][1, ]
    )
    a <- cbind(
      t(ab_xy_nml_p90),
      ab_xy_nml
    )

    multiplier <- solve(a, b)

    ab_line <-
      st_shift(
        ab_line,
        round(multiplier[[1]] / plot_width) * plot_width * ab_xy_nml_p90 +
          multiplier[[2]] * ab_xy_nml,
        merge = FALSE
      )
  }

  return(list(
    plot_heading = ab_line,
    ab_xy_nml = ab_xy_nml,
    ab_xy_nml_p90 = ab_xy_nml_p90
  ))
}

#++++++++++++++++++++++++++++++++++++
#+ Make ab-lines
#++++++++++++++++++++++++++++++++++++
make_ablines <- function(ab_sf,
                         ab_lines_data,
                         base_ab_lines_data,
                         plot_width,
                         machine_width,
                         abline_type) {

  # /*----------------------------------*/
  #' ## Get the ab-line
  # /*----------------------------------*/
  # plot_width
  # machine_width <- conv_unit(90, "ft","m")
  # ggplot() +
  #   geom_sf(data = field) +
  #   geom_sf(data = ab_lines_data$ab_line_for_direction_check[[2]])
  # ggplot() +
  #     geom_sf(data = field) +
  #     geom_sf(data = ab_lines_data$geometry[[1]])

  ab_xy_nml_p90 <- base_ab_lines_data$ab_xy_nml_p90

  if (abline_type == "non") {
    return(NULL)
  } else if (abline_type == "lock") {
    ab_lines <-
      ab_sf %>%
      st_as_sf() %>%
      mutate(ab_id = 1)
    return(ab_lines)
  } else if (abline_type == "free") {
    if (machine_width == plot_width) {
      ab_lines <- ab_lines_data %>%
        dplyr::select(ab_id, x) %>%
        unique(by = "ab_id") %>%
        st_as_sf() %>%
        ungroup()
    } else {
      # === ab-line re-centering when machine width > plot_width ===#
      ab_lines <- ab_lines_data %>%
        # === which direction to go ===#
        # Notes: go inward (intersecting) if machine_width > plot_width, otherwise outward
        filter(int_check == ifelse(machine_width > plot_width, 1, 0)) %>%
        mutate(ab_recentered = list(
          st_shift(
            geometry,
            dir_p * ab_xy_nml_p90 * abs(machine_width - plot_width) / 2,
            merge = FALSE
          )
        )) %>%
        pluck("ab_recentered") %>%
        purrr::reduce(c) %>%
        st_as_sf() %>%
        mutate(ab_id = seq_len(nrow(.)))
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
    #   geom_sf(data = field, fill = NA) +
    #   geom_sf(data = exp_plot, fill = "blue", color = NA) +
    #   geom_sf(data = ab_line, size = 1)

    return(ab_lines)
  }
}

#++++++++++++++++++++++++++++++++++++
#+ Make an line sf of the edge of the experiment field
#++++++++++++++++++++++++++++++++++++
make_plot_edge_line <- function(ab_lines_data,
                                create_plot_edge_line,
                                base_ab_lines_data,
                                plot_width) {

  # /*----------------------------------*/
  #' ## Get the edge of the experiment for the second input
  # /*----------------------------------*/
  # Note 1: this is used to align the left (or) right edges of the first input experiment plot
  # Note 2: even if the starting point is locked, this still applies

  # === which way to move for the first to go inward ===#
  # ab_lines_data$int_check

  ab_xy_nml_p90 <- base_ab_lines_data$ab_xy_nml_p90

  if (create_plot_edge_line) {
    line_edge <-
      ab_lines_data %>%
      # === the direction that goes off of the field ===#
      filter(int_check == 0) %>%
      # === use only the first one ===#
      .[1, ] %>%
      mutate(line_edge = list(
        st_shift(geometry, dir_p * ab_xy_nml_p90 * plot_width / 2, merge = FALSE)
      )) %>%
      pluck("line_edge") %>%
      .[[1]]

    return(line_edge)
  } else {
    return(NULL)
  }
}

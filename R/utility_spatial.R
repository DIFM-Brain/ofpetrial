#++++++++++++++++++++++++++++++++++++
#+ Shift an sf by the user-specified shift (x, y)
#++++++++++++++++++++++++++++++++++++
st_shift <- function(data_sf, shift, merge = TRUE) {
  #--- retrieve the geometry ---#
  data_geom <- sf::st_geometry(data_sf)
  #--- get the CRS ---#
  temp_crs <- sf::st_crs(data_sf)

  #--- convert a shift in vector to sfc ---#
  shift_sfc <- sf::st_point(shift) %>% sf::st_sfc()

  #--- shift ---#
  geom_shifted <-
    (data_geom + shift_sfc) %>%
    sf::st_set_crs(temp_crs)

  if (merge == TRUE) {
    data_sf <- st_drop_geometry(data_sf)
    data_sf$geometry <- geom_shifted
    return(sf::st_as_sf(data_sf))
  } else {
    return(geom_shifted)
  }
}

#++++++++++++++++++++++++++++++++++++
#+ Tilt an sf by the user-specified angle
#++++++++++++++++++++++++++++++++++++
st_tilt <- function(data_sf, angle, base_sf = FALSE, merge = TRUE) {
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  if ("sf" %in% class(base_sf)) {
    wf_bbox <- sf::st_bbox(base_sf) %>% sf::st_as_sfc()
  } else {
    wf_bbox <- sf::st_bbox(data_sf) %>% sf::st_as_sfc()
  }

  base_point <- st_centroid_quietly(wf_bbox)
  data_geom <- sf::st_geometry(data_sf)

  data_tilted <- ((data_geom - base_point) * rot(angle / 180 * pi) + base_point) %>%
    sf::st_set_crs(sf::st_crs(data_sf)) %>%
    st_make_valid()

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
st_extend_line <- function(line, multiplier) {
  new_line <- sf::st_geometry(line)[[1]]
  starting_point <- new_line[1, ]
  direction_vec <- new_line[2, ] - new_line[1, ]
  new_line[2, ] <- starting_point + multiplier * direction_vec

  return_line <-
    sf::st_sfc(new_line) %>%
    sf::st_set_crs(sf::st_crs(line))

  return(return_line)
}

#++++++++++++++++++++++++++++++++++++
#+ Create a line that goes through the middle of a strip
#++++++++++++++++++++++++++++++++++++
get_through_line <- function(geometry, radius, ab_xy_nml) {
  centroid <- suppressWarnings(sf::st_coordinates(st_centroid_quietly(geometry)))
  end_point <- centroid + radius * ab_xy_nml
  start_point <- centroid - radius * ab_xy_nml
  return_line <- sf::st_linestring(rbind(start_point, end_point)) %>%
    sf::st_sfc() %>%
    sf::st_set_crs(sf::st_crs(geometry)) %>%
    sf::st_as_sf()

  return(return_line)
}

# ============================================================
# = Convert to utm
# ============================================================
make_sf_utm <- function(data_sf) {
  return_sf <- data_sf %>%
    # st_set_4326() %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326) %>%
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
  include_polygon <- "POLYGON" %in% sf::st_geometry_type(past_aa_input)

  if (include_polygon) {
    return(NULL)
  } else {
    dominant_slope <-
      group_points_sc(temp_sf, angle_threshold = 30) %>%
      dplyr::nest_by(group) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        slope =
          lm(Y ~ X, data = data)$coef[2]
      ) %>%
      dplyr::filter(!is.na(slope)) %>%
      tidyr::unnest(cols = c(data)) %>%
      dplyr::mutate(cluster = kmeans(slope, 6)$cluster) %>%
      data.table() %>%
      .[, .(slope, cluster)] %>%
      .[, num_obs := .N, by = cluster] %>%
      .[num_obs == max(num_obs), ] %>%
      .[, mean(slope)]
  }

  ab_start <- sf::st_geometry(st_centroid_quietly(
    past_aa_input[which.min(st_distance(past_aa_input, sf::st_geometry(st_centroid_quietly(field)))), ]
  ))[[1]]
  ab_end <- ab_start + c(1, dominant_slope)

  ab_line <-
    list(
      sf::st_linestring(c(ab_start, ab_end))
    ) %>%
    sf::st_as_sfc() %>%
    sf::st_set_crs(sf::st_crs(field))

  return(ab_line)
}

#++++++++++++++++++++++++++++++++++++
#+ Get harvester angle relative to input ab-line
#++++++++++++++++++++++++++++++++++++

get_angle_lines <- function(line_1, line_2) {
  rotate <- function(angle) {
    matrix(
      c(cos(angle), sin(angle), -sin(angle), cos(angle)), 2, 2
    )
  }

  h_mat <- sf::st_geometry(line_1)[[1]]
  h_vec <- h_mat[2, ] - h_mat[1, ]
  h_vec_n <- h_vec / sqrt(sum(h_vec^2))

  i_mat <- sf::st_geometry(line_2)[[1]]
  i_vec <- i_mat[2, ] - i_mat[1, ]
  i_vec_n <- i_vec / sqrt(sum(i_vec^2))

  angle <- acos(sum(i_vec_n * h_vec_n)) / pi * 180

  angle <-
    tibble::tibble(angle = c(angle, 180 - angle)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(i_vect_rotated = list(
      i_vec_n %*% rotate(angle / 180 * pi)
    )) %>%
    dplyr::mutate(dot_product = sum(i_vect_rotated * h_vec_n)) %>%
    dplyr::mutate(dist = abs(dot_product) - 1) %>%
    dplyr::arrange(abs(dist)) %>%
    dplyr::ungroup() %>%
    dplyr::slice(1) %>%
    dplyr::pull(angle)

  return(angle)
}

#++++++++++++++++++++++++++++++++++++
#+ Create strips
#++++++++++++++++++++++++++++++++++++
create_strips <- function(field, plot_heading, plot_width, radius) {
  field_centroid <- st_centroid_quietly(field)
  circle <- sf::st_buffer(field_centroid, radius)

  #++++++++++++++++++++++++++++++++++++
  #+ get direction vectors
  #++++++++++++++++++++++++++++++++++++
  #--- get the vector (direction machines run)  ---#
  ab_xy <- sf::st_geometry(plot_heading)[[1]][2, ] - sf::st_geometry(plot_heading)[[1]][1, ]

  #--- distance of the vector ---#
  ab_length <- sqrt(sum(ab_xy^2))

  #--- normalize (distance == 1) ---#
  ab_xy_nml <- ab_xy / ab_length

  #--- create a vector that is perpendicular to ab_xy ---#
  ab_xy_nml_p90 <- ab_xy_nml %*% rotate_mat_p90

  #++++++++++++++++++++++++++++++++++++
  #+ Get the line along which strips are created
  #++++++++++++++++++++++++++++++++++++
  field_centroid_xy <- st_coordinates(field_centroid)
  starting_point <- field_centroid_xy + ab_xy_nml_p90 * radius * 2
  end_point <- field_centroid_xy - ab_xy_nml_p90 * radius * 2

  #++++++++++++++++++++++++++++++++++++
  #+ Create strips
  #++++++++++++++++++++++++++++++++++++

  make_polygon <- function(base_point, radius, plot_width) {
    point_0 <- base_point + ab_xy_nml * radius * 2
    point_1 <- point_0 + ab_xy_nml_p90 * plot_width
    point_2 <- point_1 - ab_xy_nml * radius * 4
    point_3 <- point_2 - ab_xy_nml_p90 * plot_width
    point_4 <- point_0

    temp_polygon <- rbind(
      point_0,
      point_1,
      point_2,
      point_3,
      point_4
    ) %>%
      list() %>%
      sf::st_polygon() %>%
      st_sfc() %>%
      st_as_sf()

    return(temp_polygon)
  }

  num_strips <- ceiling(sqrt(sum((starting_point - end_point)^2)) / plot_width) + 1

  base_points <-
    rep(1, num_strips + 1) %*% starting_point - (0:num_strips) %*% ab_xy_nml_p90 * plot_width

  strips <-
    lapply(
      1:nrow(base_points),
      \(x) {
        base_point <- base_points[x, ]
        make_polygon(base_point, radius, plot_width)
      }
    ) %>%
    rbindlist() %>%
    data.table::setnames(names(.), "geometry") %>%
    st_as_sf() %>%
    st_set_crs(st_crs(field)) %>%
    .[field, ] %>%
    dplyr::mutate(group = 1:nrow(.))

  return(strips)
}


#++++++++++++++++++++++++++++++++++++
#+ Transform to the appropriate UTM
#++++++++++++++++++++++++++++++++++++
st_transform_utm <- function(sf) {
  if (grepl("longitude", sf::st_crs(sf)$wkt) != TRUE) {
    message("Not in lat/long. Returning original object.")
    return(sf)
  } else {
    utmzone <- utm_zone(mean(sf::st_bbox(sf)[c(1, 3)]))
    projutm <- as.numeric(paste0("326", utmzone))
    new_sf <- sf::st_transform(sf, projutm)
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
extend_or_shorten_line <- function(line, dist, ab_xy_nml) {
  # dist > 0 means shortening the line
  # dist < 0 means extending the line

  #--- in case the intersected line is multi-linestring ---#
  temp_lines <- sf::st_cast(line, "LINESTRING")
  line <- temp_lines[[length(temp_lines)]]

  if (as.numeric(sf::st_length(line)) > 2 * dist) { # if the line is shorter than the twice of the distance to be "shortened" (dist > 0). If dist < 0, then okay.
    start_point <- line[1, ]
    end_point <- line[2, ]

    new_start_point <- line[1, ] + ab_xy_nml * dist
    new_end_point <- line[2, ] - ab_xy_nml * dist

    new_through_line <-
      sf::st_linestring(
        rbind(
          new_start_point,
          new_end_point
        )
      ) %>%
      sf::st_sfc() %>%
      sf::st_set_crs(sf::st_crs(line))

    return(new_through_line)
  } else {
    return(NULL)
  }
}

#++++++++++++++++++++++++++++++++++++
#+ Get plot data
#++++++++++++++++++++++++++++++++++++
get_trial_plot_data <- function(tot_plot_length, min_plot_length, max_plot_length) {

  #* +++++++++++++++++++++++++++++++++++
  #* For debugging
  #* +++++++++++++++++++++++++++++++++++

  # tot_plot_length <- final_exp_plots$tot_plot_length[[5]]
  # min_plot_length <- 73.152
  # max_plot_length <- 91.44

  #* +++++++++++++++++++++++++++++++++++
  #* Main
  #* +++++++++++++++++++++++++++++++++++
  #--- use minimum length to start ---#
  num_comp_plots <- tot_plot_length %/% min_plot_length
  #--- find remainder of available plot length ---#
  remainder <- tot_plot_length %% min_plot_length

  #--- how much additional length can be added to each plot ---#
  additional_length <- remainder / num_comp_plots

  if (length(num_comp_plots) == 0 | num_comp_plots == 0) {
    return_data <- NULL
  } else if (additional_length + min_plot_length > max_plot_length) {
    #--- in this case there is more length than can be used with max plot length ---#
    #--- thus, we will use the max ---#
    return_data <-
      data.table::data.table(
        plot_id = seq_len(num_comp_plots),
        plot_length = max_plot_length
      )
  } else {
    #--- here the additional length plus the minimum plot length do not exceed the max length ---#
    #--- can divide up the remainder among the plots ---#
    return_data <-
      data.table::data.table(
        plot_id = seq_len(num_comp_plots),
        plot_length = (min_plot_length + additional_length)
      )
  }

  return(return_data)
}


# get_plot_data <- function(tot_plot_length, min_plot_length, mean_length) {
#   num_comp_plots <- tot_plot_length %/% mean_length
#   remainder <- tot_plot_length %% mean_length

#   return_data <- data.table(plot_id = seq_len(num_comp_plots + 1))

#   if (num_comp_plots == 0) { # if no complete plots
#     if (remainder < min_plot_length) {
#       return(NULL)
#     } else {
#       return_data[, plot_length := remainder]
#     }
#   } else if (min_plot_length == mean_length) { # no flexibility in plot length allowed
#     return_data <- return_data %>%
#       .[seq_len(num_comp_plots), ] %>%
#       .[, plot_length := mean_length]
#   } else if (remainder >= (2 * min_plot_length - mean_length)) {
#     # make the last two short
#     return_data[, plot_length := c(
#       rep(mean_length, num_comp_plots - 1),
#       rep((mean_length + remainder) / 2, 2)
#     )]
#   } else if (
#     num_comp_plots >= 2 &
#       remainder >= (3 * min_plot_length - 2 * mean_length)
#   ) {
#     # make the last three short
#     return_data[, plot_length := c(
#       rep(mean_length, num_comp_plots - 2),
#       rep((2 * mean_length + remainder) / 3, 3)
#     )]
#   } else if (
#     num_comp_plots >= 2
#   ) {
#     # make the 2nd and 3rd last longer
#     return_data <- return_data[, plot_length := c(
#       rep(mean_length, num_comp_plots - 2),
#       rep((2 * mean_length + remainder) / 2, 2),
#       NA
#     )] %>%
#       .[!is.na(plot_length), ]
#   } else {
#     # only 1 complete plot
#     return_data <- return_data[, plot_length := mean_length + remainder] %>%
#       .[1, ]
#   }

#   return(return_data)
# }

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
      sf::st_polygon()

    return(temp_polygon)
  }

  base_point <- sf::st_geometry(new_center_line)[[1]][1, ]

  if (is.null(plot_data) == FALSE) {
    return_polygons <-
      plot_data %>%
      .[, plot_start := data.table::shift(plot_length, type = "lag")] %>%
      .[is.na(plot_start), plot_start := 0] %>%
      .[, start_length := cumsum(plot_start)] %>%
      dplyr::rowwise() %>%
      dplyr::mutate(geometry = list(
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
      sf::st_as_sf()
  } else {
    return_polygons <- NULL
  }


  return(return_polygons)
}

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
  ab_xy <- sf::st_geometry(ab_line)[[1]][2, ] - sf::st_geometry(ab_line)[[1]][1, ]
  #--- distance of the vector ---#
  ab_length <- sqrt(sum(ab_xy^2))
  #--- normalize (distance == 1) ---#
  ab_xy_nml <- ab_xy / ab_length
  #--- create a vector that is perpendicular to ab_xy ---#
  ab_xy_nml_p90 <- ab_xy_nml %*% rotate_mat_p90

  # === if ab-line is outside of the field boundary ===#
  if (nrow(sf::st_as_sf(suppressWarnings(sf::st_intersection(field, ab_line)))) == 0) {
    b <- t(
      sf::st_coordinates(st_centroid_quietly(field)) -
        sf::st_geometry(ab_line)[[1]][1, ]
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

#* +++++++++++++++++++++++++++++++++++
#* Make harvester (yield) polygons based on harvester ab-line
#* +++++++++++++++++++++++++++++++++++
make_harvest_path <- function(harvester_width, harvest_ab_line, field_sf) {
  base_ab_lines_data <-
    prepare_ablines(
      ab_line = harvest_ab_line,
      field = field_sf,
      plot_width = harvester_width
    )

  #--- ab-line tilted by harvester angle ---#
  plot_heading <- base_ab_lines_data$plot_heading
  #--- unit vector pointing in the direction the machine moves ---#
  ab_xy_nml <- base_ab_lines_data$ab_xy_nml
  #--- unit vector pointing in the direction PERPENDICULAR to the direction the machine moves ---#
  ab_xy_nml_p90 <- base_ab_lines_data$ab_xy_nml_p90

  #++++++++++++++++++++++++++++++++++++
  #+ Create strips
  #++++++++++++++++++++++++++++++++++++
  f_bbox <- sf::st_bbox(field_sf)

  #--- maximum distance ---#
  radius <-
    sqrt(
      (f_bbox["xmax"] - f_bbox["xmin"])^2 +
        (f_bbox["ymax"] - f_bbox["ymin"])^2
    ) / 2 + 100

  #--- create strips ---#
  #* only the angle of plot is used from plot_heading
  strips <-
    create_strips(field_sf, plot_heading, harvester_width, radius) %>%
    st_make_valid()

  # ggplot() +
  #   geom_sf(data = strips, aes(fill = group)) +
  #   geom_sf(data = field_sf, col = "black", fill = NA) +
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

  new_dist <-
    sf::st_distance(
      get_through_line(int_group_corrected, radius, ab_xy_nml),
      plot_heading
    ) %>%
    as.numeric()

  # move the intersecting strip so the ab-line goes through the center
  if (new_dist > correction_dist) {
    #--- if moved further away ---#
    strips_shifted <- st_shift(strips, -correction_dist * ab_xy_nml_p90)
  } else {
    #--- if get close ---#
    strips_shifted <- st_shift(strips, correction_dist * ab_xy_nml_p90)
  }

  harvester_path <-
    st_intersection_quietly(strips_shifted, field_sf) %>%
    .$result %>%
    dplyr::mutate(ha_strip_id = 1:dplyr::n()) %>%
    dplyr::select(ha_strip_id)

  return(harvester_path)
}

#++++++++++++++++++++++++++++++++++++
#+ Quiet intersection
#++++++++++++++++++++++++++++++++++++
st_intersection_quietly <- purrr::quietly(sf::st_intersection)
st_centroid_quietly <- function(...) {
  suppressWarnings(sf::st_centroid(...))
}

#++++++++++++++++++++++++++++++++++++
#+ Rotating sf object
#++++++++++++++++++++++++++++++++++++
st_rotate <- function(x, radians) {
  rot_matrix <- matrix(c(cos(radians), sin(radians), -sin(radians), cos(radians)), 2, 2)
  crs <- st_crs(x)

  center <- sf::st_centroid(sf::st_as_sfc(sf::st_bbox(x)))

  x_rotated <- (sf::st_geometry(x) - center) * rot_matrix + center

  # put new geometry in the same sf object
  sf::st_geometry(x) <- x_rotated
  # replace lost crs
  sf::st_crs(x) <- crs
  return(x)
}

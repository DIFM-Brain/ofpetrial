#' Assign rates to the plots of experimental plots
#'
#' This functions assign input rates for the plots created by make_exp_plots() according to the rate designs specified by the user in rate_info, which can be created by prep_rateingle().
#'
#' @param exp_data experiment plots created by make_exp_plots()
#' @param rate_info rate information created by prep_rate()
#' @returns trial design as sf (experiment plots with rates assigned)
#' @import data.table
#' @export
#' @examples
#' #--- load experiment plots made by make_exp_plots() ---#
#' data(exp_data)
#' exp_data
#'
#' #--- load rate information ---#
#' data(rate_info)
#' rate_info
#'
#' #--- assign rates ---#
#' td <- assign_rates(exp_data, rate_info)
#'
#' #--- visualization of the assigned rates ---#
#' viz(td)
assign_rates <- function(exp_data, rate_info) {
  if ("data.frame" %in% class(rate_info)) {
    input_trial_data_with_rates <-
      rate_info %>%
      dplyr::left_join(exp_data, ., by = "input_name") %>%
      dplyr::rowwise()
  } else if ("list" %in% class(rate_info)) {
    input_trial_data_with_rates <-
      data.table::rbindlist(rate_info, fill = TRUE) %>%
      dplyr::left_join(exp_data, ., by = "input_name") %>%
      dplyr::rowwise()
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Prepare rate information
  #++++++++++++++++++++++++++++++++++++

  # !===========================================================
  # ! Assign rates
  # !===========================================================
  # exp_sf <- input_trial_data_with_rates$exp_plots[[1]]
  # exp_plots <- input_trial_data_with_rates$exp_plots[[1]]
  # rates_data <- input_trial_data_with_rates$rates_data[[1]]
  # rank_seq_ws <- input_trial_data_with_rates$rank_seq_ws[[1]]
  # rank_seq_as <- input_trial_data_with_rates$rank_seq_as[[1]]
  # design_type <- input_trial_data_with_rates$design_type[[1]]

  if (nrow(input_trial_data_with_rates) == 2) {
    #++++++++++++++++++++++++++++++++++++
    #+ two-input case
    #++++++++++++++++++++++++++++++++++++
    # if two-input experiment and the experimental plots are identical, then check if joint processing is necessary
    num_rates_ls <- input_trial_data_with_rates$num_rates %>% unlist()

    # if the number of rates of an input is the multiple of the other
    multiple_of_the_other <- (num_rates_ls[which.max(num_rates_ls)] %% num_rates_ls[-which.max(num_rates_ls)] == 0)

    # are the geometries identical?
    geometry_identical <-
      if (!length(input_trial_data_with_rates$exp_plots[[1]]$geometry) == length(input_trial_data_with_rates$exp_plots[[2]]$geometry)) {
        # if the numbers of plots are different
        FALSE
      } else {
        # if the numbers of plots are the same and they are identical
        all(input_trial_data_with_rates$exp_plots[[1]]$geometry == input_trial_data_with_rates$exp_plots[[2]]$geometry)
      }

    # whether any of rank_seq_as and rank_seq_es is specified
    no_rank_seq_specified <- all(lapply(input_trial_data_with_rates$rank_seq_as, is.null) %>% unlist()) & all(lapply(input_trial_data_with_rates$rank_seq_ws, is.null) %>% unlist())

    # are both design types "ls" (not random)?
    both_ls <- all(input_trial_data_with_rates$design_type == "ls" | is.na(input_trial_data_with_rates$design_type))

    # whether you need to crete designs separately
    require_joint_designing <- geometry_identical & both_ls & multiple_of_the_other & no_rank_seq_specified

    if (require_joint_designing) {
      if (all(num_rates_ls == 2)) { # special case of 2 by 2

        plots_with_rates_assigned <- make_design_for_2_by_2(input_trial_data_with_rates)
      } else {
        #--- make design for the first input ---#
        design_first_input <-
          assign_rates_by_input(
            exp_sf = input_trial_data_with_rates$exp_plots[[1]],
            rates_data = input_trial_data_with_rates$rates_data[[1]],
            rank_seq_ws = input_trial_data_with_rates$rank_seq_ws[[1]],
            rank_seq_as = input_trial_data_with_rates$rank_seq_as[[1]],
            design_type = input_trial_data_with_rates$design_type[[1]],
            rate_jump_threshold = input_trial_data_with_rates$rate_jump_threshold[[1]]
          )

        design_second_input <-
          get_design_for_second(
            input_trial_data = input_trial_data_with_rates[2, ],
            first_design = design_first_input,
            rate_jump_threshold = input_trial_data_with_rates$rate_jump_threshold[[2]]
          )

        input_trial_data_with_rates$experiment_design <- list(design_first_input, design_second_input)

        plots_with_rates_assigned <-
          input_trial_data_with_rates %>%
          dplyr::mutate(experiment_design = list(
            experiment_design %>%
              dplyr::select(rate, strip_id, plot_id) %>%
              dplyr::mutate(type = "experiment")
          ))

        #--- check if each rate-combination has enough replications ---#
        check_replicates_two_input(plots_with_rates_assigned, geometry_identical)
      }
    } else { # two-input, but can just design them independently

      if ((geometry_identical & both_ls & multiple_of_the_other) & !no_rank_seq_specified) {
        message("You specified either rank_seq_ws and/or rank_seq_as for at least one of the inputs and the two trial designs are created independently without any consideration for even number of replications and balanced spatial distribution of the joint rate combinations to respect the specified rank sequence(s). Please use ofpetrial::make_trial_report() to check these aspected of the trial designs created. You could alternatively leave those arguments empty, and then the code will generate trial designs that are free of the above problems.")
      }

      plots_with_rates_assigned <-
        input_trial_data_with_rates %>%
        dplyr::mutate(experiment_design = list(
          assign_rates_by_input(
            exp_sf = exp_plots,
            rates_data = rates_data,
            rank_seq_ws = rank_seq_ws,
            rank_seq_as = rank_seq_as,
            design_type = design_type,
            rate_jump_threshold = rate_jump_threshold
          ) %>%
            dplyr::select(rate, strip_id, plot_id) %>%
            dplyr::mutate(type = "experiment")
        ))

      #--- check if each rate-combination has enough replications ---#
      check_replicates_two_input(plots_with_rates_assigned, geometry_identical)
    }
  } else {
    #++++++++++++++++++++++++++++++++++++
    #+ single-input case
    #++++++++++++++++++++++++++++++++++++
    plots_with_rates_assigned <-
      input_trial_data_with_rates %>%
      dplyr::mutate(experiment_design = list(
        assign_rates_by_input(
          exp_sf = exp_plots,
          rates_data = rates_data,
          rank_seq_ws = rank_seq_ws,
          rank_seq_as = rank_seq_as,
          design_type = design_type,
          rate_jump_threshold = rate_jump_threshold
        ) %>%
          dplyr::select(rate, strip_id, plot_id) %>%
          dplyr::mutate(type = "experiment")
      ))

    #--- check if each rate has enough replications ---#
    check_replicates_single_input(plots_with_rates_assigned)
  }

  trial_design <-
    plots_with_rates_assigned %>%
    dplyr::mutate(headland = list(
      dplyr::mutate(headland, rate = gc_rate) %>%
        dplyr::select(rate) %>%
        dplyr::mutate(strip_id = NA, plot_id = NA, type = "headland")
    )) %>%
    dplyr::mutate(input_type = list(
      dplyr::case_when(
        input_name == "seed" ~ "S",
        input_name %in% c("uan28", "uan32", "urea", "NH3", "cover") ~ "N",
        input_name %in% "chicken_manure" ~ "M",
        input_name %in% "forage_pea" ~ "P",
        # === needs to change this ===#
        input_name %in% c("potash", "K") ~ "K",
        input_name == "KCL" ~ "C",
        input_name == "boron" ~ "B"
      )
    )) %>%
    dplyr::mutate(trial_design = list(
      rbind(
        experiment_design,
        headland
      ) %>%
        sf::st_transform(4326)
    )) %>%
    dplyr::mutate(trial_design = list(
      if ("tgts_K" %in% names(trial_design)) {
        dplyr::mutate(trial_design, tgts = trial_design$tgts_K * 1000) %>%
          dplyr::relocate(tgts_K, tgts)
      } else {
        trial_design
      }
    )) %>%
    dplyr::ungroup()



  return(trial_design)
}

#' Assign rates to the plots of experimental plots for a single input based on existing trial designs created by assign_rates()
#'
#' This functions assign input rates for the plots created by make_exp_plots() for a single input according to the rate design specified by the user in rate_info. It assigns rates to the input so that the resulting design avoids significant correlation with the rate of another input specified as existing_design.
#'
#' @param exp_data experiment plots created by make_exp_plots()
#' @param rate_info rate information created by prep_rate()
#' @param existing_design trial design of another input created with assign_rates()
#' @returns trial design as sf (experiment plots with rates assigned)
#' @import data.table
#' @export
#' @examples
#' #--- load experiment plots made by make_exp_plots() ---#
#' data(td_single_input)
#' exp_data
#'
#'seed_plot_info <-
#'  prep_plot(
#'    input_name = "seed",
#'    unit_system = "imperial",
#'    machine_width = 60,
#'    section_num = 24,
#'    harvester_width = 30,
#'    plot_width = 30
#'  )
#'
#'exp_data <-
#'  make_exp_plots(
#'    input_plot_info = seed_plot_info,
#'    boundary_data = system.file("extdata", "boundary-simple1.shp", package = "ofpetrial"),
#'    abline_data = system.file("extdata", "ab-line-simple1.shp", package = "ofpetrial"),
#'    abline_type = "free"
#'  )
#'
#'seed_rate_info <-
#'  prep_rate(
#'    plot_info = seed_plot_info,
#'    gc_rate = 32000,
#'    unit = "seed",
#'    min_rate = 16000,
#'    max_rate = 40000,
#'    num_rates = 5,
#'    design_type = "ls"
#'  )
#'
#'assign_rates_conditional(
#'  exp_data = exp_data, 
#'  rate_info = seed_rate_info, 
#'  existing_design = td_single_input
#')

assign_rates_conditional <- function(exp_data, rate_info, existing_design) {
  if ("data.frame" %in% class(rate_info)) {
    input_trial_data_with_rates <-
      rate_info %>%
      dplyr::left_join(exp_data, ., by = "input_name") %>%
      dplyr::rowwise()
  } else if ("list" %in% class(rate_info)) {
    stop("Error: you cannot assign rates for two inputs using this function.")
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Existing trial designs
  #++++++++++++++++++++++++++++++++++++

  if (nrow(existing_design) == 2) {
    td_geometry_identical <-
      if (!length(existing_design$exp_plots[[1]]$geometry) == length(existing_design$exp_plots[[2]]$geometry)) {
        # if the numbers of plots are different
        FALSE
      } else {
        # if the numbers of plots are the same and they are identical
        all(existing_design$exp_plots[[1]]$geometry == existing_design$exp_plots[[2]]$geometry)
      }

    if (!td_geometry_identical) {
      stop("It seems you are trying to add a third input. This package does not accommodate a three-input-type experiment.")
    }

    first_design <-
      existing_design$trial_design[[1]] %>%
      dplyr::rename(rate_1 = rate) %>%
      left_join(
        .,
        existing_design$trial_design[[2]] %>%
          sf::st_drop_geometry() %>%
          dplyr::select(rate, strip_id, plot_id) %>%
          dplyr::rename(rate_2 = rate),
        by = c("plot_id", "strip_id")
      ) %>%
      data.table() %>%
      .[, rate_rank := .GRP, by = .(rate_1, rate_2)] %>%
      .[type != "headland", ]
  } else {
    first_design <-
      existing_design$trial_design[[1]] %>%
      data.table() %>%
      .[, rate_rank := .GRP, by = .(rate)]
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Check if joint consideration is necessary
  #++++++++++++++++++++++++++++++++++++

  num_rates_ls <- c(input_trial_data_with_rates$num_rates, max(first_design$rate_rank)) %>% unlist()

  # if the number of rates of an input is the multiple of the other
  multiple_of_the_other <- (num_rates_ls[which.max(num_rates_ls)] %% num_rates_ls[-which.max(num_rates_ls)] == 0)

  # are the geometries identical?
  geometry_identical <-
    if (!length(input_trial_data_with_rates$exp_plots[[1]]$geometry) == length(first_design$geometry)) {
      # if the numbers of plots are different
      FALSE
    } else {
      # if the numbers of plots are the same and they are identical
      all(input_trial_data_with_rates$exp_plots[[1]]$geometry == first_design$geometry)
    }

  # whether any of rank_seq_as and rank_seq_es is specified
  no_rank_seq_specified <- all(lapply(input_trial_data_with_rates$rank_seq_as, is.null) %>% unlist()) & all(lapply(input_trial_data_with_rates$rank_seq_ws, is.null) %>% unlist())

  # are both design types "ls" (not random)?
  both_ls <- all(input_trial_data_with_rates$design_type == "ls" | is.na(input_trial_data_with_rates$design_type))

  # whether you need to crete designs separately
  require_joint_designing <- geometry_identical & both_ls & multiple_of_the_other & no_rank_seq_specified

  #++++++++++++++++++++++++++++++++++++
  #+ Assign rates
  #++++++++++++++++++++++++++++++++++++
  if (require_joint_designing) {
    #---------------------
    #- If not just create a design independently
    #---------------------

    design_second_input <-
      get_design_for_second(
        input_trial_data = input_trial_data_with_rates,
        first_design = first_design,
        rate_jump_threshold = input_trial_data_with_rates$rate_jump_threshold[[1]]
      )

    input_trial_data_with_rates$experiment_design <- list(design_second_input)

    plots_with_rates_assigned <-
      input_trial_data_with_rates %>%
      dplyr::mutate(experiment_design = list(
        experiment_design %>%
          dplyr::select(rate, strip_id, plot_id) %>%
          dplyr::mutate(type = "experiment")
      ))

  } else {
    #---------------------
    #- If so create a design conditional on the other deign
    #---------------------
    plots_with_rates_assigned <-
      input_trial_data_with_rates %>%
      dplyr::mutate(experiment_design = list(
        assign_rates_by_input(
          exp_sf = exp_plots,
          rates_data = rates_data,
          rank_seq_ws = rank_seq_ws,
          rank_seq_as = rank_seq_as,
          design_type = design_type,
          rate_jump_threshold = rate_jump_threshold
        ) %>%
          dplyr::select(rate, strip_id, plot_id) %>%
          dplyr::mutate(type = "experiment")
      ))
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Complete a trial design
  #++++++++++++++++++++++++++++++++++++
  trial_design <-
    plots_with_rates_assigned %>%
    dplyr::mutate(headland = list(
      dplyr::mutate(headland, rate = gc_rate) %>%
        dplyr::select(rate) %>%
        dplyr::mutate(strip_id = NA, plot_id = NA, type = "headland")
    )) %>%
    dplyr::mutate(input_type = list(
      dplyr::case_when(
        input_name == "seed" ~ "S",
        input_name %in% c("uan28", "uan32", "urea", "NH3", "cover") ~ "N",
        input_name %in% "chicken_manure" ~ "M",
        input_name %in% "forage_pea" ~ "P",
        # === needs to change this ===#
        input_name %in% c("potash", "K") ~ "K",
        input_name == "KCL" ~ "C",
        input_name == "boron" ~ "B"
      )
    )) %>%
    dplyr::mutate(trial_design = list(
      rbind(
        experiment_design,
        headland
      ) %>%
        sf::st_transform(4326)
    )) %>%
    dplyr::mutate(trial_design = list(
      if ("tgts_K" %in% names(trial_design)) {
        dplyr::mutate(trial_design, tgts = trial_design$tgts_K * 1000) %>%
          dplyr::relocate(tgts_K, tgts)
      } else {
        trial_design
      }
    )) %>%
    dplyr::ungroup()

  return(trial_design)
}

# !==================-=========================================
# ! Helper internal functions
# !===========================================================
#* +++++++++++++++++++++++++++++++++++
#* Assign rates (latin and jump-rate-conscious)
#* +++++++++++++++++++++++++++++++++++
# exp_sf <- plots_with_rates_assigned$exp_plots[[1]]
# rates_data <- plots_with_rates_assigned$rates_data[[1]]
# rank_seq_ws <- plots_with_rates_assigned$rank_seq_ws[[1]]
# rank_seq_as <- plots_with_rates_assigned$rank_seq_as[[1]]
# design_type <- plots_with_rates_assigned$design_type[[1]]
# rate_jump_threshold <- plots_with_rates_assigned$rate_jump_threshold[[1]]

assign_rates_by_input <- function(exp_sf, rates_data, rank_seq_ws, rank_seq_as, design_type, rate_jump_threshold) {
  max_plot_id <- max(exp_sf$plot_id)
  max_strip_id <- max(exp_sf$strip_id)
  num_rates <- nrow(rates_data)

  if (is.na(design_type)) {
    design_type <- "ls"
  }

  if (design_type == "ls") {
    #---------------------
    #- Preparation
    #---------------------

    if (is.null(rank_seq_ws) & is.null(rank_seq_as)) {
      # if both are missing, then first get rank_seq_ws and then rank_seq_as
      rank_seq_ws <- gen_basic_rank_ws(num_rates, rate_jump_threshold)
      rank_seq_as <- get_starting_rank_as_ls(rank_seq_ws)
    } else if (!is.null(rank_seq_ws) & is.null(rank_seq_as)) {
      # if rank_seq_ws was specified, but rank_seq_as is missing
      rank_seq_ws <- rank_seq_ws
      rank_seq_as <- get_starting_rank_as_ls(rank_seq_ws)
    } else if (is.null(rank_seq_ws) & !is.null(rank_seq_as)) {
      # if rank_seq_as was specified, but rank_seq_ws is missing
      rank_seq_as <- rank_seq_as
      rank_seq_ws <- gen_basic_rank_ws(num_rates, rate_jump_threshold)
      message(
        "Note: You specified rank_seq_as yourself. Please study the resulting trial design map and make sure it is satisfactory."
      )
    } else {
      # both specified, do nothing
    }

    full_start_seq_long <-
      rep(
        rank_seq_as,
        ceiling(max_strip_id / num_rates) + 5
      )

    #---------------------
    #- Assign rates
    #---------------------
    exp_sf$rate_rank <- NA
    shift_counter <- 0
    strip_list <- vector(mode = "list", max_strip_id)

    for (i in 1:max(exp_sf$strip_id)) {
      working_strip <- dplyr::filter(exp_sf, strip_id == i)
      start_rank <- full_start_seq_long[i + shift_counter]
      num_plots_ws <- nrow(working_strip)

      if (i == 1) {
        rate_ranks <-
          rep(
            get_rank_ws_for_strip(start_rank, rank_seq_ws),
            ceiling(num_plots_ws / length(rank_seq_ws))
          ) %>%
          .[1:num_plots_ws]
      } else {
        previous_strip <- strip_list[[i - 1]]

        rate_ranks <-
          rep(
            get_rank_ws_for_strip(start_rank, rank_seq_ws),
            ceiling(num_plots_ws / length(rank_seq_ws))
          ) %>%
          .[1:num_plots_ws]

        neighbor_rate_ranks <-
          suppressWarnings(st_distance(st_centroid_quietly(working_strip), st_centroid_quietly(previous_strip))) %>%
          apply(1, which.min) %>%
          previous_strip[., ] %>%
          pull(rate_rank)

        duplication_score <- mean(rate_ranks == neighbor_rate_ranks)

        if (duplication_score > 0.5) {
          # create a new start_rank sequence that starts with a value except the current one
          shift_counter <- shift_counter + 1
          start_rank <- full_start_seq_long[i + shift_counter]

          # determine rates
          rate_ranks <-
            rep(
              get_rank_ws_for_strip(start_rank, rank_seq_ws),
              ceiling(num_plots_ws / length(rank_seq_ws))
            ) %>%
            .[1:num_plots_ws]
        }
      }
      # exp_sf <- dplyr::mutate(exp_sf, rate_rank = ifelse(strip_id == i, rate_ranks, rate_rank))
      working_strip$rate_rank <- rate_ranks
      strip_list[[i]] <- working_strip
    }

    return_data <-
      data.table::rbindlist(strip_list) %>%
      st_as_sf() %>%
      left_join(., rates_data, by = "rate_rank")

    # ggplot(return_data) +
    #   geom_sf(aes(fill = factor(rate))) +
    #   scale_fill_viridis_d()
  } else if (design_type == "rb") {
    if (!is.null(rank_seq_ws)) {
      message(
        'Note: rank_seq_ws is ignored when design_type = "rb"'
      )
    }
    if (!is.null(rank_seq_as)) {
      message(
        'Note: rank_seq_as is ignored when design_type = "rb"'
      )
    }

    return_data <-
      exp_sf %>%
      data.table::data.table() %>%
      .[, block_row := ((plot_id - 1) %/% num_rates + 1)] %>%
      .[, block_col := ((strip_id - 1) %/% num_rates + 1)] %>%
      .[, block_id := paste0(block_row, "-", block_col)] %>%
      dplyr::nest_by(block_id) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(data = list(
        dplyr::mutate(data, rate_rank = get_rank_for_rb(num_rates, data))
      )) %>%
      tidyr::unnest(cols = c(data)) %>%
      data.table::data.table() %>%
      rates_data[., on = "rate_rank"] %>%
      .[, block := .GRP, by = block_id] %>%
      .[, `:=`(block_id = NULL, block_row = NULL, block_col = NULL)] %>%
      sf::st_as_sf()
  } else if (design_type == "str") {
    if (!is.null(rank_seq_ws)) {
      message(
        "Note: You specified rank_seq_ws. However, it is irrelevant for strip design and it is ignored."
      )
    }
    if (is.null(rank_seq_as)) {
      start_rank_as <- get_starting_rank_as(num_rates)
    } else {
      start_rank_as <- rank_seq_as
    }

    #--- get the starting ranks across strips for the field---#
    assigned_rates_data <-
      rep(
        start_rank_as,
        ceiling(max_strip_id / length(start_rank_as)) + 1
      ) %>%
      .[1:(max_strip_id + 1)] %>%
      data.table::data.table(rate_rank = .) %>%
      .[, strip_id := 1:.N] %>%
      rates_data[., on = "rate_rank"] %>%
      .[, .(strip_id, rate)]

    return_data <-
      dplyr::left_join(
        exp_sf,
        assigned_rates_data,
        by = "strip_id"
      )
  } else if (design_type == "rstr") {
    if (!is.null(rank_seq_ws)) {
      message(
        "Note: You specified rank_seq_ws. However, it is irrelevant for randomized strip design and it is ignored."
      )
    }
    if (is.null(rank_seq_as)) {
      message(
        "Note: You specified rank_seq_as. However, it is irrelevant for randomized strip design and it is ignored."
      )
    }

    start_rank_as <-
      replicate(ceiling(max_strip_id / num_rates), sample(rep(1:num_rates), num_rates)) %>%
      as.vector()

    #--- get the starting ranks across strips for the field---#
    assigned_rates_data <-
      rep(
        start_rank_as,
        ceiling(max_strip_id / length(start_rank_as)) + 1
      ) %>%
      .[1:(max_strip_id + 1)] %>%
      data.table::data.table(rate_rank = .) %>%
      .[, strip_id := 1:.N] %>%
      rates_data[., on = "rate_rank"] %>%
      .[, .(strip_id, rate)]

    return_data <-
      dplyr::left_join(
        exp_sf,
        assigned_rates_data,
        by = "strip_id"
      )
  } else if (design_type == "sparse") {
    #* gc_rate is always ranked 1

    #--- get the rate rank sequence within a strip---#
    if (is.null(rank_seq_ws)) {
      basic_seq <- gen_basic_rank_ws_sparse(num_rates, design_type)
    } else {
      basic_seq <- rank_seq_ws
    }

    #--- get the starting ranks across strips for the field---#
    if (is.null(rank_seq_as)) {
      start_rank_as <- get_starting_rank_as(num_rates - 1) + 1
    } else {
      start_rank_as <- rank_seq_as
    }

    # === get the starting ranks across strips for the field ===#
    full_start_seq <- rep(
      start_rank_as,
      ceiling(max_strip_id / num_rates) + 1
    ) %>%
      .[1:max_strip_id]

    assigned_rates_data <-
      data.table::data.table(
        strip_id = 1:max_strip_id,
        start_rank = full_start_seq
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(rate_rank = list(
        rep(
          get_rank_ws_for_strip_sparse(start_rank, basic_seq, strip_id),
          ceiling(max_plot_id / length(basic_seq))
        )
      )) %>%
      tidyr::unnest(rate_rank) %>%
      data.table::data.table() %>%
      .[, dummy := 1] %>%
      .[, plot_id := cumsum(dummy), by = strip_id] %>%
      rates_data[., on = "rate_rank"] %>%
      .[, .(strip_id, plot_id, rate)]

    return_data <-
      dplyr::left_join(
        exp_sf,
        assigned_rates_data,
        by = c("strip_id", "plot_id")
      )
  } else if (design_type == "ejca") { # Extra jump-conscious alternate

    rates_data[, tier := ifelse(rate_rank < median(rate_rank), 1, 2)] %>%
      .[, rank_in_tier := rowid(tier)]

    assigned_rates_data <-
      rates_data %>%
      dplyr::nest_by(tier) %>%
      dplyr::mutate(num_levels = nrow(data)) %>%
      dplyr::mutate(basic_seq = list(
        gen_basic_rank_ws(num_levels, rate_jump_threshold)
      )) %>%
      #--- split the strips to two tiers in an alternate fashion ---#
      dplyr::mutate(strip_plot_data = list(
        if (tier == 1) {
          dplyr::filter(exp_sf, (strip_id %% 2) == 1) %>%
            data.table::data.table() %>%
            .[, .(strip_id, plot_id)] %>%
            unique(by = c("strip_id", "plot_id"))
        } else {
          dplyr::filter(exp_sf, (strip_id %% 2) == 0) %>%
            data.table::data.table() %>%
            .[, .(strip_id, plot_id)] %>%
            unique(by = c("strip_id", "plot_id"))
        }
      )) %>%
      #--- create new strip id within tier (called group_in_strip) ---#
      dplyr::mutate(strip_plot_data = list(
        strip_plot_data[, group_in_strip := .GRP, by = strip_id]
      )) %>%
      #--- reverse the order of plots alternately---#
      dplyr::mutate(strip_plot_data = list(
        lapply(
          unique(strip_plot_data$strip_id),
          function(x) {
            temp_data <- strip_plot_data[strip_id == x, ]
            if ((unique(temp_data$group_in_strip) %% 2) == 0) {
              temp_data <- temp_data[order(rev(plot_id)), ]
            }
            return(temp_data)
          }
        ) %>%
          data.table::rbindlist()
      )) %>%
      dplyr::mutate(strip_plot_data = list(
        strip_plot_data[, rank_in_tier :=
          rep(basic_seq, ceiling(nrow(strip_plot_data) / num_levels))[1:nrow(strip_plot_data)]]
      )) %>%
      dplyr::mutate(rate_data = list(
        data.table::data.table(data)[strip_plot_data[, .(strip_id, plot_id, rank_in_tier)], on = "rank_in_tier"]
      )) %>%
      purrr::pluck("rate_data") %>%
      data.table::rbindlist()

    return_data <-
      dplyr::left_join(
        exp_sf,
        assigned_rates_data,
        by = c("strip_id", "plot_id")
      )
  }

  return(return_data)
}

assign_rate_rank_by_strip <- function(rank_seq_ws, rank_seq_as, num_strips, max_plot_num) {
  full_start_seq <-
    rep(
      rank_seq_as,
      ceiling(num_strips / length(rank_seq_as)) + 1
    ) %>%
    .[1:num_strips]

  assigned_rate_rank_data <-
    data.table::data.table(
      strip_id = 1:num_strips,
      start_rank = full_start_seq
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rate_rank = list(
      rep(
        get_rank_ws_for_strip(start_rank, rank_seq_ws),
        ceiling(max_plot_num / length(rank_seq_ws))
      )
    )) %>%
    tidyr::unnest(rate_rank) %>%
    data.table::data.table() %>%
    .[, dummy := 1] %>%
    .[, plot_id := cumsum(dummy), by = strip_id] %>%
    .[, .(strip_id, plot_id, rate_rank)]

  return(assigned_rate_rank_data)
}
#++++++++++++++++++++++++++++++++++++
#+ Utility functions that helps assign_rates_by_input()
#++++++++++++++++++++++++++++++++++++
# It generates basic within-strip rate rank sequence given the number of rates in a way rate jumps are under the rate_jump_threshold
# It is probably better if rates go up and down alternately instead of increasing and then decreasing (measured by zigzag_score). Having low rate_jump_threshold sacrifices zigzagability.

gen_basic_rank_ws <- function(num_rates, rate_jump_threshold = NA) {
  #--- function to calculate cumulative sum ---#
  rsum.cumsum <- function(x, n = 3L) tail(cumsum(x) - cumsum(c(rep(0, n), head(x, -n))), -n + 1)

  if (is.na(rate_jump_threshold)) {
    rate_jump_threshold <- ceiling(num_rates / 2)
  }

  if (num_rates >= 9) {
    # if num_rates are too high, the permutation takes so long. So, we just go simiple here.
    if (num_rates %% 2 == 0) { # even
      return_basic_seq <- c(seq(1, num_rates, by = 2), seq(num_rates, 2, by = -2))
    } else { # odd
      return_basic_seq <- c(seq(1, num_rates, by = 2), seq(num_rates - 1, 2, by = -2))
    }
  } else if (num_rates == 2) {
    return_basic_seq <- c(1, 2)
  } else {
    mat <- return_permutations(1:num_rates) %>% do.call(rbind, .)
    dif_mat <- mat - cbind(mat[, 2:num_rates], mat[, 1])
    max_rate_jump <- apply(dif_mat, 1, \(x) max(abs(x)))
    jump_score <- apply(dif_mat, 1, \(x) sum(abs(x)^2))
    zigzag_score <- apply(dif_mat, 1, \(x) sum(abs(rsum.cumsum(x, n = 2)))^2)
    total_score <- jump_score + zigzag_score
    which_satisfy_rate_jump <- max_rate_jump <= rate_jump_threshold

    best_id <-
      data.table(
        id = 1:length(jump_score),
        total_score = total_score,
        which_satisfy_rate_jump = max_rate_jump <= rate_jump_threshold
      ) %>%
      .[which_satisfy_rate_jump == TRUE, ] %>%
      .[which.min(total_score), id]

    return_basic_seq <- mat[best_id, ]
  }

  return(return_basic_seq)
}

gen_basic_rank_ws_sparse <- function(length, design_type) {
  if (length %% 2 == 0) { # even
    return_basic_seq <- c(seq(1, length, by = 2), seq(length, 2, by = -2))
  } else { # odd
    return_basic_seq <- c(seq(1, length, by = 2), seq(length - 1, 2, by = -2))
  }

  return_basic_seq <- return_basic_seq[-1]
  for (i in (seq(1, 2 * length(return_basic_seq) - 1, by = 2))) {
    return_basic_seq <- append(return_basic_seq, 1, after = i)
  }
  return(return_basic_seq)
}


# generate sequence of rate ranks within a strip given the starting rate rank (starting_rate_rank)
get_rank_ws_for_strip <- function(starting_rate_rank, basic_seq) {
  max_rank <- length(basic_seq)
  start_position <- which(basic_seq == starting_rate_rank)

  f_seq <- start_position:max_rank
  s_seq <- 1:start_position

  return_rank <- basic_seq[c(f_seq, s_seq) %>% unique()]

  return(return_rank)
}

# generate sequence of rate ranks within a strip given the starting rate rank specifically for the sparse design (starting_rate_rank)
get_rank_ws_for_strip_sparse <- function(starting_rate_rank, basic_seq, strip_id) {
  return_rank <- get_rank_ws_for_strip(starting_rate_rank, basic_seq)

  if (strip_id %% 2 == 0) {
    return_rank <- append(1, return_rank[-length(return_rank)])
  }

  return(return_rank)
}

# Get sequence of rate ranks across strips based on the number of rates. This is used for design_type == "str" and "sparse"
get_starting_rank_as <- function(num_levels) {
  if (num_levels > 1) {
    return_seq <-
      lapply(
        return_permutations(1:num_levels),
        \(seq){
          score <- sum((seq[1:num_levels] - c(seq[2:num_levels], seq[1]))^2) / num_levels

          results_table <-
            data.table::data.table(
              seq = list(seq),
              score = score
            )

          return(results_table)
        }
      ) %>%
      data.table::rbindlist() %>%
      .[score == max(score), ] %>%
      #--- pick one from the remaining options randomly ---#
      .[sample(nrow(.), 1), seq] %>%
      .[[1]]
  } else {
    return_seq <- 1
  }

  return(return_seq)
}

# Get sequence of rate ranks across strips based on the number of rates. This is used for design_type == "ls". It checks changes in rates across strips (columns) for all the rows and avoid designs that has gradual changes in rate rank across strips.

get_starting_rank_as_ls <- function(rank_seq_ws) {
  num_rates <- length(rank_seq_ws)

  if (num_rates <= 3) { # no degrees of freedom (does not matter which)
    rank_seq_as <- sample(1:num_rates, num_rates)
  } else if (num_rates >= 9) {
    # Since the number of rates are too large, we give up finding the best.

    temp_seq <- 1:num_rates
    rank_seq_as <- rep(0, num_rates)

    #--- even position ---#
    # reverse the values so that we have up and down in the resulting sequence
    rank_seq_as[(temp_seq %% 2 == 0)] <- rev(temp_seq[(temp_seq %% 2 == 0)])

    #--- odd position ---#
    rank_seq_as[(temp_seq %% 2 != 0)] <- temp_seq[(temp_seq %% 2 != 0)]
  } else {
    rank_seq_as <-
      lapply(
        return_permutations(1:num_rates),
        \(seq) {
          # left to right (direction of machine)
          mat_list <-
            lapply(
              seq,
              \(x) {
                get_rank_ws_for_strip(x, rank_seq_ws)
              }
            )
          mat <- do.call(rbind, mat_list)
          mat_dif <- mat - rbind(mat[2:num_rates, ], mat[1, ])
          mat_dif_diag_up <- mat - kronecker(matrix(1, 2, 2), mat)[2:(num_rates + 1), num_rates:(2 * num_rates - 1)]
          mat_dif_diag_down <- mat - kronecker(matrix(1, 2, 2), mat)[2:(num_rates + 1), 2:(num_rates + 1)]

          results_table <-
            data.table::data.table(
              seq = list(seq),
              # mat = list(mat_dif),
              #--- count the number of changes of 1 ---#
              check_1_horizontal = max(apply(mat_dif, 2, \(x) sum(abs(x) == 1))),
              check_0_diagonal_up = max(apply(mat_dif_diag_up, 2, \(x) sum(abs(x) == 0))),
              check_0_diagonal_down = max(apply(mat_dif_diag_down, 2, \(x) sum(abs(x) == 0))),
              score_horizontal = abs(mat_dif) %>% mean(),
              score_diagonal = (abs(mat_dif_diag_up) + abs(mat_dif_diag_down)) %>% mean()
            ) %>%
            .[, score := score_horizontal + score_diagonal]

          return(results_table)
        }
      ) %>%
      data.table::rbindlist() %>%
      #--- pick the sequence that would result in the smallest numbers of gradual changes ---#
      .[check_1_horizontal <= (min(check_1_horizontal) + 2), ] %>%
      .[check_0_diagonal_up < num_rates, ] %>%
      .[check_0_diagonal_down < num_rates, ] %>%
      #--- pick the one with the higest score (typically exactly the same for all the options) ---#
      .[score == max(score), ] %>%
      #--- pick one from the remaining options randomly ---#
      .[sample(nrow(.), 1), seq] %>%
      .[[1]]
  }

  return(rank_seq_as)
}

# get_starting_rank_as_ls <- function(rank_seq_ws) {

#   num_rates <- length(rank_seq_ws)

#   seq <-
#     lapply(
#       return_permutations(1:num_rates),
#       \(seq) {
#         mat_list <-
#           lapply(
#             seq,
#             \(x) {
#               get_rank_ws_for_strip(x, rank_seq_ws)
#             }
#           )
#         mat <- do.call(rbind, mat_list)
#         mat_lag <- rbind(mat[2:num_rates, ], mat[1, ])
#         mat_dif <- mat - mat_lag

#         results_table <-
#           data.table::data.table(
#             seq = list(seq),
#             # mat = list(mat_dif),
#             #--- count the number of changes of 1 ---#
#             check_1 = max(apply(mat_dif, 2, \(x) sum(x == 1))),
#             #--- count the number of changes of -1 ---#
#             check_n1 = max(apply(mat_dif, 2, \(x) sum(x == -1))),
#             score = abs(mat_dif) %>% mean()
#           )

#         return(results_table)
#       }
#     ) %>%
#     data.table::rbindlist() %>%
#     #--- pick the sequence that would result in smallest numbers of gradual changes ---#
#     .[(check_1 + check_n1) == min(check_1 + check_n1), ] %>%
#     #--- pick the one with the higest score (typically exactly the same for all the options) ---#
#     .[score == max(score), ] %>%
#     #--- pick one from the remaining options randomly ---#
#     .[sample(nrow(.), 1), seq] %>%
#     .[[1]]

#   return(seq)
# }

get_rank_for_rb <- function(num_rates, data) {
  n_plot <- nrow(data)
  n_comp_block <- n_plot %/% num_rates
  n_plots_remaining <- n_plot %% num_rates
  if (n_comp_block > 0) {
    rate_rank_ls <-
      c(
        c(replicate(n_comp_block, sample(1:num_rates, num_rates, replace = FALSE))),
        sample(1:num_rates, n_plots_remaining, replace = FALSE)
      )
  } else {
    rate_rank_ls <- sample(1:num_rates, n_plots_remaining, replace = FALSE)
  }

  return(rate_rank_ls)
}




#++++++++++++++++++++++++++++++++++++
#+ Find the design for the second input
#++++++++++++++++++++++++++++++++++++
# This function is used internally in assign_rates().

get_design_for_second <- function(input_trial_data, first_design, rate_jump_threshold = NA) {
  #++++++++++++++++++++++++++++++++++++
  #+ Prepare basic information for the second input
  #++++++++++++++++++++++++++++++++++++
  trial_data_second <- input_trial_data
  second_design <- trial_data_second$exp_plots[[1]]
  rates_data_second <- trial_data_second$rates_data[[1]]
  num_rates <- nrow(rates_data_second)
  num_plots <- nrow(second_design)

  #--- table of unique combinations of the first and second input rates ---#
  comb_table <-
    data.table::CJ(
      rate_rank_1 = first_design$rate_rank %>% unique(),
      rate_rank_2 = 1:num_rates
    ) %>%
    .[, cases := 0]

  #--- table of rates to be filled and referenced for dynamic rate assignment ---#
  rate_table <-
    data.table::data.table(second_design)[, .(strip_id, plot_id)] %>%
    .[, rate_rank := 0]

  #--- Find the spatial weights ---#
  centroids_second <- st_centroid_quietly(second_design)

  dist_mat <-
    1 / (st_distance(
      centroids_second,
      centroids_second
    ))

  diag(dist_mat) <- 0

  W <- apply(dist_mat, 1, \(x)  x / sum(x))

  #--- determin rate jump threshold if not specified by the user ---#
  if (is.na(rate_jump_threshold)) {
    if (num_rates == 2) {
      rate_jump_threshold <- 1 # plays no role
    } else if (num_rates == 3) {
      rate_jump_threshold <- 2 # plays no role
    } else if (num_rates == 4) {
      rate_jump_threshold <- 3 # plays no role
    } else if (num_rates >= 5) {
      rate_jump_threshold <- num_rates - 2 # plays no role
    }
  }

  #++++++++++++++++++++++++++++++++++++
  #+ Find rates
  #++++++++++++++++++++++++++++++++++++
  for (row_index in 1:num_plots) { # loop over the strips

    track_data <-
      data.table::data.table(
        plot_id = round(num_plots * seq(0.1, 1, by = 0.1), digits = 0)
      ) %>%
      .[, index := 1:.N]

    if (row_index %in% round(num_plots * seq(0.1, 1, by = 0.1), digits = 0)) {
      percentage <- track_data[plot_id == row_index, index]
      message(paste(paste0(rep("==", percentage), collapse = ""), percentage * 10, "% complete"))
    }

    #--- find the strip id for the working plot ---#
    working_strip_id <- second_design$strip_id[row_index]
    working_plot_id <- second_design$plot_id[row_index]

    #--- get the sf of the working strip ---#
    working_strip_1st <- dplyr::filter(first_design, strip_id == working_strip_id)
    working_strip_2nd <- dplyr::filter(second_design, strip_id == working_strip_id)

    num_plots_in_strip <- max(working_strip_1st$plot_id)

    if (working_strip_id == 1) { # the first strip
      #--- rate of the first design for the working plot ---#
      rate_rank_1st <- data.table::data.table(working_strip_1st)[plot_id == working_plot_id, rate_rank]

      #---------------------
      #- Find the rate rank
      #---------------------
      if (working_plot_id == 1) { # for the very first plot
        rate_rank_2nd <- sample(1:num_rates, 1) # just pick a rate randomly
      } else {
        rate_rank_2nd_prev <- rate_table[row_index - 1, rate_rank]
        rate_rank_2nd <-
          #--- cannot take the save value as the last plot and the closeset plot in the previous strip  ---#
          data.table::copy(comb_table)[rate_rank_2 != rate_rank_2nd_prev & rate_rank_1 == rate_rank_1st, ] %>%
          #--- does not allow rate rank jumps of more than rate_jump_threshold (e.g., 1 -> 4) ---#
          .[abs(rate_rank_2 - rate_rank_2nd_prev) <= rate_jump_threshold, ] %>%
          .[cases == min(cases), ] %>%
          .[sample(1:.N, 1), rate_rank_2]
      }

      #---------------------
      #- Record the results
      #---------------------
      #--- record the chosen rate ---#
      rate_table[row_index, rate_rank := rate_rank_2nd]

      #--- update the combination table ---#
      comb_table <-
        update_comb_table(
          comb_table,
          rate_rank_first = rate_rank_1st,
          rate_rank_second = rate_rank_2nd
        )
    } else { # after the first strip
      previous_strip_2nd <- dplyr::filter(second_design, strip_id == working_strip_id - 1)

      working_plot_2nd <- dplyr::filter(working_strip_2nd, plot_id == working_plot_id)

      closest_plot_id_in_the_previous_strip <- previous_strip_2nd[which.min(st_distance(st_centroid_quietly(working_plot_2nd), st_centroid_quietly(previous_strip_2nd))), ]

      #---------------------
      #- Find the rate ranks for narrowing down the options
      #---------------------
      #--- rate_rank of the closest plot in the previous strip ---#
      rates_list <- list()
      rates_list$rate_rank_2nd_nb <-
        rate_table[
          plot_id == closest_plot_id_in_the_previous_strip$plot_id &
            strip_id == closest_plot_id_in_the_previous_strip$strip_id,
          rate_rank
        ]

      #--- rate of the 1st design of the working plot ---#
      rates_list$rate_rank_1st <- data.table::data.table(first_design)[row_index, rate_rank]
      rates_list$rate_rank_2nd_prev <- rate_table[row_index - 1, rate_rank]

      #---------------------
      #- Find the rate rank
      #---------------------
      rate_rank_2nd <- find_rate(row_index, working_plot_id, rates_list, comb_table, rate_table, W)

      #---------------------
      #- Record the results
      #---------------------
      #--- record the chosen rate ---#
      rate_table[row_index, rate_rank := rate_rank_2nd]

      #--- update the combination table ---#
      comb_table <-
        update_comb_table(
          comb_table,
          rate_rank_first = rates_list$rate_rank_1st,
          rate_rank_second = rate_rank_2nd
        )
    }
  }

  trial_design <-
    left_join(second_design, rate_table, by = c("strip_id", "plot_id")) %>%
    left_join(rates_data_second, rate_table, by = c("rate_rank"))

  return(trial_design)
}
#++++++++++++++++++++++++++++++++++++
#+ Find the rate taking into account local spatial variability and rate jumps
#++++++++++++++++++++++++++++++++++++
# This function finds the rate taking into account local spatial variability and rate jumps for the second input when the first and second inputs share exactly the same experimental plots.
# This function is used internally in get_design_for_second().

find_rate <- function(row_index, working_plot_id, rates_list, comb_table, rate_table, W, rate_jump_threshold = 3) {
  #--- Limit the options ---#
  if (working_plot_id == 1) {
    options <-
      comb_table[rate_rank_2 != rates_list$rate_rank_2nd_nb & rate_rank_1 == rates_list$rate_rank_1st, ]
  } else {
    options <-
      comb_table[rate_rank_2 != rates_list$rate_rank_2nd_prev & rate_rank_2 != rates_list$rate_rank_2nd_nb & rate_rank_1 == rates_list$rate_rank_1st, ]
  }

  #--- find the rate based on local variability score ---#
  score_seq <-
    lapply(options$rate_rank_2, \(x) {
      local_rate_rank_vec <- rate_table[1:(row_index - 1), rate_rank - x]
      if (working_plot_id == 1) {
        #--- if the first plot of a strip ---#
        # do not penalize deviation from the previous strip
        variability_score <- mean(local_rate_rank_vec[1:(row_index - 1)]^2 * W[row_index, 1:(row_index - 1)])
      } else {
        #--- if not the first plot of a strip ---#
        # penalize deviation from the previous strip to avoid an abrupt change in rate in the moving direction
        variability_score <-
          mean(c(
            local_rate_rank_vec[1:(row_index - 2)]^2 * W[row_index, 1:(row_index - 2)],
            -local_rate_rank_vec[row_index - 1]^2 / 2 * W[row_index, row_index - 1]
          ))
      }
    }) %>%
    unlist()

  options$variability_score <- score_seq

  final_options <-
    options %>%
    .[cases %in% c(min(cases), min(cases) + 1), ] %>%
    .[abs(rate_rank_2 - rates_list$ rate_rank_2nd_prev) <= rate_jump_threshold, ]

  if (nrow(final_options) > 0) { # if no options are available
    rate_rank_2nd <-
      final_options[variability_score == max(variability_score), ] %>%
      .[sample(1:.N, 1), rate_rank_2]
  } else {
    rate_rank_2nd <-
      options %>%
      .[abs(rate_rank_2 - rates_list$ rate_rank_2nd_prev) <= rate_jump_threshold, ] %>%
      .[cases %in% c(min(cases), min(cases) + 1), ] %>%
      .[variability_score == max(variability_score), ] %>%
      .[sample(1:.N, 1), rate_rank_2]
  }

  return(rate_rank_2nd)
}

#++++++++++++++++++++++++++++++++++++
#+ Simple routine to update comb_table in get_design_for_second()
#++++++++++++++++++++++++++++++++++++
update_comb_table <- function(comb_table, rate_rank_first, rate_rank_second) {
  comb_table[, cases := ifelse(rate_rank_1 == rate_rank_first & rate_rank_2 == rate_rank_second, cases + 1, cases)]
}

#++++++++++++++++++++++++++++++++++++
#+ Assign rates for 2 by 2 case
#++++++++++++++++++++++++++++++++++++
make_design_for_2_by_2 <- function(input_trial_data_with_rates) {
  #++++++++++++++++++++++++++++++++++++
  #+ Initial setup
  #++++++++++++++++++++++++++++++++++++
  #--- get experimental plots ---#
  # this code is only for the case with identical experimental plots for the two inputs
  exp_sf <- input_trial_data_with_rates$exp_plots[[1]]

  #--- find the number of strips ---#
  num_strips <- max(exp_sf$strip_id)

  #--- max plot id ---#
  max_plot_num <- max(exp_sf$plot_id)

  #++++++++++++++++++++++++++++++++++++
  #+ Assign rates
  #++++++++++++++++++++++++++++++++++++
  #---------------------
  #- First input
  #---------------------
  design_first_input <-
    assign_rate_rank_by_strip(rank_seq_ws = c(1, 2), rank_seq_as = c(1, 2), num_strips, max_plot_num) %>%
    left_join(exp_sf, ., by = c("strip_id", "plot_id")) %>%
    left_join(., input_trial_data_with_rates$rates_data[[1]], by = "rate_rank")

  #---------------------
  #- Second input
  #---------------------
  design_second_input <-
    assign_rate_rank_by_strip(rank_seq_ws = c(1, 2), rank_seq_as = c(1, 1, 2, 2), num_strips, max_plot_num) %>%
    left_join(exp_sf, ., by = c("strip_id", "plot_id")) %>%
    left_join(., input_trial_data_with_rates$rates_data[[2]], by = "rate_rank")

  #++++++++++++++++++++++++++++++++++++
  #+ Combine and return
  #++++++++++++++++++++++++++++++++++++
  input_trial_data_with_rates$experiment_design <- list(design_first_input, design_second_input)

  plots_with_rates_assigned <-
    input_trial_data_with_rates %>%
    dplyr::mutate(experiment_design = list(
      experiment_design %>%
        dplyr::select(rate, strip_id, plot_id) %>%
        dplyr::mutate(type = "experiment")
    ))

  return(plots_with_rates_assigned)
}


#++++++++++++++++++++++++++++++++++++
#+ Check the number of replicates
#++++++++++++++++++++++++++++++++++++
check_replicates_two_input <- function(plots_with_rates_assigned, geometry_identical) {
  exp_1 <- plots_with_rates_assigned$experiment_design[[1]]
  exp_2 <- plots_with_rates_assigned$experiment_design[[2]]

  if (geometry_identical) {
    exp_1_dt <-
      data.table(exp_1)[, .(plot_id, strip_id, rate)] %>%
      setnames("rate", "rate_1")

    exp_2_dt <-
      data.table(exp_2)[, .(plot_id, strip_id, rate)] %>%
      setnames("rate", "rate_2")

    min_rep <-
      exp_1_dt[exp_2_dt, on = .(plot_id, strip_id)] %>%
      .[, .(num_replicates = .N), by = .(rate_1, rate_2)] %>%
      .[, min(num_replicates)]
  } else {
    min_plot_area <-
      min(
        exp_1 %>%
          dplyr::mutate(area = as.numeric(st_area(.))) %>%
          pull(area) %>%
          median(),
        exp_2 %>%
          dplyr::mutate(area = as.numeric(st_area(.))) %>%
          pull(area) %>%
          median()
      )

    min_rep <-
      st_intersection_quietly(
        dplyr::filter(exp_1, type != "Border Buffer"),
        dplyr::filter(exp_2, type != "Border Buffer")
      ) %>%
      .$result %>%
      dplyr::mutate(treatment = paste0(rate, "_", rate.1)) %>%
      dplyr::mutate(area = as.numeric(st_area(.))) %>%
      dplyr::filter(area > min_plot_area * 0.8) %>%
      dplyr::group_by(treatment) %>%
      dplyr::select(treatment) %>%
      dplyr::summarise(num_replicates = dplyr::n()) %>%
      data.frame() %>%
      dplyr::select(treatment, num_replicates) %>%
      dplyr::rename("Treatment Rates" = "treatment") %>%
      dplyr::pull(num_replicates) %>%
      min()
  }

  if (min_rep <= 4) {
    message("Minimum number of treatment replications is less than or equal to 4. Please consider reducing the number of treatment levels or designing a one input trial.")
  }
}

check_replicates_single_input <- function(plots_with_rates_assigned) {
  min_rep <-
    plots_with_rates_assigned$experiment_design[[1]] %>%
    data.table() %>%
    .[, .(num_replicates = .N), by = rate] %>%
    .[, min(num_replicates)]

  if (min_rep <= 4) {
    message("Minimum number of treatment replications is less than or equal to 4. Please consider reducing the number of treatment levels.")
  }
}

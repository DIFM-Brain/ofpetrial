#' Plot information
#'
#' Plot information for creating experiment plots using `make_exp_plot()`. This data exists only for the purpose of making examples in some function references succinct.
#'
#' @format data.frame `plot_info`
#' A data frame with 1 rows and 10 columns:
#' \describe{
#'   \item{input_name}{input name}
#'   \item{unit_system}{measurement system (metric or imperial)}
#'   \item{machine_width}{width of the applicator/planter}
#'   \item{section_num}{number of the sections of the machine}
#'   \item{section_width}{width of a section of the machine}
#'   \item{harvester_width}{width of the harvester}
#'   \item{plot_width}{width of the plots to be made}
#'   \item{headland_length}{length of the headland}
#'   \item{side_length}{length of the side}
#'   \item{min_plot_length}{minimum plot length allowed}
#'   \item{max_plot_length}{maximum plot length allowed}
#' }
"plot_info"

#' Experiment data
#'
#' Data on the experiment created by running the `make_exp_plot()` function, which includes various sf objects (e.g., experiment plots, ab-line, headland, etc). This data exists only for the purpose of making examples in some function references succinct.
#'
#' @format tbl_df tbl data.frame `exp_data`
#' A data frame with 1 rows and 9 columns:
#' \describe{
#'   \item{input_name}{input name}
#'   \item{harvester_width}{width of the harvester}
#'   \item{plot_width}{width of the plots to be made}
#'   \item{field_sf}{field boundary as an sf object}
#'   \item{headland}{headland as an sf object}
#'   \item{exp_plots}{experiment plots as an sf object}
#'   \item{ab_lines}{ab-lines for the applicator/planter as an sf object}
#'   \item{harvest_ab_lines}{ab-lines for the harvester as an sf object}
#'   \item{abline_type}{(character) one of "free", "lock", "none" indicating the way ab-line is (or not) created}
#' }
"exp_data"

#' Rate information
#'
#' Rate information for assigning rates to the experiment plots using the `assign_rates()` function. This data exists only for the purpose of making examples in some function references succinct.
#'
#' @format data.frame `rate_info`
#' A data frame with 1 rows and 7 columns:
#' \describe{
#'   \item{input_name}{input name}
#'   \item{design_type}{type of the trial design to be created}
#'   \item{gc_rate}{normal rate the grower would have used if not running an experiment}
#'   \item{unit}{unit of the input}
#'   \item{rates_data}{data.frame of rates and their ranks}
#'   \item{rank_seq_ws}{vector of the ranking of rates that will repeated within a strip}
#'   \item{rank_seq_as}{vector of the ranking of rates that will repeated as the first rate of the strips}
#' }
"rate_info"

#' Trial design (single-input)
#'
#' Trial design data created by assigning rates to experiment plots running the `assign_rates()` function. This data exists only for the purpose of making examples in some function references succinct.
#'
#' @format tbl_df tbl data.frame `td_single_input`
#' A data frame with 1 rows and 9 columns:
#' \describe{
#'   \item{input_name}{input name}
#'   \item{input_type}{shorthand for the type of the input: "N" for nitrogen, "S" for seed, etc.}
#'   \item{trial_design}{experiment plots with input rats assigned as an sf object}
#'   \item{design_type}{type of the trial design used}
#'   \item{unit}{unit of the input}
#'   \item{abline_type}{(character) one of "free", "lock", "none" indicating the way ab-line is (or not) created}
#'   \item{ab_lines}{ab-lines for the applicator/planter as an sf object}
#'   \item{harvest_ab_lines}{ab-lines for the harvester as an sf object}
#'   \item{field_sf}{field boundary as an sf object}
#'   \item{harvest_width}{width of the harvester}
#' }
"td_single_input"

#' Trial design (two-input)
#'
#' Trial design data created by assigning rates to experiment plots running the `assign_rates()` function. This data exists only for the purpose of making examples in some function references succinct.
#'
#' @format tbl_df tbl data.frame `td_two_input`
#' A data frame with 1 rows and 9 columns:
#' \describe{
#'   \item{input_name}{input name}
#'   \item{input_type}{shorthand for the type of the input: "N" for nitrogen, "S" for seed, etc.}
#'   \item{trial_design}{experiment plots with input rats assigned as an sf object}
#'   \item{design_type}{type of the trial design used}
#'   \item{unit}{unit of the input}
#'   \item{abline_type}{(character) one of "free", "lock", "none" indicating the way ab-line is (or not) created}
#'   \item{ab_lines}{ab-lines for the applicator/planter as an sf object}
#'   \item{harvest_ab_lines}{ab-lines for the harvester as an sf object}
#'   \item{field_sf}{field boundary as an sf object}
#'   \item{harvest_width}{width of the harvester}
#' }
"td_two_input"

#' Trial design (single-input) for a curved field
#'
#' Trial design data created by assigning rates to experiment plots running the `assign_rates()` function. This data exists only for the purpose of making examples in some function references succinct.
#'
#' @format tbl_df tbl data.frame `td_curved`
#' A data frame with 1 rows and 9 columns:
#' \describe{
#'   \item{input_name}{input name}
#'   \item{input_type}{shorthand for the type of the input: "N" for nitrogen, "S" for seed, etc.}
#'   \item{trial_design}{experiment plots with input rats assigned as an sf object}
#'   \item{design_type}{type of the trial design used}
#'   \item{unit}{unit of the input}
#'   \item{abline_type}{(character) one of "free", "lock", "none" indicating the way ab-line is (or not) created}
#'   \item{ab_lines}{ab-lines for the applicator/planter as an sf object}
#'   \item{harvest_ab_lines}{ab-lines for the harvester as an sf object}
#'   \item{field_sf}{field boundary as an sf object}
#'   \item{harvest_width}{width of the harvester}
#' }
"td_curved"

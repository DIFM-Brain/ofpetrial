#' Write trial design files for field implementation
#'
#' Write out all the necessary files to implement the trial design created. Exported files include
#'
#' @param td (tibble) a tibble of a trial design created by applying assign_rate() to experimental plots made by make_exp_plots().
#' @param folder_path (character) path to the folder in which the files will be saved
#' @param td_name (character) file name given to the trial design shape files
#' @param ap_ab_name (character) file name(s) given to the applicator/planter ab-line shape files
#' @param h_ab_name (character) file name given to the harvester ab-line shape files
#' @param zip (logical) Default = FALSE. If TRUE, all the files that are being written will be zipped.
#' @returns nothing
#' @import sf
#' @export
#' @examples
#' #--- load trial design ---#
#' data(td_single_input)
#' write_trial_files(
#'   td = td_single_input,
#'   folder_path = NA,
#'   td_name = "td",
#'   ap_ab_name = "a-abline",
#'   h_ab_name = "h-abline"
#' )
#'
write_trial_files <- function(td, folder_path = NA, zip = FALSE) {
  folder_path <- ifelse(is.na(folder_path), getwd(), folder_path)

  input_name_ls <- td$input_name
  trial_design_ls <- td$trial_design

  purrr::walk2()

  #--- write trial designs ---#
  td_written <- dplyr::reframe(td, purrr::pmap(list(input_name, trial_design, td_name, folder_path), write_td))
  #--- write applicator/planter ab-lines ---#
  ap_ab_written <- dplyr::reframe(td, purrr::pmap(list(input_name, abline_type, ab_lines, ap_ab_name, folder_path), write_ap_abline))
}

# !===========================================================
# ! Helper function
# !===========================================================
write_td <- function(input_name, trial_design, td_name, folder_path) {
  st_write(
    trial_design,
    dsn = folder_path,
    layer =
      ifelse(
        is.na(td_name),
        paste0("trial-design-", input_name),
        td_name
      ),
    driver = "ESRI Shapefile",
    append = FALSE,
    delete_layer = TRUE
  )
}

write_ap_abline <- function(input_name, abline_type, ab_lines, ap_ab_name, folder_path) {
  if (abline_type != "non") {
    st_write(
      ab_lines,
      dsn = folder_path,
      layer =
        ifelse(
          is.na(ap_ab_name),
          paste0("ab-lines-", input_name),
          ap_ab_name
        ),
      driver = "ESRI Shapefile",
      append = FALSE,
      delete_layer = TRUE
    )
  } else {
    NULL
  }
}

#' Write trial design files for field implementation
#'
#' Desc
#'
#' @param trial_design (tibble) tibble that was created by applying assign_rate() to experimental plots made by make_exp_plots().
#' @param folder_path (character) path to the folder in which the files will be saved
#' @param tr_file_name (character) file name given to the trial design shape files
#' @param a_abline_file_name (character) file name given to the applicator/planter ab-line shape files
#' @param h_abline_file_name (character) file name given to the harvester ab-line shape files
#' @param overwrite (logical) Default = FALSE. If TRUE, the existing shape file with the same file name will be overwritten.
#' @param zip (logical) Default = FALSE. If TRUE, all the files that are being written will be zipped.
#' @returns write out files
#' @import sf
#' @export
write_trial_files <- function(trial_design, folder_path = NA, tr_file_name = NA, a_abline_file_name = NA, h_abline_file_name = NA, overwrite = FALSE, zip = FALSE) {
  #++++++++++++++++++++++++++++++++++++
  #+ write trial designs as shape files
  #++++++++++++++++++++++++++++++++++++
  trial_design %>%
    summarise(list(
      st_write(
        ab_lines,
        dsn =
          ifelse(
            is.na(folder_path),
            getwd(),
            folder_path
          ),
        layer =
          ifelse(
            is.na(tr_file_name),
            paste0("ab-lines-", ifelse(input_type == "S", "planter", "applicator")),
            tr_file_name
          ),
        driver = "ESRI Shapefile",
        append = FALSE,
        delete_layer = overwrite
      )
    )) %>%
    #++++++++++++++++++++++++++++++++++++
    #+ ab-lines
    #++++++++++++++++++++++++++++++++++++
    summarise(list(
      if (ab_line_type != "non") {
        st_write(
          ab_lines,
          dsn =
            ifelse(
              is.na(folder_path),
              getwd(),
              folder_path
            ),
          layer =
            ifelse(
              is.na(tr_file_name),
              paste0("trial-design-", input_type),
              a_abline_file_name
            ),
          driver = "ESRI Shapefile",
          append = FALSE,
          delete_layer = overwrite
        )
      } else {
        NULL
      }
    ))
  #++++++++++++++++++++++++++++++++++++
  #+ harvester ab-line
  #++++++++++++++++++++++++++++++++++++
}

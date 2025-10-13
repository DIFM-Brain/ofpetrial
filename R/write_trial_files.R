#' Write trial design files for field implementation
#'
#' Write out all the necessary files to implement the trial design created. Exported files include
#'
#' @param td (tibble) a tibble of a trial design created by applying assign_rate() to experimental plots made by make_exp_plots().
#' @param folder_path (character) path to the folder in which the files will be saved
#' @param ext (character) Default = "shp". Extension to use to save the files, "geojson" or any other extension supported by sf::st_write()
#' @param zip (logical) Default = FALSE. If TRUE, all the files that are being written will be zipped.
#' @param zip_name (character) name of the zip file created when zip = TRUE.
#' @returns nothing
#' @export
#' @examples
#' #--- load trial design ---#
#' \donttest{
#' data(td_two_input)
#' 
#' write_trial_files(
#'   td = td_two_input,
#'   folder_path = tempdir(),
#'   zip = FALSE
#' )
#' }
write_trial_files <- function(td, folder_path, ext = "shp", zip = FALSE, zip_name = NA) {
  # write_trial_files(td, folder_path = here::here("test"), zip = TRUE)
  # folder_path <- here::here(getwd(), "test")
  folder_path <- ifelse(is.na(folder_path), getwd(), folder_path)

  #++++++++++++++++++++++++++++++++++++
  #+ trial design
  #++++++++++++++++++++++++++++++++++++
  input_name_ls <- td$input_name
  trial_design_ls <- td$trial_design

  #--- write ---#
  purrr::walk2(
    input_name_ls,
    trial_design_ls,
    \(input_name, trial_design) write_td(input_name, trial_design, folder_path, ext)
  )

  #++++++++++++++++++++++++++++++++++++
  #+ applicator/planter ab-line
  #++++++++++++++++++++++++++++++++++++
  abline_type <- td$abline_type %>% unique()

  if (abline_type != "none") {
    ab_lines_ls <- td$ab_lines
  }


  #--- write applicator/planter ab-lines ---#
  purrr::walk2(
    input_name_ls,
    ab_lines_ls,
    \(input_name, ab_lines) write_ap_abline(input_name, abline_type, ab_lines, folder_path, ext)
  )

  # st_read(here::here("test", "ab-line-NH3.shp")) %>% plot()

  #+++++++++++++++++++++++++++++++++++
  # harvester ab-line
  #+++++++++++++++++++++++++++++++++++
  #* Note: harvest ab-lines are identical if two-input case

  message(paste0("Writing the harvester ab-line as .", ext, " file. \n"))

  dsn <- file.path(folder_path, paste0("ab-line-harvester", ".", ext))
  unlink(dsn)

  sf::st_write(
    td$harvest_ab_lines[[1]] %>% sf::st_transform(4326),
    dsn = dsn,
    quiet = TRUE
  )

  # st_read(here::here("test", "harvester-ab-line.shp")) %>% plot()

  #++++++++++++++++++++++++++++++++++++
  #+ Zip if requested
  #++++++++++++++++++++++++++++++++++++
  if (zip == TRUE) {
    zip_name <- ifelse(is.na(zip_name), "td_files.zip", zip_name)
    zip_path <- paste0(folder_path, "/", zip_name)

    if (file.exists(zip_path)) {
      # Delete file if it exists
      file.remove(zip_path)
    }

    shp_files_ls_text <-
      c(
        paste0("trial-design-", input_name_ls),
        paste0("ab-line-", input_name_ls),
        paste0("harvester-ab-line")
      ) %>%
      paste(collapse = "|")

    zip::zip(
      zipfile = zip_name,
      files = list.files(folder_path, recursive = FALSE, full.names = FALSE) %>%
        .[grepl(shp_files_ls_text, .)],
      recurse = FALSE,
      compression_level = 9,
      root = folder_path
    )
  }
}


# !===========================================================
# ! Helper function
# !===========================================================
write_td <- function(input_name, trial_design, folder_path, ext) {
  message(paste0("Writing the trial design as .", ext, " files. \n"))

  dsn <- file.path(folder_path, paste0("trial-design-", input_name, ".", ext))
  unlink(dsn)

  sf::st_write(
    trial_design %>% sf::st_transform(4326),
    dsn = dsn,
    quiet = TRUE
  )
}

write_ap_abline <- function(input_name, abline_type, ab_lines, folder_path, ext) {
  message(paste0("Writing the ab-lines as .", ext, " files. \n"))

  dsn <- file.path(folder_path, paste0("ab-line-", input_name, ".", ext))
  unlink(dsn)

  # try this and change message
  sf::st_write(
    ab_lines %>% sf::st_transform(4326),
    dsn = dsn,
    quiet = TRUE
  )
}

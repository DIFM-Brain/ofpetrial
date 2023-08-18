#' Create data of base application rate information for a single input
#'
#' Create data of base application rate information for a single input when applicable. This can be added when preparing trial rates using prepare_rates().
#'
#' @param base_input_name (string) input name
#' @param base_unit (string) unit of input applied at base
#' @param base_rate (numeric) amount of input applied
#' @returns data.frame of base rate information
#' @import data.table
#' @export
#' @examples
#' base_info <- add_base_rate(
#'   "uan28",
#'   "gallons",
#'   15
#'   )
#'
add_base_rate <- function(base_input_name, base_unit, base_rate){
  base_info = data.frame(
    input_name = base_input_name,
    unit = base_unit,
    rate = base_rate
  )

  return(base_info)
}

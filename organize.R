usethis::use_package("data.table")
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("magrittr")
usethis::use_package("sf")
usethis::use_package("lwgeom")
usethis::use_package("stringr")
usethis::use_package("measurements")
usethis::use_package("purrr")
usethis::use_package("tidyr")
usethis::use_pipe() # can use %>% after this

devtools::document() # regenerate documents reflecting the changes and apply load_all()

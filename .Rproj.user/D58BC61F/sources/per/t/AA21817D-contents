
#!===========================================================
#! One-time operations
#!===========================================================
usethis::use_package("data.table")
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("magrittr")
usethis::use_package("sf")
usethis::use_package("lwgeom")
usethis::use_package("stringr")
usethis::use_package("measurements")
usethis::use_package("purrr")
usethis::use_package("tibble")
usethis::use_package("tidyr")
usethis::use_package("ggpubr")
usethis::use_pipe() # can use %>% after this

usethis::use_build_ignore(c("notes.md", "organize.R", "structure.rmd", "todo.md", "test.R", "docs", "README.html"))

usethis::use_readme_rmd()
usethis::use_news_md()
usethis::use_vignette("ab-line")
usethis::use_vignette("trial-design-options")
usethis::use_vignette("random-vs-orthogonal")

#!===========================================================
#! 
#!===========================================================

devtools::document() # regenerate documents reflecting the changes and apply load_all()
devtools::load_all()

#--- build pkgdown website ---#
pkgdown::build_site()


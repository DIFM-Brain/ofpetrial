#!===========================================================
#! Check and debug
#!===========================================================
#--- regenerate documents reflecting the changes and apply load_all() ---#
devtools::document()
devtools::load_all()

#--- run all the examples to check error ---#
devtools::run_examples(pkg = here::here())

#--- check if the yaml can be read successfully ---#
readLines("./_pkgdown.yml")

#!===========================================================
#! Build website
#!===========================================================
#--- build pkgdown website ---#
pkgdown::build_site()

#!===========================================================
#! One-time operation
#!===========================================================
#++++++++++++++++++++++++++++++++++++
#+ packages to use
#++++++++++++++++++++++++++++++++++++
usethis::use_package("data.table")
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("magrittr")
usethis::use_package("sf")
usethis::use_package("lwgeom")
usethis::use_package("stringr")
usethis::use_package("measurements")
usethis::use_package("purrr")
usethis::use_package("terra")
usethis::use_package("tibble")
usethis::use_package("tidyr")
usethis::use_package("ggpubr")
usethis::use_package("ggExtra")
usethis::use_package("zip")
usethis::use_pipe() # can use %>% after this

#++++++++++++++++++++++++++++++++++++
#+ Folders, files to ignore
#++++++++++++++++++++++++++++++++++++
usethis::use_build_ignore(c("dev_notes.md", "checklist.md", "notes.md", "organize.R", "structure.rmd", "todo.md", "test.R", "docs", "README.html", "debug", "random-vs-orthogonal.Rmd", "test", "dev", "_pkgdown.yml"))

#++++++++++++++++++++++++++++++++++++
#+ Vignette and other documents
#++++++++++++++++++++++++++++++++++++
usethis::use_readme_rmd()
usethis::use_news_md()
usethis::use_vignette("V1-trial-design-options")
usethis::use_vignette("V2-ab-line")
usethis::use_vignette("V3-change-rates-manually")
usethis::use_vignette("V4-diagnose-td")
usethis::use_vignette("V5-understanding-structure")


usethis::use_vignette("random-vs-orthogonal")
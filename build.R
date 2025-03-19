usethis::use_package("config")
usethis::use_package("qualtRics")
usethis::use_package("dplyr")
usethis::use_package("knitr")
usethis::use_package("REDCapR")
usethis::use_package("cli")
usethis::use_package("R6")
#source("sync.R")
# Generate documentation from roxygen comments
devtools::document()
devtools::load_all()  # Load the package with the new name
# Check if your package passes CRAN checks
devtools::check()

# Build the package
devtools::build()

# Install the package locally
devtools::install()

wizaRdry::getRedcap("measure_1")

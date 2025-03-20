usethis::use_package("config")
usethis::use_package("qualtRics")
usethis::use_package("dplyr")
usethis::use_package("knitr")
usethis::use_package("REDCapR")
usethis::use_package("cli")
usethis::use_package("R6")
usethis::use_package("mongolite")
usethis::use_package("future")
usethis::use_package("future.apply")
usethis::use_package("parallel")
usethis::use_package("stringdist")
usethis::use_package("rlang")

# syncs all functions from dev (belieflab/api) to wizaRdry
source("dev/sync.R")

# Generate documentation from roxygen comments
devtools::document()

# Load the package with the new name
devtools::load_all()

# Check if your package passes CRAN checks
devtools::check()

# Build the package
devtools::build()

# Install the package locally
devtools::install()

# run test cases
measure_1 <- wizaRdry::getRedcap("measure_1")
rgpts <- wizaRdry::getQualtrics("rgpts")
dd <- wizaRdry::getMongo("dd", "capr")

# !!! remove previous version !!!
remove.packages("wizaRdry")


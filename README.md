---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# wizaRdry

<!-- badges: start -->
[![R-CMD-check](https://github.com/belieflab/wizaRdry/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/belieflab/wizaRdry/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/wizaRdry)](https://CRAN.R-project.org/package=wizaRdry)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## A Framework For Collaborative & Reproducible Data Analysis

The wizaRdry package provides a comprehensive data analysis framework specifically designed for NIH-funded psychology research with built-in NIH Data Archive (NDA) integration.

## Installation

You can install the released version of wizaRdry from [CRAN](https://CRAN.R-project.org) with:

```{r eval=FALSE}
install.packages("wizaRdry")
```

Or install the development version from GitHub:

```{r eval=FALSE}
# install.packages("devtools")
devtools::install_github("belieflab/wizaRdry")
```

## Features

- **Cross-modal data access**: Unified interface to REDCap, MongoDB, and Qualtrics
- **Memory-aware parallel processing**: Automatically scales to available resources
- **Field harmonization**: Standardizes data fields across platforms
- **NIH Data Archive integration**: Prepares submissions for NDA compliance

## Core Functions

### Data Access

```{r eval=FALSE}
# Get data from REDCap
redcap_data <- getRedcap("measure_1")

# Get data from Qualtrics
qualtrics_data <- getQualtrics("rgpts")

# Get data from MongoDB
mongo_data <- getMongo("dd", "capr")
```

### Workflow Functions

```{r eval=FALSE}
# Access data across modalities
dataRequest("measure1", csv = TRUE)

# Prepare NDA submissions
ndaRequest("eefrt01", rdata = TRUE)
```

### Data Processing

```{r eval=FALSE}
# Filter data
filtered_data <- dataFilter(df, 
                           rows_of_interest = c("sub001","sub002"),
                           columns_of_interest = c("src_subject_id", "phenotype"))

# Merge datasets
merged_data <- dataMerge(df1, df2)

# Parse multi-survey datasets
dataParse("combined_surveys")
```

### Export Functions

```{r eval=FALSE}
# Create CSV output
createCsv(df, "data_export")

# Create R data file
createRda(df, "data_export")

# Create SPSS file
createSpss(df, "data_export")

# Create NDA submission template
createNda(eefrt01)
```

## Configuration

wizaRdry uses configuration files to securely manage database credentials:

1. Create a `config.yml` file in your project directory
2. Create a `secrets.R` file for API credentials
3. See package documentation for detailed configuration instructions

## Citation

If you use wizaRdry in your research, please cite it:

```
Kenney, J., Pappu, M., Williams, T., Pokorny, V., Horgan, C., & Spilka, M. (2025). 
wizaRdry: A Framework For Collaborative & Reproducible Data Analysis. 
R package version 0.1.0. https://github.com/belieflab/wizaRdry
```

## License

MIT © [Joshua Kenney](LICENSE.md)

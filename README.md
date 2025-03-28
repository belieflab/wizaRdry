<!-- README.md is generated from README.Rmd. Please edit that file -->

# wizaRdry

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/wizaRdry)](https://CRAN.R-project.org/package=wizaRdry)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## A Framework for Collaborative & Reproducible Data Analysis

The wizaRdry package provides a comprehensive data analysis framework specifically designed for NIH-funded computational psychiatry, neuroscience, and psychology research with built-in NIH Data Archive (NDA) integration.

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("belieflab/wizaRdry")
```

Once accepted on CRAN, you will be able to install the released version with:

```r
install.packages("wizaRdry")
```

## Getting Started

After installation, follow these steps to set up your project:

### 1. Initialize Project Structure

Use the `scry()` function to create the necessary directory structure:

```r
library(wizaRdry)
scry()
```

This will create a standard directory structure that looks like this:

```
.
├── clean
│   ├── mongo
│   ├── qualtrics
│   └── redcap
├── nda
│   ├── mongo
│   ├── qualtrics
│   ├── redcap
│   └── tmp
├── tmp
├── .gitignore
├── config.yml
├── main.R
├── project.Rproj
└── secrets.R
```

Each directory has a specific purpose in the wizaRdry workflow:
- `clean/` - Scripts for cleaning and processing raw data
- `nda/` - Scripts for preparing NDA submission templates
- `tmp/` - Temporary output files
- Configuration files at the root level

### 2. Configure Secrets

Edit the generated `secrets.R` file to add your API credentials:

```r
# REDCap
uri   <- "https://your-redcap-instance.edu/api/"
token <- "YOUR_TOKEN"

# Qualtrics
apiKeys <- c("YOUR_API_KEY")
baseUrls <- c("YOUR_BASE_URL")

# MongoDB
connectionString <- "mongodb://your-connection-string"
```

### 3. Configure Study Settings

Edit the generated `config.yml` file to specify your study settings:

```yaml
default:
  study_alias: yourstudy
  identifier: src_subject_id
  mongo:
    collection: ${study_alias}
  qualtrics:
    survey_ids:
      Institution1:
        survey_alias: "SV_QUALTRICS_ID"
  redcap:
    superkey: ndar_subject01
```

## Features

- **Project scaffolding**: Creates standard directory structures with `scry()`
- **Cross-modal data access**: Unified interface to REDCap, MongoDB, and Qualtrics
- **Memory-aware parallel processing**: Automatically scales to available resources
- **Field harmonization**: Standardizes data fields across platforms
- **NIH Data Archive integration**: Prepares submissions for NDA compliance
- **Collaborative workflow**: Enables multiple researchers to work from the same data source

## Core Functions

wizaRdry provides a suite of functions organized by their purpose in the data workflow:

### Project Setup

```r
# Initialize project structure
scry()
```

### Data Access

```r
# Get data from REDCap
demoses01 <- getRedcap("demoses01")

# Get data from Qualtrics
lshrs01 <- getQualtrics("lshrs01")

# Get data from MongoDB
prl01 <- getMongo("prl01")
```

### Data Cleaning

```r
# Data Cleaning Workflow - run cleaning scripts and validation
dataRequest("demo", "rgpts", "overfitting", csv = TRUE)
```

### Data Cleaning
Cleaning scripts are written inside the `clean/` directory and called by their script name (e.g., "demo" for demographics) in `dataRequest()`

```r
# Filter data
filtered_data <- dataFilter(df, 
                           rows_of_interest = c("sub001","sub002"),
                           columns_of_interest = c("src_subject_id", "phenotype"))

# Merge datasets
merged_data <- dataMerge(demo_clean, rgpts_clean) 

# Parse multi-survey datasets
dataParse("overfitting")
```

### NDA Submission

```r
# NDA Submission Workflow - prepare NDA templates
ndaRequest("demoses01", "lshrs01", "prl01")
```

### Data Export

```r
# Create CSV output
createCsv(df, "data_export")

# Create R data file
createRda(df, "data_export")

# Create SPSS file
createSpss(df, "data_export")

# Create NDA submission template
createNda(eefrt01)
```

## Workflows

The wizaRdry package supports two distinct but complementary workflows:

### 1. Data Cleaning Workflow

This workflow focuses on accessing and cleaning raw data for analysis:

- Place cleaning scripts in the `clean/` directory
- Scripts should be organized by data source: `clean/qualtrics/`, `clean/redcap/`, `clean/mongo/`
- Name your cleaned datasets with the `_clean` suffix (e.g., `rgpts_clean`)
- Access and process data with:

```r
# Process data from multiple sources in one command
dataRequest("rgpts", "wtar", "prl", csv = TRUE)
```

This runs your cleaning scripts, performs validation tests, and exports cleaned data.

### 2. NDA Submission Workflow

This workflow prepares data for NIH Data Archive submission:

- Place NDA remediation scripts in the `nda/` directory
- Scripts should follow NDA structure naming: `nda/qualtrics/rgpts01.R`
- NDA structure names typically end with a two-digit suffix (e.g., `01`)
- Process and validate NDA structures with:

```r
# Prepare NDA submission templates
ndaRequest("rgpts01", "wtar01", "prl01", csv = TRUE)
```

This creates properly formatted NDA submission templates in the `.nda/tmp` directory.

## Script Examples

### Data Cleaning Script Example (clean/qualtrics/rgpts.R)

```r
# Get raw data from Qualtrics
rgpts <- getQualtrics("rgpts")

# Cleaning process
rgpts$interview_date <- as.Date(rgpts$interview_date, "%m/%d/%Y")
rgpts$src_subject_id <- as.numeric(rgpts$src_subject_id)

# Calculate scores
rgpts$rgpts_total <- rowSums(rgpts[,grep("^rgpts_q\\d+$", names(rgpts))], na.rm = TRUE)

# Final cleaned dataset
rgpts_clean <- rgpts
```

### NDA Remediation Script Example (nda/qualtrics/rgpts01.R)

```r
# Get data for NDA submission
rgpts01 <- getQualtrics("rgpts")

# Apply NDA standards
rgpts01$src_subject_id <- as.character(rgpts01$src_subject_id)
rgpts01$interview_date <- format(as.Date(rgpts01$interview_date, "%m/%d/%Y"), "%m/%d/%Y")

# Ensure NDA structure compliance
if (!"visit" %in% names(rgpts01)) {
  rgpts01$visit <- "baseline"
}

# Additional NDA-specific processing...
```

## Citation

If you use wizaRdry in your research, please cite it:

```
Kenney, J., Williams, T., Pappu, M., Spilka, M., Pratt, D., Pokorny, V., Castiello de Obeso, S., Suthaharan, P., & Horgan, C. (2025). 
wizaRdry: A Framework For Collaborative & Reproducible Data Analysis. 
R package version 0.1.0. https://github.com/belieflab/wizaRdry
```

## License

MIT © [Joshua Kenney](LICENSE.md)

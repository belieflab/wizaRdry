<!-- README.md is generated from README.Rmd. Please edit that file -->

# wizaRdry

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/wizaRdry)](https://CRAN.R-project.org/package=wizaRdry)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## A Magical Framework for Collaborative & Reproducible Data Analysis

The **wizaRdry** package provides a comprehensive data analysis framework specifically designed for NIH-funded computational psychiatry, neuroscience, and psychology research with built-in NIH Data Archive (NDA) integration.

## Installation

You can install the latest published version from CRAN with:

```r
install.packages("wizaRdry")
```

Alternativley, you can install the latest development version from GitHub:

```r
remove.packages("wizaRdry")
rstudioapi::restartSession()
install.packages("devtools")
devtools::install_github("belieflab/wizaRdry")
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
│   ├── csv
│   ├── mongo
│   ├── qualtrics
│   ├── redcap
│   └── sql
├── nda
│   ├── csv
│   ├── mongo
│   ├── qualtrics
│   ├── redcap
│   └── sql
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
apiKeys <- c("API_KEY_1", "API_KEY_2")
baseUrls <- c("BASE_URL_1", "BASE_URL_2")

# MongoDB
connectionString <- "mongodb://your-connection-string"

# SQL Server
conn <- "BASE_URL"
uid <- "USERNAME"
pwd <- "PASSWORD"
```

### 3. Configure Study Settings

Edit the generated `config.yml` file to specify your study settings:

```yaml
default:
  study_alias: yourstudy
  identifier: src_subject_id
  mongo:
    database: ${study_alias}
  qualtrics:
    survey_ids:
      Institution1:
        survey_alias: "SV_QUALTRICS_ID"
  redcap:
    primary_key: record_id
    superkey: ndar_subject01
  sql:
    primary_key: 'sub_id'
    superkey: 'phi'
    schemas:
      - ${study_alias}
    pii_fields:
      - 'name_first'
      - 'name_middle'
      - 'name_last'
```

### 4. Configure Missing Data Codes

Additionally, edit the generated `config.yml` file to specify missing data codes if they are used in your data.

You may use multiple types of codes (skipped, refused, missing, undefined) and multiple codes for each.

These values will be **replaced** by the missing data codes associated with each NDA Data Structure automatically when using `nda()`

```yaml
default:
  missing_data_codes:
    skipped:
      - -888   # Skip pattern/branching logic
    refused:
      - -9999  # Explicitly declined to answer (radio buttons)
      - -1     # Explicitly declined to answer (text boxes)
    missing:
      - -777    # Missing for unknown reasons
    undefined:
      - -555   # Otherwise undefined value
```

## Features

- **Project scaffolding**: Creates standard directory structures with `scry()`
- **Cross-modal data access**: Unified interface to REDCap, MongoDB, Qualtrics and SQL (beta)
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
#  data from REDCap
demoses01 <- redcap("demoses01")

# Get data from Qualtrics
lshrs01 <- qualtrics("lshrs01")

# Get data from MongoDB
prl01 <- mongo("prl01")
```

### Data Cleaning

```r
# Data Cleaning Workflow - run cleaning scripts and validation
clean("demo", "rgpts", "overfitting", csv = TRUE)
```

### Data Cleaning
Cleaning scripts are written inside the `clean/` directory and called by their script name (e.g., "demo" for demographics) in `clean()`

```r
# Filter data
filtered_data <- sift(df, rows = c("sub001","sub002"),
                          cols = c("src_subject_id", "phenotype"))

# Merge datasets
merged_data <- meld(demo_clean, rgpts_clean) 

# Parse multi-survey datasets
rune("overfitting")
```

### NDA Submission

```r
# NDA Submission Workflow - prepare NDA templates
nda("demoses01", "lshrs01", "prl01")
```

### Data Export

```r
# Create CSV output
to.csv(df, "data_export")

# Create R data file
to.rds(df, "data_export")

# Create SPSS file
to.sav(df, "data_export")
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
clean("rgpts", "wtar", "prl", csv = TRUE)
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
nda("rgpts01", "wtar01", "prl01", csv = TRUE)
```

This creates properly formatted NDA submission templates in the `.nda/tmp` directory.

## Script Examples

### Data Cleaning Script Example (clean/qualtrics/rgpts.R)

```r
# Get raw data from Qualtrics
rgpts <- qualtrics("rgpts")

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
rgpts01 <- qualtrics("rgpts")

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
# Test

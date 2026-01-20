# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## 🚨 CRITICAL RULES - READ FIRST

### Rule 1: Git Operations - NEVER Push Without Permission

**FORBIDDEN:**
- ❌ `git push` (any branch, any remote)
- ❌ `git push --force`
- ❌ `git push origin <tag>`
- ❌ ANY operation that sends commits to remote repository

**REQUIRED WORKFLOW:**
1. Make commits locally ✅
2. Show user what was committed ✅
3. **ASK:** "Would you like me to push to remote?" 
4. **WAIT** for explicit "yes" or "push it"
5. Only then execute `git push`

**Example:**
```
AI: "Committed changes. Would you like me to push to origin/main?"
[WAIT FOR USER]
User: "yes"
AI: [now pushes]
```

### Rule 2: Text Editor - vim ONLY

**REQUIRED:**
- ✅ If interactive editing needed: **vim ONLY**
- ✅ CORRECT: "Edit with `vim file.txt`"

**FORBIDDEN:**
- ❌ NEVER suggest nano
- ❌ NEVER suggest emacs
- ❌ NEVER suggest any other editor

**PREFERRED:**
- Use programmatic tools (Edit tool, Write tool, sed, awk) when possible
- But if terminal editor needed: **vim only**

**Why These Rules Exist:**
- Git push without permission can publish broken code to production
- User exclusively uses vim and hates nano
- User must control what goes to shared repository

**ALWAYS ASK BEFORE PUSHING. NO EXCEPTIONS.**

---

## Project Overview

wizaRdry is an R package for NIH-funded computational psychiatry, neuroscience, and psychology research. It provides a comprehensive data analysis framework with built-in NIH Data Archive (NDA) integration. The package unifies access to multiple data sources (REDCap, MongoDB, Qualtrics, SQL/Oracle) and provides workflows for both data cleaning and NDA submission preparation.

## Development Commands

### Building and Testing

```r
# Remove previous version and restart R session
remove.packages("wizaRdry")
rstudioapi::restartSession()

# Generate documentation from roxygen comments
devtools::document()

# Load package for testing
devtools::load_all()

# Run CRAN checks
devtools::check()

# Build package
devtools::build()

# Install locally
devtools::install()

# Spell check
devtools::spell_check()

# Check on Windows
devtools::check_win_devel()
```

See `dev/build.R` for the complete build workflow.

### Installing Development Version

```r
# From GitHub
remove.packages("wizaRdry")
rstudioapi::restartSession()
if(!require(devtools)) {install.packages("devtools")}; library(devtools)
devtools::install_github("belieflab/wizaRdry")
```

## Architecture Overview

### Configuration System (R6 Classes)

The package uses two R6 class-based configuration systems that validate and manage settings:

**ConfigEnv** (`R/ConfigEnv.R`): Manages `config.yml` configuration
- Validates API configurations (mongo, qualtrics, redcap, sql, oracle)
- Handles variable substitutions like `${study_alias}`
- Defines missing data code mappings with category aliases
- Accessed via `validate_config()` throughout the codebase

**SecretsEnv** (`R/SecretsEnv.R`): Manages `secrets.R` credentials
- Validates API credentials (tokens, connection strings, API keys)
- Determines which APIs are configured by checking config.yml
- Only validates secrets for APIs that are actually configured
- Accessed via `validate_secrets()` and `get_secret()` functions

Both systems work together: ConfigEnv validates structural settings while SecretsEnv validates credentials.

### Data Access Layer

Core data retrieval functions follow a consistent pattern:

**REDCap** (`R/getRedcap.R`): `redcap(instrument_name, ...)`
- Fetches REDCap instruments via REDCapR
- Supports event filtering with `redcap_event_name` parameter (single string or vector)
- Propagates subject identifiers across events
- Optionally filters to complete cases using `...` column specification

**Qualtrics** (`R/getQualtrics.R`): `qualtrics(qualtrics_alias, ...)`
- Multi-institution support via institution parameter
- Survey IDs mapped in config.yml under `qualtrics.survey_ids`
- Automatic institution detection if not specified
- Supports label conversion, interview_date filtering, and complete-only filtering

**MongoDB** (`R/getMongo.R`): `mongo(collection_name, ...)`
- Uses mongolite package
- Implements memory-aware parallel processing for large datasets
- Cross-platform memory detection (Windows, macOS, Linux)
- Automatic chunking based on available system memory

**SQL/MariaDB** (`R/getSql.R`): `sql(table_name, ...)`
- Auto-joins with primary keys table when configured
- Supports custom WHERE clauses and field selection
- PII field filtering based on config.yml settings
- Batch processing for large datasets

**Oracle** (`R/getOracle.R`): `oracle(table_name, ...)`
- Supports DSN, DBQ (TNS alias), or host-based connections
- Automatic driver detection with validation
- Similar interface to sql() function with auto-joining

All data retrieval functions:
- Use loading animations via `initializeLoadingAnimation()`
- Support optional column filtering with `...` parameters
- Return data frames with superkeys/identifiers first
- Display timing information on completion

### Workflow Functions

**Data Cleaning Workflow** (`R/dataRequest.R`): `clean(..., csv=FALSE, rdata=FALSE, spss=FALSE)`
- Executes cleaning scripts from `./clean/{api_type}/` directories
- Automatically detects data source (CSV, REDCap, Qualtrics, MongoDB, Oracle, SQL)
- Runs validation tests via `testSuite.R`
- Interactive script creation for missing measures
- User preferences stored in `.wizaRdry_prefs` file
- Expects cleaned datasets to be named with `_clean` suffix

**NDA Submission Workflow** (`R/ndaRequest.R`): `nda(..., csv=FALSE, rdata=FALSE, spss=FALSE, limited_dataset=FALSE)`

The NDA workflow has **three distinct pathways** based on whether the data structure exists in the NDA data dictionary:

#### Pathway 1: Existing NDA Structure (Full Validation)
When a data structure already exists in the NDA data dictionary:

1. **Structure Enhancement** (`ndaRequest.R:1003-1010`):
   - Fetches structure definition from `https://nda.nih.gov/api/datadictionary/v2`
   - Calls `addNdarSubjectElements()` to fetch ALL required + common recommended fields from ndar_subject01
   - Merges ndar_subject01 metadata into existing structure via `mergeNdarSubjectIntoExisting()`
   - This ensures ndar_subject01 fields override any conflicting definitions in the original structure

2. **Full Validation** (`ndaRequest.R:1014`):
   - Runs `ndaValidator()` with the enhanced structure
   - Validates data types, value ranges, required fields
   - Detects value range violations (e.g., data contains codes not in NDA valueRange)
   - Handles array field conversions (bracket notation → numeric codes)
   - Returns validation results with `value_range_violations` tracked

3. **File Creation** (`ndaRequest.R:1065-1232`):
   - **Always creates**: Submission File (`measure_submission.csv`) via `createNdaSubmissionTemplate()`
     - CSV file with structure name in first line, headers in second line, data below
     - Ready for upload to NDA submission portal
   - **Conditionally creates**: Data Definition (`measure_data-definition.xlsx`) via `createNdaDataDefinition()`
     - Only created if structure is **modified**:
       - New fields detected (not in original NDA structure)
       - Value range violations found (data values exceed NDA-defined ranges)
     - Skipped for unmodified structures
     - Excel file with field metadata for registering structure changes with NDA

4. **Output**: `tmp/measure_submission.csv` (always), `tmp/measure_data-definition.xlsx` (if modified)

#### Pathway 2: New NDA Structure (Bypass Validation)
When a data structure is NOT found in the NDA data dictionary:

1. **Structure Detection** (`ndaRequest.R:1076-1117`):
   - API returns 404 or empty response for structure lookup
   - Sets `bypassed_validation = TRUE` in validation results
   - Creates mock NDA structure with empty dataElements

2. **Metadata Enhancement**:
   - Calls `addNdarSubjectElements()` to fetch ndar_subject01 metadata
   - Calls `mergeRequiredMetadata()` to inject required + recommended field definitions
   - This provides complete field metadata even without an existing structure

3. **File Creation**:
   - **Skips**: Submission Template (cannot submit data for non-existent structure)
   - **Always creates**: Data Definition (`measure_data-definition.xlsx`)
     - Uses computed metadata from data frame types + ndar_subject01
     - Required for registering the new structure with NDA before data submission

4. **Output**: `tmp/measure_data-definition.xlsx` only

#### Pathway 3: Data Definition Creation Logic
The decision to create a data definition file is made in `ndaRequest.R:1132-1232`:

**For NEW structures** (Pathway 2):
- Always create data definition
- Reason: Must register structure with NDA before submitting data

**For EXISTING structures** (Pathway 1):
- Only create if **modified** by checking:

  a) **New fields detected** (`ndaRequest.R:1160-1171`):
     - Compares data frame columns vs. NDA structure's dataElements
     - Excludes REDCap completion fields (`*_complete`) and super-required fields from "new" count
     - If new fields exist → `is_modified_structure = TRUE`

  b) **Value range violations** (`ndaRequest.R:1174-1199`):
     - Checks `validation_results$value_range_violations` from ndaValidator
     - If violations exist (data values outside NDA-defined ranges) → `is_modified_structure = TRUE`
     - Example: NDA structure defines valueRange="1;2;3" but data contains "9999"

- If unmodified → skip data definition (only submission file needed)

**Key Functions**:
- `addNdarSubjectElements()`: Fetches and adds ndar_subject01 required/recommended fields (`ndaRequest.R:1273`)
- `mergeNdarSubjectIntoExisting()`: Overrides existing structure fields with ndar_subject01 definitions (`ndaRequest.R:1515`)
- `mergeRequiredMetadata()`: Merges ndar_subject01 metadata into new structures (`ndaRequest.R:1468`)
- `createNdaSubmissionTemplate()` / `to.nda()`: Creates CSV for data upload (`R/createNdaSubmissionTemplate.R`)
- `createNdaDataDefinition()`: Creates Excel metadata for structure registration (`R/createNdaDataDefinition.R`)

**Common Operations**:
- Executes NDA remediation scripts from `./nda/{api_type}/` directories
- Handles missing data code replacements based on config.yml
- Performs date-shifting (MM/DD/YYYY → MM/01/YYYY) and age-capping unless `limited_dataset=TRUE`
- Removes API-specific columns (REDCap: primary_key & redcap_event_name; Qualtrics: Progress, ResponseId, etc.)
- Normalizes race values ("Native Hawaiian or Pacific Islander" → "Hawaiian or Pacific Islander")

### Key Validation Systems

**ndaValidator** (`R/ndaValidator.R`):
- Comprehensive validation against NDA data dictionaries
- **Type checking**: Ensures Integer/Float/String/Date types match NDA expectations
- **Range validation**: Checks data values against NDA-defined valueRanges
  - Tracks violations in `validation_results$value_range_violations`
  - Used to determine if data definition file is needed
- **Required field verification**: Ensures all NDA-required fields are present and non-empty
- **Array field handling** (`ndaValidator.R:66-306`):
  - Detects array fields in bracket notation (e.g., `"[Left, Middle, Right]"`)
  - Parses array content and maps to numeric codes via `convert_array_fields()`
  - Supports both notes-based mapping and position-based mapping
  - Robust error handling for fields with many NAs (branching logic)
- **Missing field handling** (`ndaValidator.R:308-350`):
  - Auto-adds missing required fields with appropriate missing data codes
  - Extracts missing codes from NDA field notes (e.g., "999 = Missing")
- **Field standardization** (`ndaValidator.R:352-399`):
  - Converts "index" → "trial" for behavioral task data
  - Handles other common field transformations
- **REDCap integration**:
  - Fetches metadata via `redcap.dict()` to enrich field descriptions
  - Handles checkbox fields (parent___1 format) with choice mappings
  - Parses REDCap validation rules to derive value ranges
- **Qualtrics integration**:
  - Fetches column map via `qualtrics.dict()` for question text descriptions
- **Safe readline implementation**: Handles Ctrl+C interrupts gracefully without crashing
- **Detailed error reporting**: Shows line numbers, field names, and specific violations

**testSuite** (`R/testSuite.R`):
- Validates cleaned data frames
- Checks for required identifiers
- Verifies data quality standards

### Utility Functions

**Project Setup**:
- `scry()` - Initialize wizaRdry directory structure (`R/scry.R`)
  - Creates `clean/`, `nda/`, `tmp/` directories
  - Generates config.yml and secrets.R templates
  - Optionally creates R project file
  - Repair mode for incomplete structures

**Data Manipulation**:
- `sift()` - Filter data by rows and columns (`R/dataFilter.R`)
- `meld()` - Merge datasets (`R/dataMerge.R`)
- `qualtrics.rune()` - Parse multi-survey Qualtrics data by prefix (`R/dataParse.R`)

**Data Export**:
- `to.csv()` - Export to CSV (`R/createCsv.R`)
- `to.rds()` - Export to R data file (`R/createRda.R`)
- `to.sav()` - Export to SPSS (`R/createSpss.R`)

### Package Lifecycle

**Startup** (`R/zzz.R`):
- `.onLoad()`: Sets mongolite options, loads secrets.R if exists
- `.onAttach()`: Checks for project structure, suggests `scry()` or `scry(repair=TRUE)`
- Validates structure completeness on startup
- Checks for duplicate script names across folders

## Important Patterns

### Configuration Files

**config.yml structure**:
```yaml
default:
  study_alias: studyname
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
    database: ${study_alias}
    schemas: [${study_alias}]
    pii_fields: ['name_first', 'name_last']
  missing_data_codes:
    skipped: [-888]
    refused: [-9999, -1]
    missing: [-777]
    undefined: [-555]
```

### Script Organization

Cleaning scripts go in: `clean/{csv|mongo|qualtrics|redcap|oracle|sql}/scriptname.R`
- Should create a `scriptname_clean` data frame
- Accessed via: `clean("scriptname")`

NDA scripts go in: `nda/{csv|mongo|qualtrics|redcap|oracle|sql}/structure01.R`
- Should create a `structure01` data frame matching NDA requirements
- Accessed via: `nda("structure01")`

### Common Gotchas

1. **Oracle connections**: Support DSN, DBQ (TNS alias), or host. Use uppercase field names (DSN, DBQ) in secrets.R per Oracle convention.

2. **REDCap event filtering**: The `redcap_event_name` parameter accepts either a single string OR a vector of event names (e.g., `c("baseline_arm_1", "followup_arm_1")`). A vector will filter to multiple events.

3. **Missing data codes**: Config supports multiple categories (skipped, refused, missing, undefined) with aliases. NDA validator automatically replaces these with NDA-specific codes.

4. **Memory-aware processing**: MongoDB queries automatically chunk based on available system memory to prevent OOM errors.

5. **Parallel processing**: The package uses future/future.apply for parallel operations with automatic worker scaling.

6. **User preferences**: Stored in `.wizaRdry_prefs` file, controls auto-creation prompts and tree display preferences.

## Dependencies

Core dependencies include:
- Data access: REDCapR, qualtRics, mongolite, DBI, RMariaDB, odbc
- Data manipulation: dplyr, haven
- Configuration: config, R6
- Parallel processing: future, future.apply, parallel
- Utilities: cli, stringdist, lubridate, knitr, openxlsx/openxlsx2

## Testing Philosophy

The package implements runtime validation rather than traditional unit tests:
- Config/secrets validation on function entry
- Data quality checks via testSuite during clean()
- NDA compliance validation during nda()
- Memory checks before large operations

## NDA Workflow Decision Tree

Understanding which pathway the `nda()` function will take:

```
Does structure exist in NDA data dictionary?
│
├─ YES (Existing Structure - Pathway 1)
│  │
│  ├─ Run full ndaValidator()
│  ├─ Create submission file (CSV)
│  │
│  └─ Is structure modified?
│     │
│     ├─ YES (new fields OR value range violations)
│     │  └─ Create data definition (Excel)
│     │     → Output: measure_submission.csv + measure_data-definition.xlsx
│     │
│     └─ NO (unmodified)
│        └─ Skip data definition
│           → Output: measure_submission.csv only
│
└─ NO (New Structure - Pathway 2)
   │
   ├─ Bypass validation (no structure to validate against)
   ├─ Skip submission file (can't submit to non-existent structure)
   │
   └─ Create data definition (Excel)
      → Output: measure_data-definition.xlsx only
```

**File Outputs (as of v0.6.2)**:
- `*_submission.csv` - Final submission file ready for NDA upload
- `*_submission_draft.csv` - Draft file (lenient mode with NEW/MODIFIED structures)
- `*_definitions.xlsx` - Data definitions for DCC registration/approval

**File Creation Logic:**
```
NEW structure:
├─ strict=TRUE  → Skip submission file, create data definition
└─ strict=FALSE → Create *_submission_draft.csv + data definition

MODIFIED structure:
├─ strict=TRUE  → Skip all files (validation failed)
└─ strict=FALSE → Create *_submission_draft.csv + data definition

EXISTING structure (unmodified):
├─ strict=TRUE  → Skip all files (validation failed)
└─ strict=FALSE → Create *_submission.csv, skip data definition
```

**When to use each output file**:
- **Submission File** (`*_submission.csv`): Upload to NDA submission portal for existing structures
- **Draft Submission** (`*_submission_draft.csv`): Test file requiring DCC approval (NEW/MODIFIED)
- **Data Definition** (`*_definitions.xlsx`):
  - For new structures: Submit to NDA to register the structure
  - For modified structures: Submit to NDA to update the structure definition
  - Use this to communicate new fields or expanded value ranges to NDA

**Common scenarios**:

1. **First-time structure submission**: Pathway 2 → Creates data definition only (strict) or draft + definition (lenient)
2. **Reusing existing NDA structure unchanged**: Pathway 1 → Creates submission file only
3. **Adding fields to existing structure**: Pathway 1 with modifications → Creates draft + definition (lenient)
4. **Data has codes not in NDA valueRange**: Pathway 1 with violations → Creates draft + definition (lenient)

**BREAKING CHANGE (v0.6.2):** Renamed `*_template.csv` → `*_submission.csv` for clarity

# AGENTS.md

This file provides guidance to AI coding assistants (GitHub Copilot, Cursor, Aider, etc.) when working with this R package.

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

**wizaRdry** is an R package for NIH-funded computational psychiatry, neuroscience, and psychology research. It provides a comprehensive data analysis framework with built-in NIH Data Archive (NDA) integration.

**Key Features:**
- Unified data access across REDCap, MongoDB, Qualtrics, SQL/Oracle
- Automated data cleaning workflows
- NDA submission template generation with validation
- Memory-aware parallel processing
- Modular validation architecture

**Target Users:** Researchers in computational psychiatry, neuroscience, and psychology conducting NIH-funded studies.

---

## Architecture

### Core Workflows

**Data Cleaning:** `clean()`
- Executes cleaning scripts from `./clean/{api_type}/` directories
- Auto-detects data source (CSV, REDCap, Qualtrics, MongoDB, Oracle, SQL)
- Runs validation tests via `testSuite.R`
- Outputs cleaned datasets with `_clean` suffix

**NDA Submission:** `nda()`
- Executes NDA remediation scripts from `./nda/{api_type}/` directories
- Validates against NDA data dictionaries via `ndaValidator()`
- Creates submission templates (`*_template.csv`) and data definitions (`*_data-definition.xlsx`)
- Handles missing data codes, date-shifting, and age-capping

### Data Access Layer

All data retrieval functions follow consistent patterns:

```r
# REDCap
redcap(instrument_name, redcap_event_name = NULL, ...)

# Qualtrics  
qualtrics(qualtrics_alias, institution = NULL, ...)

# MongoDB
mongo(collection_name, ...)

# SQL/MariaDB
sql(table_name, ...)

# Oracle
oracle(table_name, ...)
```

**Common behaviors:**
- Use loading animations
- Support column filtering with `...`
- Return data frames with identifiers first
- Display timing information

### Configuration System

**R6-based configuration management:**

1. **ConfigEnv** (`R/ConfigEnv.R`): Manages `config.yml`
   - Validates API configurations
   - Handles variable substitutions (`${study_alias}`)
   - Defines missing data code mappings

2. **SecretsEnv** (`R/SecretsEnv.R`): Manages `secrets.R` credentials
   - Validates API credentials
   - Only validates secrets for configured APIs

### Validation Architecture

**Modular NDA validation system** (refactored Dec 2024, updated Jan 2025):

**Main Components:**
- `ValidationState` (R6): State management for validation results
- `NdaDataStructure` (R6): Typed struct for NDA field definitions (Jan 2025)
- `ndaValidator()`: Main validation orchestrator with strict/lenient modes
- Helper modules (all internal, not exported):
  - `ndaValidationHelpers.R`: Value range checking, GUID validation, field data completeness
  - `ndaTransformations.R`: Date/age standardization, type conversions
  - `ndaFieldMapping.R`: Field detection and similarity matching
  - `ndaFieldSelection.R`: Centralized user prompts for field selection
  - `ndaFileCreation.R`: Template and definition file creation with validation gate

**Super Required Fields (5 mandatory for ALL NDA submissions):**
1. `subjectkey` - Unique GUID identifier
2. `src_subject_id` - Study-specific subject ID
3. `interview_date` - Date of data collection
4. `interview_age` - Age at interview (months)
5. `sex` - Biological sex (M/F/O)

These fields are defined in the `SUPER_REQUIRED_FIELDS` constant (`R/zzz.R`) and automatically sourced from `ndar_subject01`. They are added to all structures regardless of whether the structure explicitly requires them.

**DCC (Data Coordinating Center) Fields (11 optional fields, controlled by `dcc` parameter):**

**7 Required:**
1. `race` - Race category
2. `phenotype` - Phenotype
3. `phenotype_description` - Phenotype description
4. `twins_study` - Twin study participation
5. `sibling_study` - Sibling study participation
6. `family_study` - Family study participation
7. `sample_taken` - Sample collection status

**4 Recommended:**
1. `ethnic_group` - Ethnicity
2. `site` - Study site
3. `study` - Study identifier
4. `subsiteid` - Sub-site identifier

These fields are defined in three constants in `R/zzz.R`:
- `DCC_REQUIRED_FIELDS` - 7 required fields
- `DCC_RECOMMENDED_FIELDS` - 4 recommended fields
- `DCC_FIELDS` - All 11 fields combined

**DCC Usage:**
```r
# Default: DCC fields NOT included (appear as blue "new" fields in Excel)
nda("cde_phq901")

# Include DCC fields: merged and validated
nda("cde_phq901", dcc = TRUE)

# Include DCC fields with strict validation
nda("cde_phq901", dcc = TRUE, strict = TRUE)
```

**DCC Validation Behavior:**
- `dcc = FALSE` (default): DCC fields excluded from merge, appear as "new" fields in Excel output
- `dcc = TRUE`: DCC fields merged with full metadata and validated:
  - DCC required fields: ANY NA = validation failure
  - DCC recommended fields: ALL NA = violation (strict mode only)

**BREAKING CHANGE:** Prior to v0.x.x, `ethnic_group`, `site`, and `subsiteid` were automatically included from ndar_subject01 (unintentional behavior). They now require `dcc = TRUE`.

**IMPORTANT:** Validation checks super required fields (5) and DCC fields (when `dcc = TRUE`). Structure-level required fields (e.g., phq9_1, phq9_2) are NOT validated to prevent false positives.

**Validation Modes:**
- `strict = TRUE` (default): Required fields with ANY missing data → validation fails (`is_valid = FALSE`), no files created
- `strict = FALSE` (lenient): Required fields with missing data → validation fails (`is_valid = FALSE`), but files created anyway with warnings

**Key Validation Logic:**
- **Super required fields (5):** ANY NA values = validation failure (both modes)
- **DCC fields (when `dcc=TRUE`):**
  - DCC required (7): ANY NA = validation failure
  - DCC recommended (4): ALL NA = violation (strict mode only)
- **Recommended fields:** ALL NA values = violation (strict mode only)
- **Value ranges:** Only flag violations when data exceeds **defined** valueRange
- **Unbounded fields:** String, GUID, Date, Integer, Float fields **without valueRange** = valid (no violations)
- **Message suppression:** In lenient non-verbose mode, individual field messages suppressed to avoid duplication (still shown in verbose mode)
- **Validation status:** Both modes set `is_valid = FALSE` on violations. Difference is whether files are created.

**Validation Gate Logic:**
```r
# In should_create_nda_files():
if (!validation_state$is_valid && !strict) {
  # LENIENT: Create files despite failure
  return(TRUE)
}
if (!validation_state$is_valid) {
  # STRICT: Skip file creation
  return(FALSE)
}
return(TRUE)  # Validation passed
```

**File Creation Decision Tree:**
```
Validation check:
├─ FAILED + strict=TRUE  → Skip all file creation (show errors)
├─ FAILED + strict=FALSE → Continue to file creation (show warnings)
└─ PASSED → Continue to file creation

File creation logic:
├─ NEW structure → STEP 4A: Skip template (doesn't exist yet)
│                  STEP 4B: Create data definition (for registration)
├─ EXISTING unmodified → STEP 4A: Create submission template
│                        STEP 4B: Skip data definition (not needed)
└─ EXISTING modified → STEP 4A: Create submission template
                       STEP 4B: Create data definition (requires approval)
```

### NDA File Naming Conventions

**Output Files:**
- `*_submission.csv` - Final submission file ready for NDA upload
- `*_submission_draft.csv` - Draft submission file (lenient mode with NEW/MODIFIED structures)
- `*_definitions.xlsx` - Data definitions file for DCC registration/approval

**Submission File Naming Logic:**
```
NEW structure:
├─ strict=TRUE  → No submission file created (structure doesn't exist yet)
└─ strict=FALSE → *_submission_draft.csv (for testing before registration)

MODIFIED structure:
├─ strict=TRUE  → No submission file created (validation failed)
└─ strict=FALSE → *_submission_draft.csv (needs DCC approval for modifications)

EXISTING structure:
├─ strict=TRUE  → No submission file created (validation failed)
└─ strict=FALSE → *_submission.csv (just fix data errors, no DCC approval needed)
```

**BREAKING CHANGE (v0.6.2):** Prior to v0.6.2, files were named `*_template.csv`. This was renamed to `*_submission.csv` for clarity, as these files are intended for direct submission to NDA, not as empty templates.

### NDA Field Definition Structure

**NdaDataStructure R6 Class** (added Jan 2025):

A typed struct (similar to Go structs) that represents a single NDA field definition. Replaces ad-hoc list construction for consistency and type safety.

**Core Fields (match Excel columns):**
- `element_name` - Field name (ElementName)
- `data_type` - Data type (String, Integer, Float, Date, GUID, Boolean)
- `size` - Size for String types
- `required` - Requirement level (Required, Recommended, Conditional, No)
- `element_description` - Field description
- `value_range` - Allowed values or range
- `notes` - Field notes
- `aliases` - Field aliases

**Internal Metadata:**
- `selection_order` - Order in which field was selected
- `source` - Field source (nda_validated, nda_modified, computed_from_data, etc.)
- `is_modified` - Whether field was modified from NDA
- `missing_info` - Missing data statistics
- `validation_rules` - Validation rules (min/max/allowed values)

**Key Methods:**
- `to_excel_row()` - Convert to Excel export row
- `to_list()` - Convert to legacy list format
- `is_super_required()` - Check if field is super required
- `is_from_ndar_subject()` - Check if from ndar_subject01
- `modify()` - Create modified copy

**Usage Pattern:**
```r
# Create from NDA API response
field <- NdaDataStructure$new(
  element_name = "subjectkey",
  data_type = "GUID",
  required = "Required",
  value_range = "NDAR*"
)

# Or use factory functions
field <- nda_structure_from_nda(nda_element, selection_order = 1)
field <- nda_structure_from_data("race", df$race, selection_order = 15)

# Convert to list for backward compatibility
field_list <- field$to_list()
```

**Benefits:**
- Type safety and validation at construction
- Consistent structure across all code paths
- Eliminates 200+ lines of sapply extraction code
- Self-documenting field definitions
- Direct mapping to Excel export schema

---

## Code Conventions

### Function Naming

**User-facing (exported):**
- `verb()` - Main workflows: `clean()`, `nda()`, `scry()`
- `source()` - Data access: `redcap()`, `qualtrics()`, `mongo()`, `sql()`, `oracle()`
- `source.util()` - Utilities: `redcap.dict()`, `qualtrics.index()`, etc.
- `to.format()` - Exports: `to.csv()`, `to.rds()`, `to.sav()`, `to.nda()`

**Internal (not exported, use `@noRd`):**
- `verb_noun()` - Helpers: `check_value_range_violations()`, `standardize_dates()`
- `get_*()` - Getters: `get_field_value_range()`, `get_violations()`
- Module documentation uses `@keywords internal`

### File Organization

```
R/
├── get*.R           # Data retrieval (redcap, qualtrics, mongo, sql, oracle)
├── *Request.R       # Workflow orchestrators (dataRequest, ndaRequest)
├── create*.R        # File creators (CSV, RDS, SPSS, NDA templates)
├── nda*.R           # NDA validation modules
├── ValidationState.R, DataEnvironment.R  # R6 classes
├── Config*.R        # Configuration management
└── zzz.R           # Package lifecycle hooks

clean/{api}/        # Cleaning scripts (user-created)
nda/{api}/          # NDA remediation scripts (user-created)
tmp/                # Output directory for generated files
```

### Documentation Standards

**roxygen2 tags:**
- `@export` - User-facing functions only
- `@noRd` - Internal helpers (no .Rd file generated)
- `@keywords internal` - Internal documentation (hidden from help index)
- `@inheritParams` - Reuse parameter documentation
- `@examples` wrapped in `\dontrun{}` for functions requiring external data

### Testing Philosophy

Runtime validation over unit tests:
- Config/secrets validation on function entry
- Data quality checks via `testSuite()` during `clean()`
- NDA compliance validation during `nda()`
- Memory checks before large operations

---

## Common Patterns

### Error Handling

```r
tryCatch({
  # Main operation
}, error = function(e) {
  message(sprintf("Error in %s: %s", function_name, e$message))
  # Return sensible default or stop with informative error
})
```

### Configuration Access

```r
# Validate and get config
config_env <- ConfigEnv$new("config.yml")
study_alias <- config_env$get_study_alias()

# Get secrets
secrets_env <- SecretsEnv$new("secrets.R", config_env)
token <- secrets_env$get_secret("redcap", "token")
```

### Loading Animations

```r
loading_animation <- initializeLoadingAnimation()
# ... long-running operation ...
loading_animation$stop()
```

### Missing Data Codes

```r
# From config.yml:
missing_data_codes:
  skipped: [-888]
  refused: [-9999, -1]
  missing: [-777]
  undefined: [-555]

# Access in code:
config_env$get_missing_data_codes()
```

---

## Development Workflow

### Making Changes

1. **Read relevant files first** - Understand existing patterns
2. **Update documentation** - Keep roxygen comments in sync
3. **Regenerate docs** - `devtools::document()`
4. **Test changes** - `devtools::load_all()` or `devtools::install()`
5. **Run checks** - `devtools::check()`
6. **Commit with clear messages** - Follow conventional commits

### Adding New Data Sources

1. Create `R/getSourceName.R` with main function
2. Add utility functions: `sourcename.index()`, `sourcename.dict()`, `sourcename.rune()`
3. Update `ConfigEnv.R` and `SecretsEnv.R` for validation
4. Add example to `config.yml` template
5. Update `CLAUDE.md` and `AGENTS.md`

### Adding NDA Structures

1. User creates script in `./nda/{api}/structure_name.R`
2. Script creates data frame named `structure_name`
3. `nda("structure_name")` validates and creates files
4. Validation uses modular system (`ndaValidator()`)

---

## Important Gotchas

### Oracle Connections
- Support DSN, DBQ (TNS alias), or host-based
- Use UPPERCASE field names in `secrets.R` per Oracle convention
- Example: `DSN = "my_dsn"` not `dsn = "my_dsn"`

### REDCap Event Filtering
- `redcap_event_name` accepts **single string OR vector**
- Example: `redcap("demo", redcap_event_name = c("baseline_arm_1", "followup_arm_1"))`

### MongoDB Memory Management
- Auto-chunks based on available system memory
- Cross-platform detection (Windows, macOS, Linux)
- Uses `future` for parallel processing

### NDA Value Ranges
- **No valueRange defined** + unbounded type (String/GUID/Date/Integer/Float) = **VALID**
- **No valueRange defined** + unknown type = flag for documentation
- **valueRange defined** + data outside range = **VIOLATION**

### Missing Data Codes
- Config supports multiple categories with aliases
- NDA validator auto-replaces with NDA-specific codes
- Example: `-777` (missing) → `777` in NDA template

---

## Recent Major Changes

### January 2025 - Validation System Refinements & NdaDataStructure
- **NdaDataStructure R6 class:** Typed struct for NDA field definitions (replaces ad-hoc lists)
  - Type safety and validation at construction
  - Factory methods: `nda_structure_from_nda()`, `nda_structure_from_data()`
  - Direct Excel export via `to_excel_row()`
  - Eliminates 200+ lines of sapply extraction code
- **Excel metadata display:** ALL NDA fields now show full metadata (removed clearing logic)
- **SUPER_REQUIRED_FIELDS constant:** Centralized definition of 5 mandatory NDA fields (DRY principle)
- **Validation scope fix:** Only checks 5 super required fields, ignores structure-level required fields (e.g., phq9_*)
- **Lenient mode behavior:** Both strict and lenient modes set `is_valid=FALSE` on violations; lenient mode creates files with warnings
- **Message suppression:** Individual field messages hidden in lenient non-verbose mode to prevent duplication
- **Two-step file output:** STEP 4A (template) and STEP 4B (definition) for clearer user feedback
- **Excel highlighting fix:** Blue highlights only NEW fields from NDA (not already in base structure)
- **Message helper:** `should_show_validation_message(strict, verbose)` centralizes display logic

### December 2024 Refactor
- **Modularized validator:** 4,262 lines → 172 lines (96% reduction)
- **Fixed false positives:** Unbounded fields no longer flagged as violations
- **Eliminated redundant prompts:** Single field selection prompt
- **Created R6 classes:** ValidationState, DataEnvironment
- **Cleaned exports:** 62 → 41 functions (21 internals hidden)

---

## Resources

- **GitHub:** https://github.com/belieflab/wizaRdry
- **NDA Data Dictionary API:** https://nda.nih.gov/api/datadictionary/v2
- **Package Documentation:** Run `help(package = "wizaRdry")` after installing

---

## When Contributing

1. **Preserve backward compatibility** - Lots of research projects depend on this
2. **Test with real data** - If possible, validate changes with actual research datasets
3. **Document thoroughly** - Researchers need clear, complete documentation
4. **Follow R conventions** - Use `snake_case` for functions, `PascalCase` for R6 classes
5. **Respect user environment** - Minimize global environment pollution
6. **Handle errors gracefully** - Research data is messy, expect the unexpected

---

*This file is for AI coding assistants. For Claude Code specific guidance, see `CLAUDE.md`.*

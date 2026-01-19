# AGENTS.md

This file provides guidance to AI coding assistants (GitHub Copilot, Cursor, Aider, etc.) when working with this R package.

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

**IMPORTANT:** Validation ONLY checks these 5 super required fields. Structure-level required fields (e.g., phq9_1, phq9_2) are NOT validated to prevent false positives.

**Validation Modes:**
- `strict = TRUE` (default): Required fields with ANY missing data → validation fails (`is_valid = FALSE`), no files created
- `strict = FALSE` (lenient): Required fields with missing data → validation fails (`is_valid = FALSE`), but files created anyway with warnings

**Key Validation Logic:**
- **Super required fields:** ANY NA values = validation failure (both modes)
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

### January 2025 - Validation System Refinements
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

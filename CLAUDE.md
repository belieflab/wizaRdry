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

### Rule 2: Text Editor - vim ONLY

- ✅ Use programmatic tools (Edit tool, Write tool, sed, awk) when possible
- ✅ If interactive editing needed: **vim ONLY**
- ❌ NEVER suggest nano, emacs, or any other editor

**ALWAYS ASK BEFORE PUSHING. NO EXCEPTIONS.**

---

## Project Overview

wizaRdry is a CRAN R package for NIH-funded computational psychiatry, neuroscience, and psychology research. It provides a data analysis framework with built-in NIH Data Archive (NDA) integration: unified access to multiple data sources (REDCap, MongoDB, Qualtrics, SQL/MariaDB, Oracle) plus workflows for data cleaning and NDA submission preparation.

A parallel `AGENTS.md` exists for other AI assistants; keep the two in sync when documenting major changes.

## Development Commands

There is no Makefile or test runner; development uses devtools from an R session (see `dev/build.R` for the full workflow):

```r
remove.packages("wizaRdry")        # remove previous version
rstudioapi::restartSession()       # restart R

devtools::document()               # regenerate man/ from roxygen comments
devtools::load_all()               # load package for interactive testing
devtools::check()                  # run CRAN checks
devtools::build()                  # build tarball
devtools::install()                # install locally
devtools::spell_check()            # spell check (wordlist in inst/WORDLIST)
devtools::check_win_devel()        # Windows check before CRAN release
```

```r
devtools::test()                   # run the testthat suite (tests/testthat/)
testthat::test_file("tests/testthat/test-nda-interfaces.R")  # single file
```

The package also relies on runtime validation (config/secrets validation on entry, `testSuite()` during `clean()`, NDA compliance checks during `nda()`).

After changing roxygen comments, always run `devtools::document()` before `check()`.

### Test Suite

`tests/testthat/` exercises each data-source interface (csv, redcap, qualtrics, mongo, sql, oracle) through both `clean()` and `nda()` pathways:

- **Mocked layer** (CRAN-safe, offline): each test bootstraps a temp project via `scry()`, writes a fixture-backed script into `clean/{api}/` or `nda/{api}/`, and runs the workflow. The NDA dictionary API is mocked by a local webfakes server (`setup-nda-mock.R` + `fixtures/*.json`); all NDA calls resolve the URL via `get_nda_base_url()` (`R/ndaApi.R`), which reads the `wizaRdry.nda_base_url` option.
- **Live layer** (`test-live-interfaces.R`): skipped unless `WIZARDRY_LIVE_PROJECT` points at a real configured project; per-interface env vars (`WIZARDRY_LIVE_REDCAP`, etc.) name measures to pull against real APIs.
- Key trap: prompt guards are `if (!skip_prompt | !user_prefs$auto_*)`, so tests must pre-seed `.wizaRdry_prefs` with all `auto_*` flags TRUE (done by `local_wizardry_project()` in `helper-project.R`) or `readline()` spins forever headless.
- Hardcoding `https://nda.nih.gov` in R/ is a regression: `grep -rn "nda.nih.gov" R/` must return only `zzz.R` and the `get_nda_base_url()` fallback.

## Architecture Overview

### Configuration System (R6 classes)

- **ConfigEnv** (`R/ConfigEnv.R`): validates `config.yml` — API configurations (mongo, qualtrics, redcap, sql, oracle), `${study_alias}` variable substitution, missing data code mappings. Accessed via `validate_config()`.
- **SecretsEnv** (`R/SecretsEnv.R`): validates `secrets.R` credentials, but only for APIs actually present in config.yml. Accessed via `validate_secrets()` / `get_secret()`.

### Data Access Layer

One function per source, all following the same pattern: loading animation, optional column filtering via `...`, identifiers/superkeys returned first, timing display.

- `redcap(instrument_name, ...)` (`R/getRedcap.R`) — `redcap_event_name` accepts a single string OR a vector of events
- `qualtrics(qualtrics_alias, ...)` (`R/getQualtrics.R`) — multi-institution support; survey IDs mapped in config.yml
- `mongo(collection_name, ...)` (`R/getMongo.R`) — memory-aware chunking and parallel processing for large collections
- `sql(table_name, ...)` (`R/getSql.R`) — auto-joins primary keys table; PII field filtering from config.yml
- `oracle(table_name, ...)` (`R/getOracle.R`) — DSN, DBQ (TNS alias), or host-based connections

Companion utilities follow `source.util()` naming: `redcap.dict()`, `qualtrics.index()`, `qualtrics.rune()`, etc.

### Workflow Functions

**`clean(..., csv=FALSE, rdata=FALSE, spss=FALSE)`** (`R/dataRequest.R`):
- Executes user scripts from `./clean/{csv|mongo|qualtrics|redcap|oracle|sql}/scriptname.R`
- Each script must create a `scriptname_clean` data frame
- Runs validation via `testSuite.R`; offers interactive script creation for missing measures
- User preferences stored in `.wizaRdry_prefs`

**`nda(..., csv=FALSE, rdata=FALSE, spss=FALSE, limited_dataset=TRUE, skip_prompt=TRUE, verbose=FALSE, strict=TRUE, dcc=FALSE)`** (`R/ndaRequest.R`):
- Executes user scripts from `./nda/{api}/structure01.R`; each script must create a data frame named `structure01`
- Fetches structure definitions from the NDA data dictionary API (`https://nda.nih.gov/api/datadictionary/v2`, configurable via `wizaRdry.nda_base_url` option)
- Performs date-shifting (MM/DD/YYYY → MM/01/YYYY) and age-capping unless `limited_dataset=TRUE` (the default)
- Replaces missing data codes per config.yml; strips API-specific columns; normalizes race values

### NDA Validation System (modular, in `R/nda*.R`)

`ndaValidator()` (`R/ndaValidator.R`) orchestrates; the heavy lifting lives in internal modules:

- `ValidationState.R` (R6): accumulates validation results, violations, and `bypassed_validation` flag
- `NdaDataStructure.R` (R6): typed struct for a single NDA field definition; factory functions `nda_structure_from_nda()` / `nda_structure_from_data()`; `to_excel_row()` for export
- `ndaValidationHelpers.R`: value range checking, GUID validation, field completeness
- `ndaTransformations.R`: date/age standardization, type conversions, `convert_array_fields()` (bracket-notation arrays → numeric NDA codes)
- `ndaFieldMapping.R`: field detection and similarity matching
- `ndaFieldSelection.R`: centralized interactive prompts for field selection
- `ndaFileCreation.R`: `should_create_nda_files()` validation gate + submission/definition file creation

Validator also enriches metadata from REDCap (`redcap.dict()`, checkbox `parent___1` handling) and Qualtrics (`qualtrics.dict()` question text).

### Field Constants (`R/zzz.R`)

- `SUPER_REQUIRED_FIELDS` (5, mandatory for ALL submissions): `subjectkey`, `src_subject_id`, `interview_date`, `interview_age`, `sex`. Sourced from ndar_subject01 and added to every structure.
- `DCC_REQUIRED_FIELDS` (7) + `DCC_RECOMMENDED_FIELDS` (4) = `DCC_FIELDS` (11): only merged/validated when `dcc=TRUE`; otherwise excluded and shown as "new" fields in Excel output.
- `NDAR_SKIP_FIELDS`: DCC fields plus internal tracking fields (`state`, `lost_to_followup`, `study_status`) silently dropped during field mapping.

**Validation scope:** only super required fields (and DCC fields when `dcc=TRUE`) are checked for completeness. Structure-level required fields (e.g., `phq9_1`) are NOT validated, to prevent false positives.

**Strict vs lenient:** both modes set `is_valid=FALSE` on violations; the difference is that `strict=TRUE` skips file creation while `strict=FALSE` creates files anyway with warnings.

### NDA Decision Tree and File Outputs

```
Does structure exist in NDA data dictionary?
│
├─ YES, unmodified
│  ├─ strict=TRUE  → files only if validation passes: *_submission.csv
│  └─ strict=FALSE → *_submission.csv (despite data errors)
│
├─ YES, but MODIFIED (new fields OR value range violations)
│  ├─ strict=TRUE  → skip all files
│  └─ strict=FALSE → *_submission_draft.csv + *_definitions.xlsx
│
└─ NO (new structure; validation bypassed, mock structure built)
   ├─ strict=TRUE  → *_definitions.xlsx only (register structure first)
   └─ strict=FALSE → *_submission_draft.csv + *_definitions.xlsx
```

- `*_submission.csv` — upload directly to the NDA portal (structure name on line 1, headers on line 2)
- `*_submission_draft.csv` — test file requiring DCC approval (new/modified structures)
- `*_definitions.xlsx` — field metadata for registering new structures or structure changes with NDA

All outputs go to `tmp/`. "Modified" is detected by comparing data frame columns against the structure's dataElements (excluding `*_complete` and super-required fields) and by `value_range_violations` from the validator.

Key internal functions in `R/ndaRequest.R`: `addNdarSubjectElements()` (fetch ndar_subject01 metadata), `mergeNdarSubjectIntoExisting()` (override existing structure fields), `mergeRequiredMetadata()` (inject metadata into new/mock structures).

### Project Setup & Utilities

- `scry()` (`R/scry.R`) — initializes the wizaRdry project structure (`clean/`, `nda/`, `tmp/`, config.yml, secrets.R templates); `repair=TRUE` fixes incomplete structures
- `sift()` (`R/dataFilter.R`), `meld()` (`R/dataMerge.R`), `qualtrics.rune()` (`R/dataParse.R`)
- Exports: `to.csv()`, `to.rds()`, `to.sav()`, `to.nda()`
- `zzz.R` lifecycle: `.onLoad()` sets mongolite options and sources `secrets.R`; `.onAttach()` checks project structure and duplicate script names

## Code Conventions

- **Exported:** single-word verbs for workflows (`clean()`, `nda()`, `scry()`), source names for data access (`redcap()`), `source.util()` for utilities, `to.format()` for exports
- **Internal:** `verb_noun()` snake_case helpers with `@noRd`; module docs use `@keywords internal`; R6 classes are PascalCase
- **Environment hygiene:** never assign to `globalenv()` — use `.pkg_env$.wizaRdry_env` for internal storage (CRAN requirement); data frames are assigned to the calling environment for user convenience
- `@examples` requiring external data must be wrapped in `\dontrun{}`
- Preserve backward compatibility — many active research projects depend on this package

## Configuration Files (user projects, not this repo)

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

## Common Gotchas

1. **Oracle secrets use UPPERCASE field names** (`DSN`, `DBQ`) per Oracle convention.
2. **`redcap_event_name`** accepts a single string or a vector (e.g., `c("baseline_arm_1", "followup_arm_1")`).
3. **Missing data codes** support category aliases; the NDA validator auto-replaces them with NDA-specific codes.
4. **Value ranges:** a field with no valueRange defined and an unbounded type (String/GUID/Date/Integer/Float) is valid — only data outside a *defined* valueRange is a violation.
5. **MongoDB queries** auto-chunk based on available system memory (cross-platform detection) to avoid OOM.
6. **`limited_dataset` defaults to TRUE** in `nda()` — date-shifting/age-capping only happens when it is FALSE.

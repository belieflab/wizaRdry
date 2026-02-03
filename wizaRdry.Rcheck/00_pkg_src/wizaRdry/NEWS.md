# wizaRdry 0.6.1

## Bug Fixes

- **NDA Validation:** Fixed critical bug causing "cannot add bindings to locked 
  environment" errors when running `nda()` after package installation. Reverted 
  commits dce7cbd and 41ec81d which introduced data retrieval location mismatch. 
  Code now works correctly after formal installation via `R CMD INSTALL .`

## CRAN Compliance

- **Environment Management:** Eliminated all global environment pollution to satisfy
  CRAN policies. Package now uses proper package environment (`.pkg_env`) instead of
  `globalenv()` for internal data storage. This fixes 2 CRAN violations and removes
  43 `globalenv()` references throughout the package.
  
  **Technical Details:** All internal dataframes are now stored in `.pkg_env$.wizaRdry_env`
  instead of being assigned to `globalenv()`. Dataframes are still optionally assigned
  to the calling environment for user convenience, maintaining backward compatibility.
  
  **User Impact:** None - dataframes remain available in your working environment
  through the calling environment assignment mechanism. Package behavior is identical
  from user perspective.

- **Code Cleanup:** Removed legacy `ndaValidator_legacy.R` file (~4,262 lines) that
  was no longer maintained or used.

---

# wizaRdry 0.1.0
* complete data framework based on 3 years of R&D on NIH multi-site consortia

## Initial submission
* 0 warnings
* 0 notes

## New features
* scry() function to build out project

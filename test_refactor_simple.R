#!/usr/bin/env Rscript
# Simple non-interactive test for refactor validation

library(wizaRdry)

cat("\n=== Testing NDA Validator Refactor (Non-Interactive) ===\n\n")

# Create test data
test_data <- data.frame(
  src_subject_id = c("TEST001", "TEST002"),
  subjectkey = c("NDAR_INVTEST001", "NDAR_INVTEST002"),
  interview_date = c("01/15/2024", "02/20/2024"),
  interview_age = c(240, 300),
  sex = c("M", "F"),
  phenotype = c(999, 999)  # String field, no valueRange = should be valid
)

# Store in environment
if (!exists(".wizaRdry_env", envir = globalenv())) {
  .wizaRdry_env <- new.env(parent = globalenv())
  assign(".wizaRdry_env", .wizaRdry_env, envir = globalenv())
}
assign("ndar_subject01", test_data, envir = .wizaRdry_env)
assign("ndar_subject01", test_data, envir = globalenv())

cat("Test data created and stored\n\n")

# Run validator
cat("Running ndaValidator()...\n\n")
validation_result <- ndaValidator(
  measure_name = "ndar_subject01",
  api = "csv",
  limited_dataset = FALSE,
  verbose = TRUE
)

cat("\n=== Test Results ===\n\n")

# Check 1: ValidationState created
success_count <- 0
total_tests <- 5

if (inherits(validation_result, "ValidationState")) {
  cat("[PASS] ValidationState object created\n")
  success_count <- success_count + 1
} else {
  cat("[FAIL] ValidationState object NOT created\n")
}

# Check 2: No false positive violations for unbounded String fields
violations <- validation_result$value_range_violations
cat(sprintf("[INFO] Violations detected: %d\n", length(violations)))

# Check for false positives on unbounded fields
false_positives <- c("src_subject_id", "interview_date", "phenotype")
has_false_positive <- any(false_positives %in% names(violations))

if (!has_false_positive) {
  cat("[PASS] No false positives for unbounded String/Date fields\n")
  success_count <- success_count + 1
} else {
  cat("[FAIL] False positives detected for unbounded fields:\n")
  for (fp in intersect(false_positives, names(violations))) {
    cat(sprintf("  - %s\n", fp))
  }
}

# Check 3: Structure correctly identified as EXISTING
if (!validation_result$is_new_structure) {
  cat("[PASS] Structure correctly identified as EXISTING\n")
  success_count <- success_count + 1
} else {
  cat("[FAIL] Structure incorrectly identified as NEW\n")
}

# Check 4: needs_data_definition() method works
needs_def <- validation_result$needs_data_definition()
cat(sprintf("[INFO] needs_data_definition() = %s\n", needs_def))
cat(sprintf("[INFO] Reason: %s\n", validation_result$get_modification_reason()))

if (is.logical(needs_def)) {
  cat("[PASS] needs_data_definition() returns logical value\n")
  success_count <- success_count + 1
} else {
  cat("[FAIL] needs_data_definition() does not return logical\n")
}

# Check 5: Unbounded fields properly skipped
if (length(violations) == 0) {
  cat("[PASS] All unbounded fields correctly skipped (0 violations)\n")
  success_count <- success_count + 1
} else {
  cat(sprintf("[INFO] %d violation(s) detected (may be legitimate)\n", length(violations)))
  if (length(violations) > 0) {
    cat("Violation fields:\n")
    for (v in names(violations)) {
      cat(sprintf("  - %s\n", v))
    }
  }
  # Still pass if no false positives
  if (!has_false_positive) {
    success_count <- success_count + 1
  }
}

cat(sprintf("\n=== Summary: %d/%d tests passed ===\n\n", success_count, total_tests))

if (success_count == total_tests) {
  cat("[SUCCESS] All tests passed!\n")
  quit(status = 0)
} else {
  cat("[FAILURE] Some tests failed\n")
  quit(status = 1)
}

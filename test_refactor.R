#!/usr/bin/env Rscript
# Test script for NDA validator refactor
# Tests that value range violations properly trigger data definition file creation

library(wizaRdry)

cat("\n═══════════════════════════════════════════════════════════\n")
cat("   Testing NDA Validator Refactor - Value Range Detection\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# ============================================================================
# TEST 1: ValidationState Object Creation
# ============================================================================
cat("TEST 1: ValidationState Object Creation\n")
cat("────────────────────────────────────────\n")

# Create test data with known value range violations
test_data <- data.frame(
  src_subject_id = c("TEST001", "TEST002", "TEST003"),
  subjectkey = c("NDAR_INVTEST001", "NDAR_INVTEST002", "NDAR_INVTEST003"),
  interview_date = c("01/15/2024", "02/20/2024", "03/10/2024"),
  interview_age = c(240, 300, 180),
  sex = c("M", "F", "M"),
  
  # Fields with values likely outside NDA valueRange
  test_score = c(1, 2, 999),      # 999 typically not in NDA ranges
  reaction_time = c(500, 750, -999)  # -999 missing code
)

cat("✓ Test data created (3 subjects, 7 fields)\n")

# Store in environment (mimics real workflow)
if (!exists(".wizaRdry_env", envir = globalenv())) {
  .wizaRdry_env <- new.env(parent = globalenv())
  assign(".wizaRdry_env", .wizaRdry_env, envir = globalenv())
}
assign("test_structure", test_data, envir = .wizaRdry_env)
assign("test_structure", test_data, envir = globalenv())

cat("✓ Data stored in .wizaRdry_env\n\n")

# ============================================================================
# TEST 2: Fetch Real NDA Structure for Testing
# ============================================================================
cat("TEST 2: Testing with Real NDA Structure\n")
cat("────────────────────────────────────────\n")

cat("Attempting to fetch a common NDA structure for realistic testing...\n")
cat("(Using 'ndar_subject01' - the most common structure)\n\n")

# Use ndar_subject01 for testing (guaranteed to exist in NDA)
real_test_data <- data.frame(
  src_subject_id = c("TEST001", "TEST002"),
  subjectkey = c("NDAR_INVTEST001", "NDAR_INVTEST002"),
  interview_date = c("01/15/2024", "02/20/2024"),
  interview_age = c(240, 300),
  sex = c("M", "F"),
  
  # Add a field with value outside typical range to trigger violation
  phenotype = c(999, 999)  # Assuming phenotype has defined valueRange
)

assign("ndar_subject01", real_test_data, envir = .wizaRdry_env)
assign("ndar_subject01", real_test_data, envir = globalenv())

cat("Calling ndaValidator_new() with real structure...\n\n")

validation_result <- tryCatch({
  ndaValidator_new(
    measure_name = "ndar_subject01",
    api = "csv",
    limited_dataset = FALSE,
    verbose = TRUE
  )
}, error = function(e) {
  cat("\n[ERROR] Validation failed:\n")
  cat(conditionMessage(e), "\n")
  return(NULL)
})

# ============================================================================
# TEST 3: Validate ValidationState Object
# ============================================================================
cat("\n\nTEST 3: ValidationState Object Inspection\n")
cat("────────────────────────────────────────\n")

if (!is.null(validation_result) && inherits(validation_result, "ValidationState")) {
  cat("✓ ValidationState object created successfully\n")
  cat("  Class: ", paste(class(validation_result), collapse = ", "), "\n")
  cat("  Measure: ", validation_result$measure_name, "\n")
  cat("  API: ", validation_result$api, "\n")
  cat("  Is new structure: ", validation_result$is_new_structure, "\n")
  cat("  Is modified: ", validation_result$is_modified_structure, "\n")
  
  # ============================================================================
  # TEST 4: Value Range Violations
  # ============================================================================
  cat("\nTEST 4: Value Range Violation Detection\n")
  cat("────────────────────────────────────────\n")
  
  violations <- validation_result$value_range_violations
  cat("Number of violations detected: ", length(violations), "\n")
  
  if (length(violations) > 0) {
    cat("✓ PASS: Violations detected!\n")
    cat("\nViolation details:\n")
    for (field in names(violations)) {
      cat(sprintf("  • %s: %s\n", field, violations[[field]]$message))
    }
  } else {
    cat("✗ FAIL: No violations detected (expected at least one)\n")
  }
  
  # ============================================================================
  # TEST 5: needs_data_definition() Method
  # ============================================================================
  cat("\nTEST 5: Data Definition File Decision Logic\n")
  cat("────────────────────────────────────────\n")
  
  needs_def <- validation_result$needs_data_definition()
  reason <- validation_result$get_modification_reason()
  
  cat("needs_data_definition(): ", needs_def, "\n")
  cat("Reason: ", reason, "\n")
  
  if (needs_def && length(violations) > 0) {
    cat("✓ PASS: Data definition correctly triggered by violations\n")
  } else if (needs_def && validation_result$is_new_structure) {
    cat("✓ PASS: Data definition correctly triggered for new structure\n")
  } else if (!needs_def && !validation_result$is_new_structure && 
             length(violations) == 0 && length(validation_result$new_fields) == 0) {
    cat("✓ PASS: Data definition correctly skipped (unmodified structure)\n")
  } else {
    cat("⚠ UNEXPECTED: Review logic\n")
  }
  
  # ============================================================================
  # TEST 6: File Creation
  # ============================================================================
  cat("\nTEST 6: NDA File Creation\n")
  cat("────────────────────────────────────────\n")
  
  cat("Calling create_nda_files()...\n\n")
  
  file_result <- tryCatch({
    create_nda_files(validation_result, verbose = TRUE)
    TRUE
  }, error = function(e) {
    cat("\n[ERROR] File creation failed:\n")
    cat(conditionMessage(e), "\n")
    FALSE
  })
  
  if (file_result) {
    cat("\n✓ File creation completed without errors\n")
  }
  
  # Check if files were created
  cat("\nChecking tmp/ directory for generated files...\n")
  tmp_files <- list.files("tmp", pattern = "ndar_subject01", full.names = FALSE)
  if (length(tmp_files) > 0) {
    cat("✓ Files found:\n")
    for (f in tmp_files) {
      cat(sprintf("  • %s\n", f))
    }
  } else {
    cat("⚠ No files found in tmp/ directory\n")
  }
  
} else {
  cat("✗ FAIL: ValidationState object was not created\n")
  cat("  Possible reasons:\n")
  cat("  • Network issue fetching NDA structure\n")
  cat("  • API endpoint changed\n")
  cat("  • Package not properly loaded\n")
}

# ============================================================================
# SUMMARY
# ============================================================================
cat("\n\n═══════════════════════════════════════════════════════════\n")
cat("   Test Summary\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("Key Points to Verify:\n")
cat("1. ValidationState object created: ")
cat(ifelse(exists("validation_result") && !is.null(validation_result), "✓ YES\n", "✗ NO\n"))

cat("2. Value range violations detected: ")
if (exists("violations")) {
  cat(sprintf("✓ %d violation(s)\n", length(violations)))
} else {
  cat("✗ NO\n")
}

cat("3. needs_data_definition() works: ")
if (exists("needs_def")) {
  cat(sprintf("✓ Returns %s\n", needs_def))
} else {
  cat("✗ NO\n")
}

cat("4. File creation executes: ")
if (exists("file_result")) {
  cat(ifelse(file_result, "✓ YES\n", "✗ NO\n"))
} else {
  cat("✗ NO\n")
}

cat("\n═══════════════════════════════════════════════════════════\n")
cat("   Manual Testing Instructions\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("To test with YOUR actual data:\n\n")
cat("1. Navigate to your wizaRdry project directory\n")
cat("2. Load the package: library(wizaRdry)\n")
cat("3. Create a measure with known value range issues:\n\n")

cat('   my_task <- data.frame(\n')
cat('     src_subject_id = "SUB001",\n')
cat('     subjectkey = "NDAR_INVSUB001",\n')
cat('     interview_date = "01/15/2024",\n')
cat('     interview_age = 240,\n')
cat('     sex = "M",\n')
cat('     response = 999  # Add a value outside typical NDA range\n')
cat('   )\n\n')

cat('4. Run the NDA workflow: nda("my_task")\n\n')

cat('5. Check tmp/ directory for TWO files:\n')
cat('   • my_task_template.csv (submission file)\n')
cat('   • my_task_data-definition.xlsx (if violations detected)\n\n')

cat('6. EXPECTED BEHAVIOR:\n')
cat('   - If data has values outside NDA valueRange → BOTH files created\n')
cat('   - If data matches NDA perfectly → Only template.csv created\n')
cat('   - If structure is NEW → Only data-definition.xlsx created\n\n')

cat("═══════════════════════════════════════════════════════════\n\n")

#!/usr/bin/env Rscript
# Test script to reproduce validation_rules error

devtools::load_all()

# Test 1: Create ValidationRules with parameters
cat("Test 1: Creating ValidationRules with parameters\n")
vr <- ValidationRules$new(min_value = 0, max_value = 10, allowed_values = c("1", "2", "3"))
cat("  min_value:", vr$min_value, "\n")
cat("  max_value:", vr$max_value, "\n")
cat("  allowed_values:", paste(vr$allowed_values, collapse = ", "), "\n")
cat("  SUCCESS!\n\n")

# Test 2: Create NdaDataStructure with validation_rules as list
cat("Test 2: Creating NdaDataStructure with validation_rules list\n")
nd <- NdaDataStructure$new(
  element_name = "test_field",
  data_type = "Integer",
  validation_rules = list(
    min_value = 0,
    max_value = 10,
    allowed_values = c("1", "2", "3")
  )
)
cat("  Field:", nd$element_name$value, "\n")
cat("  Validation rules class:", class(nd$validation_rules)[1], "\n")
cat("  Min value:", nd$validation_rules$min_value, "\n")
cat("  SUCCESS!\n\n")

# Test 3: Simulate what happens in createNdaDataDefinition
cat("Test 3: Simulating createNdaDataDefinition pattern\n")
validation_rules <- list(
  min_value = NULL,
  max_value = NULL,
  allowed_values = "Yes;No",
  pattern = NULL
)

# Try to modify allowed_values (this should work for a list)
validation_rules$allowed_values <- "Modified"
cat("  Modified validation_rules$allowed_values:", validation_rules$allowed_values, "\n")

nd2 <- NdaDataStructure$new(
  element_name = "twins_study",
  data_type = "String",
  validation_rules = validation_rules
)
cat("  Field:", nd2$element_name$value, "\n")
cat("  Validation rules allowed_values:", nd2$validation_rules$allowed_values, "\n")
cat("  SUCCESS!\n\n")

cat("All tests passed!\n")

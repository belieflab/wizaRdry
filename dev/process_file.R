process_file <- function(source_path, dest_path) {
  # Read the original file
  lines <- readLines(source_path)

  # Define more precise patterns for library and require calls
  library_pattern <- "\\b(library|require)\\s*\\([^)]+\\)"
  require_check_pattern <- "\\bif\\s*\\(\\s*!\\s*require\\s*\\("
  api_pattern <- "(source|list\\.files)\\s*\\([\"']api/"
  api_ref_pattern <- "\\bapi/[^_]"

  # Filter lines - keep only those that DON'T match our removal patterns
  # unless they contain "nda_base_url" or are conditional api checks
  kept_lines <- character(0)
  for (line in lines) {
    # Check if line contains "grepl" and "api/" to exclude conditional checks
    contains_grepl_api <- grepl("grepl\\([\"']api/", line)

    # Check if line matches any pattern but exclude lines with nda_base_url or conditional api checks
    if ((grepl(library_pattern, line) ||
         grepl(require_check_pattern, line) ||
         grepl(api_pattern, line) ||
         (grepl(api_ref_pattern, line) && !contains_grepl_api)) &&
        !grepl("nda_base_url", line)) {
      # Skip this line (don't add to kept_lines)
    } else {
      # Keep this line
      kept_lines <- c(kept_lines, line)
    }
  }

  # Write to new location
  writeLines(kept_lines, dest_path)
  message(sprintf("File copied from %s to %s with api/ references and library calls removed",
                  source_path, dest_path))
}

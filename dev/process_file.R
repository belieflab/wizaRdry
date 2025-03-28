process_file <- function(source_path, dest_path) {
  # Read the original file
  lines <- readLines(source_path)
  # Process each line with more robust patterns
  modified_lines <- sapply(lines, function(line) {
    # Define more precise patterns for library and require calls
    library_pattern <- "\\b(library|require)\\s*\\([^)]+\\)"
    require_check_pattern <- "\\bif\\s*\\(\\s*!\\s*require\\s*\\("
    api_pattern <- "(source|list\\.files)\\s*\\([\"']api/"
    api_ref_pattern <- "\\bapi/[^_]"

    # Check if line contains "grepl" and "api/" to exclude conditional checks
    contains_grepl_api <- grepl("grepl\\([\"']api/", line)

    # Check if line matches any pattern but exclude lines with nda_base_url or conditional api checks
    if ((grepl(library_pattern, line) ||
         grepl(require_check_pattern, line) ||
         grepl(api_pattern, line) ||
         (grepl(api_ref_pattern, line) && !contains_grepl_api)) &&
        !grepl("nda_base_url", line)) {
      return(paste0("# ", line))  # Comment out the line
    } else {
      return(line)  # Keep line as is
    }
  })
  # Write to new location
  writeLines(modified_lines, dest_path)
  message(sprintf("File copied from %s to %s with api/ references and library calls commented out",
                  source_path, dest_path))
}

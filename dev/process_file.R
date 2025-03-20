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

    # Check if line matches any pattern
    if (grepl(library_pattern, line) ||
        grepl(require_check_pattern, line) ||
        grepl(api_pattern, line) ||
        grepl(api_ref_pattern, line)) {
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

process_file <- function(source_path, dest_path) {
  # Read the original file
  lines <- readLines(source_path)

  # Process each line - comment out lines with api/ references and library/require calls
  modified_lines <- sapply(lines, function(line) {
    if (grepl("source\\([\"']api/", line) ||               # source() calls with "api/"
        grepl("list\\.files\\([\"']api/", line) ||         # list.files() calls with "api/"
        grepl("\\bapi/[^_]", line) ||                      # "api/" but not followed by underscore
        grepl("if\\s*\\(!require\\(", line) ||             # require() checks
        grepl("library\\(", line) ||                        # library() calls
        grepl("require\\(", line)) {                        # require() calls
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

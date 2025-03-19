# Copy file with content modification
process_file <- function(source_path, dest_path) {
  # Read the original file
  lines <- readLines(source_path)

  # Process each line - comment out lines with api/ references
  modified_lines <- sapply(lines, function(line) {
    if (grepl("^\\s*api/", line) ||
        grepl("source\\([\"']api/", line) ||
        grepl("list\\.files\\([\"']api/", line) ||
        grepl("\\bapi/", line)) {
      return(paste0("# ", line))  # Comment out the line
    } else {
      return(line)  # Keep line as is
    }
  })

  # Write to new location
  writeLines(modified_lines, dest_path)
  message(sprintf("File copied from %s to %s with api/ references commented out",
                  source_path, dest_path))
}

process_file("../api/getRedcap.R", "R/getRedcap.R")




file.copy(from = "../api/SecretsEnv.R", to = "R", overwrite = TRUE)
file.copy(from = "../api/ConfigEnv.R", to = "R", overwrite = TRUE)




file.copy(from = "../api/src/shortcuts.R", to = "R/aliases.R", overwrite = TRUE)
file.copy(from = "../api/src/animations.R", to = "R/animations.R", overwrite = TRUE)

# Use the function
process_file("../api/getSurvey.R", "R/getQualtrics.R")


process_file("../api/getTask.R", "R/getMongo.R")

file.copy(from = "../api/src/addPrefixToColumns.R", to = "R/addPrefixToColumns.R", overwrite = TRUE)




#process_file("../api/dataRequest.R", "R/dataRequest.R")
#process_file("../api/ndaRequest.R", "R/ndaRequest.R")

#' Check for Duplicate Script Names
#'
#' This function checks for duplicate script names across the data source folders
#' to prevent naming conflicts. Checks clean/ and nda/ directories separately.
#'
#' @param path Character string specifying the directory path to check.
#'        Defaults to the current working directory.
#' @param quiet Logical. If TRUE, will not print messages about non-duplicate scripts.
#'        Defaults to TRUE.
#'
#' @return Invisibly returns a list of duplicate script names and their locations.
#'         If no duplicates are found, returns NULL.
#' 
#' @noRd
checkDuplicateScripts <- function(path = ".", quiet = TRUE) {
  duplicates_found <- FALSE
  result <- list()
  
  # Check clean/ subdirectories
  clean_dirs <- c(
    redcap = file.path(path, "clean", "redcap"),
    qualtrics = file.path(path, "clean", "qualtrics"),
    mongo = file.path(path, "clean", "mongo")
  )
  
  # Check if clean directories exist
  clean_dir_exists <- sapply(clean_dirs, dir.exists)
  
  if (any(clean_dir_exists)) {
    # Get all script names from each folder
    scripts <- list()
    for (dir_name in names(clean_dirs)[clean_dir_exists]) {
      scripts[[dir_name]] <- tools::file_path_sans_ext(
        list.files(clean_dirs[dir_name], pattern = "\\.R$")
      )
    }
    
    # Create a map to count occurrences of each script name
    script_counts <- table(unlist(scripts))
    
    # Find duplicates (count > 1)
    duplicates <- names(script_counts[script_counts > 1])
    
    # Check if there are any duplicates
    if (length(duplicates) > 0) {
      duplicates_found <- TRUE
      message("WARNING: Found duplicate script names in clean/ folders:")
      
      # For each duplicate, show which folders they appear in
      clean_duplicate_info <- list()
      for (dup in duplicates) {
        locations <- names(scripts)[sapply(scripts, function(x) dup %in% x)]
        locations_full <- sprintf("clean/%s", locations)
        
        message(sprintf("  - Script '%s.R' appears in: %s", 
                        dup, paste(locations_full, collapse = ", ")))
        
        clean_duplicate_info[[dup]] <- locations_full
      }
      
      result$clean <- clean_duplicate_info
    } else if (!quiet) {
      message("No duplicate script names found in clean/ folders.")
    }
  }
  
  # Check nda/ subdirectories
  nda_dirs <- c(
    redcap = file.path(path, "nda", "redcap"),
    qualtrics = file.path(path, "nda", "qualtrics"),
    mongo = file.path(path, "nda", "mongo")
  )
  
  # Check if nda directories exist
  nda_dir_exists <- sapply(nda_dirs, dir.exists)
  
  if (any(nda_dir_exists)) {
    # Get all script names from each folder
    scripts <- list()
    for (dir_name in names(nda_dirs)[nda_dir_exists]) {
      scripts[[dir_name]] <- tools::file_path_sans_ext(
        list.files(nda_dirs[dir_name], pattern = "\\.R$")
      )
    }
    
    # Create a map to count occurrences of each script name
    script_counts <- table(unlist(scripts))
    
    # Find duplicates (count > 1)
    duplicates <- names(script_counts[script_counts > 1])
    
    # Check if there are any duplicates
    if (length(duplicates) > 0) {
      duplicates_found <- TRUE
      message("WARNING: Found duplicate script names in nda/ folders:")
      
      # For each duplicate, show which folders they appear in
      nda_duplicate_info <- list()
      for (dup in duplicates) {
        locations <- names(scripts)[sapply(scripts, function(x) dup %in% x)]
        locations_full <- sprintf("nda/%s", locations)
        
        message(sprintf("  - Script '%s.R' appears in: %s", 
                        dup, paste(locations_full, collapse = ", ")))
        
        nda_duplicate_info[[dup]] <- locations_full
      }
      
      result$nda <- nda_duplicate_info
    } else if (!quiet) {
      message("No duplicate script names found in nda/ folders.")
    }
  }
  
  if (duplicates_found) {
    message("\nDuplicate scripts can cause conflicts when processing data.")
    message("Please rename one of the scripts to avoid conflicts.")
    return(invisible(result))
  } else {
    if (!quiet) {
      message("No duplicate script names found.")
    }
    return(invisible(NULL))
  }
}

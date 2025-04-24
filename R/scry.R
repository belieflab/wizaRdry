#' Initialize the wizaRdry directory structure inside an R project
#'
#' Creates the standard directory structure required for the wizaRdry package to function properly.
#' This includes folders for data cleaning scripts, NDA submission templates, and temporary outputs.
#' It can detect and repair incomplete directory structures, and optionally create an R project.
#'
#' @param study_alias Character string specifying the short name for the study e.g. impact, capr, sing
#' @param path Character string specifying the directory path where the structure should be created.
#'        Defaults to the current working directory.
#' @param overwrite Logical. If TRUE, will overwrite existing files. If FALSE (default),
#'        will not replace existing files.
#' @param repair Logical. If TRUE, will attempt to repair an incomplete directory structure.
#'        If FALSE, will abort with an error message when encountering an incomplete structure.
#' @param show_tree Logical. If TRUE (default on first run), will display a visual file tree.
#'        Set to FALSE to suppress the tree view.
#' @param create_project Logical. If TRUE, will create an R project file if one doesn't exist.
#'        If FALSE (default), will not create an R project.
#' @param examples Logical. If TRUE (default when not repairing), will create example script templates.
#'        If FALSE (default when repairing), will skip creating example scripts.
#' @param skip_prompt Logical. If TRUE, will skip the initial confirmation prompt. Defaults to FALSE.
#'
#' @return Invisible TRUE if successful.
#'
#' @details
#' The function creates the following directory structure:
#' - clean/
#'   - mongo/
#'   - qualtrics/
#'   - redcap/
#' - nda/
#'   - mongo/
#'   - qualtrics/
#'   - redcap/
#' - tmp/
#'
#' It also creates template config.yml and secrets.R files, and optionally an R project file.
#'
#' @examples
#' \dontrun{
#' # Initialize in current directory
#' scry()
#' 
#' # Repair structure in current directory
#' scry(repair = TRUE)
#' 
#' # Initialize in a specific directory with an R project
#' scry("path/to/project", create_project = TRUE, repair = TRUE)
#' 
#' # Skip the tree display
#' scry(repair = TRUE, show_tree = FALSE)
#' 
#' # Explicitly create example scripts when repairing
#' scry(repair = TRUE, examples = TRUE)
#' 
#' # Skip the confirmation prompt
#' scry(skip_prompt = TRUE)
#' }
#'
#' @export
scry <- function(study_alias = NULL, path = ".", overwrite = FALSE, repair = FALSE, show_tree = NULL,
                 create_project = FALSE, examples = FALSE, skip_prompt = FALSE) {
  
  # Check for user preferences file
  user_prefs_file <- file.path(path, ".wizaRdry_prefs")
  user_prefs <- list(shown_tree = FALSE, auto_create = FALSE)
  
  if (file.exists(user_prefs_file)) {
    tryCatch({
      user_prefs <- readRDS(user_prefs_file)
    }, error = function(e) {
      # If file exists but can't be read, create a new one
      user_prefs <- list(shown_tree = FALSE, auto_create = FALSE)
    })
  }
  
  # If skip_prompt is TRUE or user has previously set auto_create to TRUE, bypass the prompt
  if (!skip_prompt && !user_prefs$auto_create) {
    response <- readline(prompt = "Would you like to create the wizaRdry project structure? y/n ")
    
    while (!tolower(response) %in% c("y", "n")) {
      response <- readline(prompt = "Please enter either y or n: ")
    }
    
    # Ask if they want to remember this choice
    if (tolower(response) == "y") {
      remember <- readline(prompt = "Would you like to remember this choice and skip this prompt in the future? y/n ")
      
      while (!tolower(remember) %in% c("y", "n")) {
        remember <- readline(prompt = "Please enter either y or n: ")
      }
      
      if (tolower(remember) == "y") {
        user_prefs$auto_create <- TRUE
        saveRDS(user_prefs, user_prefs_file)
        message("Your preference has been saved. Use scry(skip_prompt = FALSE) to show this prompt again.")
      }
    }
    
    if (tolower(response) == "n") {
      # Instead of stopping with an error, return invisibly
      return(invisible(NULL))
    }
  }
  
  # If study_alias is NULL (not provided), try to detect from .Rproj file
  if (is.null(study_alias)) {
    # Look for .Rproj files in the specified path
    rproj_files <- list.files(path, pattern = "\\.Rproj$", full.names = FALSE)
    
    if (length(rproj_files) > 0) {
      # Extract project name from the first .Rproj file found
      project_name <- sub("\\.Rproj$", "", rproj_files[1])
      study_alias <- project_name
      message(paste0("Using project name as study alias: ", study_alias))
    } else {
      # Default to FALSE if no .Rproj file found
      study_alias <- FALSE
    }
  }
  
  # Define directory structure
  expected_dirs <- c(
    file.path(path, "clean"),
    file.path(path, "clean", "mongo"),
    file.path(path, "clean", "qualtrics"),
    file.path(path, "clean", "redcap"),
    file.path(path, "nda"),
    file.path(path, "nda", "mongo"),
    file.path(path, "nda", "qualtrics"),
    file.path(path, "nda", "redcap"),
    file.path(path, "tmp")
  )
  
  # Define expected files
  expected_files <- c(
    file.path(path, "config.yml"),
    file.path(path, "secrets.R"),
    file.path(path, "main.R")
  )
  
  # Check if this looks like a wizaRdry project structure
  has_clean <- dir.exists(file.path(path, "clean"))
  has_nda <- dir.exists(file.path(path, "nda"))
  has_config <- file.exists(file.path(path, "config.yml"))
  has_secrets <- file.exists(file.path(path, "secrets.R"))
  has_main <- file.exists(file.path(path, "main.R"))
  
  # If structure partially exists but is incomplete
  structure_exists <- has_clean || has_nda || has_config || has_secrets || has_main
  structure_complete <- all(sapply(expected_dirs, dir.exists)) && 
    all(sapply(expected_files, file.exists))
  
  # Determine if we should show the tree
  if (is.null(show_tree)) {
    show_tree <- !user_prefs$shown_tree
  }
  
  # Handle incomplete structures
  if (structure_exists && !structure_complete) {
    if (!repair) {
      missing_dirs <- expected_dirs[!sapply(expected_dirs, dir.exists)]
      missing_files <- expected_files[!sapply(expected_files, file.exists)]
      
      stop(
        "Incomplete wizaRdry directory structure detected.\n",
        "Missing components:\n",
        if(length(missing_dirs) > 0) paste0("  Directories: ", paste(missing_dirs, collapse=", "), "\n") else "",
        if(length(missing_files) > 0) paste0("  Files: ", paste(missing_files, collapse=", "), "\n") else "",
        "Use scry(repair = TRUE) to repair the structure."
      )
    } else {
      message("Repairing incomplete wizaRdry directory structure...")
    }
  }
  
  # Create directories
  created <- character(0)
  for (dir in expected_dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      created <- c(created, dir)
    } else if (overwrite) {
      created <- c(created, paste0(dir, " (already exists)"))
    } else {
      created <- c(created, paste0(dir, " (skipped, already exists)"))
    }
  }
  
  # Create .gitkeep files to ensure empty directories are tracked by git
  for (dir in expected_dirs) {
    if (dir != path) {  # Don't add .gitkeep to the root directory
      gitkeep_file <- file.path(dir, ".gitkeep")
      if (!file.exists(gitkeep_file)) {
        file.create(gitkeep_file)
      }
    }
  }
  
  # Add tmp directory contents to .gitignore
  gitignore_file <- file.path(path, ".gitignore")
  tmp_pattern <- "tmp/*"
  secrets_pattern <- "secrets.R"
  pem_pattern <- "*.pem"
  prefs_pattern <- ".wizaRdry_prefs"
  
  if (file.exists(gitignore_file)) {
    gitignore_content <- readLines(gitignore_file)
    need_to_update <- FALSE
    
    # Check for secrets.R
    if (!any(grepl(secrets_pattern, gitignore_content, fixed = TRUE))) {
      gitignore_content <- c(gitignore_content, secrets_pattern)
      need_to_update <- TRUE
      message("Added secrets.R to .gitignore")
    }
    
    # Check for tmp/* pattern
    if (!any(grepl(tmp_pattern, gitignore_content, fixed = TRUE))) {
      gitignore_content <- c(gitignore_content, tmp_pattern)
      need_to_update <- TRUE
      message("Added tmp/* to .gitignore")
    }
    
    # Check for *.pem pattern
    if (!any(grepl(pem_pattern, gitignore_content, fixed = TRUE))) {
      gitignore_content <- c(gitignore_content, pem_pattern)
      need_to_update <- TRUE
      message("Added *.pem to .gitignore")
    }
    
    # Check for .wizaRdry_prefs pattern
    if (!any(grepl(prefs_pattern, gitignore_content, fixed = TRUE))) {
      gitignore_content <- c(gitignore_content, prefs_pattern)
      need_to_update <- TRUE
      message("Added .wizaRdry_prefs to .gitignore")
    }
    
    if (need_to_update) {
      writeLines(gitignore_content, gitignore_file)
    }
  } else {
    # Create new .gitignore with all patterns
    writeLines(c(secrets_pattern, tmp_pattern, pem_pattern, prefs_pattern), gitignore_file)
    message("Created .gitignore with secrets.R, *.pem, tmp/*, and .wizaRdry_prefs patterns")
  }
  
  # Ensure .gitkeep exists in tmp directory but is tracked
  # This allows the tmp directory to be committed while its contents are ignored
  tmp_gitkeep <- file.path(path, "tmp", ".gitkeep")
  if (!file.exists(tmp_gitkeep)) {
    file.create(tmp_gitkeep)
    message("Created .gitkeep in tmp directory")
  }
  
  # Create template config.yml file
  config_file <- file.path(path, "config.yml")
  config_template <- paste(
    "default:",
    paste0("  study_alias: ", ifelse(isFALSE(study_alias), "test", tolower(study_alias))),
    "  identifier: src_subject_id",
    "  mongo:",
    "    #collection: ${study_alias}",
    "  qualtrics:",
    "    #survey_ids:",
    "      Institution1:",
    "        foo: \"SV_\"",
    "      Institution2:",
    "        bar: \"SV_\"",
    "  redcap:",
    "    #superkey: ndar_subject01",
    sep = "\n"
  )
  
  if (!file.exists(config_file) || overwrite) {
    writeLines(config_template, config_file)
    created <- c(created, config_file)
  } else {
    created <- c(created, paste0(config_file, " (skipped, already exists)"))
  }
  
  # Create template secrets.R file
  secrets_file <- file.path(path, "secrets.R")
  secrets_template <- paste(
    "# THE FIRST  RULE OF SECRETS.R IS YOU DO NOT PUSH SECRETS.R",
    "# THE SECOND RULE OF SECRETS.R IS YOU DO NOT PUSH SECRETS.R",
    "# THE THIRD  RULE OF SECRETS.R IS YOU DO NOT MODIFY .GITIGNORE",
    "",
    "# Qualtrics",
    "apiKeys <- c(\"\")",
    "baseUrls <- c(\"\")",
    "",
    "# REDCap",
    "uri <- \"\"",
    "token <- \"\"",
    "",
    "# Mongo",
    "connectionString <- \"\"",
    sep = "\n"
  )
  
  if (!file.exists(secrets_file) || overwrite) {
    writeLines(secrets_template, secrets_file)
    created <- c(created, secrets_file)
  } else {
    created <- c(created, paste0(secrets_file, " (skipped, already exists)"))
  }
  
  # Create template main.R file
  main_file <- file.path(path, "main.R")
  
  if (!file.exists(main_file) || overwrite) {
    writeLines(c(
      "# Main analysis script for this wizaRdry project",
      "",
      "# Load wizaRdry library:",
      "if(!require(wizaRdry)) {install.packages('wizaRdry')}; library(wizaRdry)",
      "",
      "# Check available REDCap forms:",
      "# redcap.index()",
      "",
      "# Example NDA request:",
      "# Create remediation script in nda/",
      "# nda(\"cde_dsmcrossad01\")",
      "",
      "# Your analysis code here",
      ""
    ), main_file)
    created <- c(created, main_file)
  } else {
    created <- c(created, paste0(main_file, " (skipped, already exists)"))
  }
  
  # Define template files with their content
  clean_templates <- list(
    collection = list(
      path = file.path(path, "clean", "mongo", "collection.R"),
      content = paste(
        "#",
        "# clean/mongo/collection.R",
        "#",
        '# config:  database name is defined in config.yml',
        '# secrets: connectionString is defined in secrets.R',
        '# encrypt: the *.pem file must be placed in the root of this repository',
        "#",
        "# return a list of the instrument_name(s) from MongoDB",
        "# mongo.index()",
        "#",
        "# get collection from MongoDB",
        "# IMPORTANT: both variable name and script filename must match",
        "collection <- mongo(\"collection\")",
        "",
        "# cleaning script code...",
        "",
        "# final df must be named like the R script and appended with _clean",
        "collection_clean <- collection",
        sep = "\n"
      )
    ),
    survey = list(
      path = file.path(path, "clean", "qualtrics", "survey.R"),
      content = paste(
        "#",
        "# clean/qualtrics/survey.R",
        "#",
        "# get survey from Qualtrics database",
        "# config:  surveys are defined in config.yml as key-value pairs",
        "# secrets: baseUrls and apiKeys are defined in secrets.R",
        "#",
        "# return a list of the instrument_name(s) from MongoDB",
        "# qualtrics.index()",
        "#",
        "# get collection from Qualtrics",
        "# IMPORTANT: both variable name and script filename must match",
        "survey <- qualtrics(\"survey\")",
        "",
        "# cleaning script code...",
        "",
        "# IMPORTANT: final df must be appended with _clean",
        "survey_clean <- survey",
        sep = "\n"
      )
    )
  )
  
  nda_templates <- list(
    cde_dsm5crossad01 = list(
      path = file.path(path, "nda", "redcap", "cde_dsm5crossad01.R"),
      content = paste(
        "#",
        "# nda/redcap/cde_dsm5crossad01.R",
        "#",
        "# config:  superkey instrument is defined in config.yml",
        "# secrets: uri and token are defined in secrets.R",
        "#",
        "# return a list of the instrument_name(s) from REDCap",
        "# redcap.index()",
        "#",
        "# get the instrument_name dsm_5 from REDCap",
        "# IMPORTANT: both variable name and script filename must match the NDA data structure alias",
        "cde_dsm5crossad01 <- redcap(\"dsm_5\")",
        "",
        "# nda remediation code...",
        "",
        "# IMPORTANT: final df name must still match the NDA data structure alias",
        "",
        sep = "\n"
      )
    )
  )
  
  # Create example template files only if examples=TRUE
  if (examples) {
    # Create clean templates
    for (template_name in names(clean_templates)) {
      template <- clean_templates[[template_name]]
      if (!file.exists(template$path)) {
        writeLines(template$content, template$path)
        created <- c(created, template$path)
      } else {
        created <- c(created, paste0(template$path, " (skipped, already exists)"))
      }
    }
    
    # Create NDA templates
    for (template_name in names(nda_templates)) {
      template <- nda_templates[[template_name]]
      if (!file.exists(template$path)) {
        writeLines(template$content, template$path)
        created <- c(created, template$path)
      } else {
        created <- c(created, paste0(template$path, " (skipped, already exists)"))
      }
    }
  } else {
    # Skip creating example templates
    message("Skipping creation of example scripts (examples=FALSE)")
  }
  
  # Create R project file if requested
  if (create_project) {
    # Get directory name for default project name
    dir_name <- basename(normalizePath(path))
    
    # Check for existing .Rproj files
    rproj_files <- list.files(path, pattern = "\\.Rproj$", full.names = FALSE)
    
    if (length(rproj_files) == 0) {
      # No project file exists, create one
      project_file <- file.path(path, paste0(dir_name, ".Rproj"))
      
      project_template <- paste(
        "Version: 1.0",
        "",
        "RestoreWorkspace: No",
        "SaveWorkspace: No",
        "AlwaysSaveHistory: Default",
        "",
        "EnableCodeIndexing: Yes",
        "UseSpacesForTab: Yes",
        "NumSpacesForTab: 2",
        "Encoding: UTF-8",
        "",
        "RnwWeave: Sweave",
        "LaTeX: pdfLaTeX",
        "",
        "AutoAppendNewline: Yes",
        "StripTrailingWhitespace: Yes",
        sep = "\n"
      )
      
      writeLines(project_template, project_file)
      created <- c(created, project_file)
      
      message(paste0("Created R project: ", project_file))
    } else {
      message(paste0("R project already exists: ", rproj_files[1]))
    }
  }
  
  # Determine appropriate message based on action
  if (structure_exists && !structure_complete && repair) {
    header_message <- "The wizaRdry crystal ball has repaired your project structure:"
  } else if (!structure_exists) {
    header_message <- "The wizaRdry crystal ball has created your project structure:"
  } else {
    header_message <- "The wizaRdry crystal ball has verified your project structure:"
  }
  
  # Print header message only
  message(header_message)
  
  # Display file tree if requested
  if (show_tree) {
    # Display tree without a header
    display_tree(path)
    
    # Save preference that tree has been shown
    user_prefs$shown_tree <- TRUE
    saveRDS(user_prefs, user_prefs_file)
    
    message("\nNote: This tree view will only be shown once.")
    message("Use scry(show_tree = TRUE) to display it again in the future.")
  }
  
  message("\nYour next enchantments:")
  message("1. Update config.yml with your study-specific configuration")
  message("2. Add your API credentials to secrets.R (DO NOT COMMIT this file)")
  message("3. Create data cleaning scripts with clean()")
  message("4. Create NDA remediation scripts with nda()")
  
  if (create_project && length(rproj_files) == 0) {
    message("5. Open the newly created .Rproj file to work in this project environment")
  }
  
  return(invisible(TRUE))
}

#' Display a file tree structure similar to the Unix tree command
#'
#' @param path The path to display as a tree
#' @return NULL (called for side effects)
#' @keywords internal
display_tree <- function(path) {
  # Function to list all files and directories in a directory
  list_all <- function(dir_path) {
    # Get all files and directories in the directory
    all_items <- list.files(dir_path, all.files = FALSE, include.dirs = TRUE,
                            recursive = FALSE, full.names = TRUE)
    
    # Separate dirs and files
    is_dir <- file.info(all_items)$isdir
    dirs <- all_items[is_dir]
    files <- all_items[!is_dir]
    
    # Check specifically for main.R file and add it if it exists
    main_r_file <- file.path(dir_path, "main.R")
    if (file.exists(main_r_file) && !(main_r_file %in% files)) {
      files <- c(files, main_r_file)
    }
    
    # Sort each group
    dirs <- sort(dirs)
    files <- sort(files)
    
    # Combine with directories first
    return(c(dirs, files))
  }
  
  # Function to process a directory
  process_dir <- function(dir_path, prefix) {
    sub_items <- list_all(dir_path)
    
    # Update counts
    dir_count <<- dir_count + sum(file.info(sub_items)$isdir)
    file_count <<- file_count + sum(!file.info(sub_items)$isdir)
    
    # Process each item
    for (j in seq_along(sub_items)) {
      sub_item <- sub_items[j]
      is_sub_last <- (j == length(sub_items))
      is_sub_dir <- file.info(sub_item)$isdir
      
      # Determine connector
      sub_connector <- if (is_sub_last) "\u2514\u2500\u2500 " else "\u251c\u2500\u2500 "
      sub_prefix <- if (is_sub_last) "    " else "\u2502   "
      
      # Print item
      sub_name <- basename(sub_item)
      if (is_sub_dir) {
        cat(paste0(prefix, sub_connector, sub_name, "\n"))
        # Recurse
        process_dir(sub_item, paste0(prefix, sub_prefix))
      } else {
        cat(paste0(prefix, sub_connector, sub_name, "\n"))
      }
    }
  }
  
  # Initialize stats counters
  dir_count <- 0
  file_count <- 0
  
  # Start with root
  cat("\n")
  cat(".\n")
  
  # Get top-level items
  items <- list_all(path)
  
  # Count directories and files at top level
  dir_count <- sum(file.info(items)$isdir)
  file_count <- sum(!file.info(items)$isdir)
  
  # Process each top-level item
  for (i in seq_along(items)) {
    item <- items[i]
    is_last <- (i == length(items))
    is_dir <- file.info(item)$isdir
    
    # Determine connector
    connector <- if (is_last) "\u2514\u2500\u2500 " else "\u251c\u2500\u2500 "
    
    # Print item
    item_name <- basename(item)
    if (is_dir) {
      cat(paste0(connector, item_name, "\n"))
      
      # Process items in this directory
      process_dir(item, if (is_last) "    " else "\u2502   ")
    } else {
      cat(paste0(connector, item_name, "\n"))
    }
  }
  
  # Print stats
  cat("\n")
  cat(paste0(dir_count, " directories, ", file_count, " files\n"))
  
  return(invisible(NULL))
}


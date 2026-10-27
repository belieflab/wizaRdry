#' Git, GitHub, and clean/ library helpers for scry()
#'
#' Ports the project bootstrap previously done by the createAnalysis init.sh script:
#' git initialisation, a clean/ script library added as a git submodule, a pre-commit
#' hook, a README, and (optionally) a private GitHub repository created with the gh CLI.
#'
#' @name scryGit
#' @keywords internal
NULL

# Source folders clean() looks in (see clean() in R/dataRequest.R)
.scry_sources <- c("csv", "mongo", "qualtrics", "redcap", "oracle", "sql")

#' Run an external command and capture its status and output
#' @keywords internal
#' @noRd
.scry_run <- function(cmd, args = character()) {
  out <- tryCatch(
    suppressWarnings(system2(cmd, args, stdout = TRUE, stderr = TRUE)),
    error = function(e) structure(conditionMessage(e), status = 127L)
  )
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else status, output = as.character(out))
}

#' Run git against the repository at path
#' @keywords internal
#' @noRd
.scry_git <- function(path, ...) {
  .scry_run("git", c("-C", shQuote(normalizePath(path, mustWork = FALSE)), ...))
}

#' Stop with a command's output if it failed
#' @keywords internal
#' @noRd
.scry_check <- function(res, what) {
  if (res$status != 0) {
    stop(what, " failed:\n", paste(res$output, collapse = "\n"), call. = FALSE)
  }
  invisible(res)
}

#' Ask a y/n question; non-interactive sessions answer with skip_prompt
#' @keywords internal
#' @noRd
.scry_confirm <- function(question, skip_prompt = TRUE) {
  if (!interactive()) {
    return(isTRUE(skip_prompt))
  }
  response <- readline(prompt = paste0(question, " y/n "))
  while (!tolower(response) %in% c("y", "n")) {
    response <- readline(prompt = "Please enter either y or n: ")
  }
  tolower(response) == "y"
}

# ---------------------------------------------------------------------------
# Preflight checks (createAnalysis parity)
# ---------------------------------------------------------------------------

#' Require a command-line tool to be installed
#' @keywords internal
#' @noRd
.scry_require_cli <- function(cmd) {
  if (!nzchar(Sys.which(cmd))) {
    hint <- switch(cmd,
      gh = paste0(
        "Install the GitHub CLI (macOS: `brew install gh`; Windows/Linux: https://cli.github.com/), ",
        "then run `gh auth login` (choose GitHub.com, SSH, and Login with Browser)."
      ),
      git = "Install git from https://git-scm.com/downloads.",
      ""
    )
    stop(cmd, " could not be found. ", hint, call. = FALSE)
  }
  invisible(TRUE)
}

#' Check that the gh CLI is authenticated
#' @keywords internal
#' @noRd
.scry_gh_authenticated <- function() {
  if (.scry_run("gh", c("auth", "status"))$status != 0) {
    stop("You are not authenticated with the GitHub CLI. Please run `gh auth login` to authenticate.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Check that git user.name and user.email are configured
#' @keywords internal
#' @noRd
.scry_git_identity <- function(path) {
  name <- .scry_git(path, "config", "user.name")
  email <- .scry_git(path, "config", "user.email")
  if (name$status != 0 || email$status != 0 ||
    !nzchar(paste(name$output, collapse = "")) || !nzchar(paste(email$output, collapse = ""))) {
    stop(
      "git does not know who you are. Run these two commands, substituting your GitHub email and name:\n",
      "  git config --global user.email \"you@example.com\"\n",
      "  git config --global user.name \"Your Name\"",
      call. = FALSE
    )
  }
  invisible(list(name = name$output[1], email = email$output[1]))
}

#' Check whether a name is camelCase (lowercase start, at least one uppercase letter)
#' @keywords internal
#' @noRd
.scry_check_camel_case <- function(name) {
  grepl("^[a-z][a-zA-Z0-9]*$", name) && grepl("[A-Z]", name)
}

#' Check whether a name is snake_case (digits allowed, e.g. phq9)
#' @keywords internal
#' @noRd
.scry_check_snake_case <- function(name) {
  grepl("^[a-z][a-z0-9]*(_[a-z0-9]+)*$", name)
}

#' Check that a GitHub user or organization exists
#' @keywords internal
#' @noRd
.scry_gh_account_exists <- function(account) {
  .scry_run("gh", c("api", shQuote(paste0("users/", account))))$status == 0 ||
    .scry_run("gh", c("api", shQuote(paste0("orgs/", account))))$status == 0
}

#' Check that a GitHub repository exists
#' @keywords internal
#' @noRd
.scry_gh_repo_exists <- function(account, repo_name) {
  .scry_run("gh", c("repo", "view", shQuote(paste0(account, "/", repo_name))))$status == 0
}

# ---------------------------------------------------------------------------
# Submodule inspection
# ---------------------------------------------------------------------------

#' Parse .gitmodules into a data frame of name, path, url, branch
#' @keywords internal
#' @noRd
.scry_gitmodules <- function(path) {
  empty <- data.frame(name = character(), path = character(), url = character(),
                      branch = character(), stringsAsFactors = FALSE)
  gitmodules_file <- file.path(path, ".gitmodules")
  if (!file.exists(gitmodules_file)) {
    return(empty)
  }

  modules <- list()
  current <- NULL
  for (line in trimws(readLines(gitmodules_file, warn = FALSE))) {
    header <- regmatches(line, regexec('^\\[submodule "(.*)"\\]$', line))[[1]]
    if (length(header) == 2) {
      current <- header[2]
      modules[[current]] <- list(name = current, path = NA_character_, url = NA_character_,
                                 branch = NA_character_)
    } else if (!is.null(current)) {
      kv <- regmatches(line, regexec("^([A-Za-z]+)\\s*=\\s*(.*)$", line))[[1]]
      if (length(kv) == 3 && kv[2] %in% c("path", "url", "branch")) {
        modules[[current]][[kv[2]]] <- kv[3]
      }
    }
  }
  if (length(modules) == 0) {
    return(empty)
  }
  do.call(rbind, lapply(modules, as.data.frame, stringsAsFactors = FALSE))
}

#' Return the clean/ submodule entry, or NULL if clean/ is not a submodule
#' @keywords internal
#' @noRd
.scry_clean_submodule <- function(path) {
  modules <- .scry_gitmodules(path)
  entry <- modules[!is.na(modules$path) & modules$path == "clean", , drop = FALSE]
  if (nrow(entry) == 0) NULL else as.list(entry[1, ])
}

#' Compare two git URLs ignoring a trailing .git or slash
#' @keywords internal
#' @noRd
.scry_same_url <- function(a, b) {
  normalize <- function(u) sub("(\\.git)?/?$", "", trimws(u))
  identical(normalize(a), normalize(b))
}

# ---------------------------------------------------------------------------
# Script library checks
# ---------------------------------------------------------------------------

#' Find script names that appear in more than one source folder of a script root
#' @return A data frame of script and source, or NULL if there are no duplicates.
#' @keywords internal
#' @noRd
.scry_find_duplicates <- function(root) {
  if (!dir.exists(root)) {
    return(NULL)
  }
  scripts <- do.call(rbind, lapply(.scry_sources, function(src) {
    files <- list.files(file.path(root, src), pattern = "\\.[Rr]$")
    if (length(files) == 0) return(NULL)
    data.frame(script = tools::file_path_sans_ext(files), source = src, stringsAsFactors = FALSE)
  }))
  if (is.null(scripts)) {
    return(NULL)
  }
  dup_names <- unique(scripts$script[duplicated(scripts$script)])
  if (length(dup_names) == 0) {
    return(NULL)
  }
  scripts[scripts$script %in% dup_names, , drop = FALSE]
}

#' Check for duplicate script names across source folders
#'
#' clean() and nda() look up a script by its file name across all source folders
#' (csv, mongo, qualtrics, redcap, oracle, sql), so the same name in two folders is ambiguous.
#'
#' @param path Project root containing clean/ and nda/.
#' @param quiet Logical. If FALSE, report duplicates with message().
#' @return A named list (clean, nda) of data frames listing each duplicate name and its
#'   folders, or NULL if there are no duplicates.
#' @keywords internal
#' @noRd
check_duplicate_scripts <- function(path = ".", quiet = FALSE) {
  dups <- Filter(Negate(is.null), list(
    clean = .scry_find_duplicates(file.path(path, "clean")),
    nda = .scry_find_duplicates(file.path(path, "nda"))
  ))
  if (length(dups) == 0) {
    return(NULL)
  }

  if (!quiet) {
    for (folder in names(dups)) {
      by_name <- split(dups[[folder]]$source, dups[[folder]]$script)
      for (script in names(by_name)) {
        message(sprintf("Duplicate script '%s' in %s/: %s", script, folder,
                        paste(by_name[[script]], collapse = ", ")))
      }
    }
  }
  dups
}

#' Validate a cleaning-script library
#'
#' Checks that a cleaning-script library (typically the \code{clean/} git submodule added by
#' \code{scry(clean_repo = ...)}) is laid out the way \code{clean()} expects.
#'
#' Errors (the library is not usable):
#' \itemize{
#'   \item no .R scripts at all
#'   \item \code{secrets.R} or \code{*.pem} files committed to the library
#'   \item the same script name in more than one source folder
#' }
#' Warnings (the library works, but does not follow conventions):
#' \itemize{
#'   \item missing source folders (csv, mongo, qualtrics, redcap, oracle, sql)
#'   \item scripts outside the source folders, which \code{clean()} cannot find
#'   \item script names that are not snake_case
#'   \item scripts that never assign a \code{<name>_clean} data frame
#' }
#'
#' @param path Path to the cleaning-script library. Defaults to \code{"clean"}.
#' @param quiet Logical. If TRUE, do not print the findings.
#'
#' @return Invisibly, a list with \code{valid} (logical), \code{errors}, and \code{warnings}
#'   (character vectors).
#'
#' @examples
#' \dontrun{
#' validate_clean_library()
#' validate_clean_library("path/to/clean")
#' }
#'
#' @export
validate_clean_library <- function(path = "clean", quiet = FALSE) {
  errors <- character()
  warnings <- character()

  if (!dir.exists(path)) {
    errors <- c(errors, sprintf("%s does not exist.", path))
    return(.scry_report_library(path, errors, warnings, quiet))
  }

  # Prefer tracked files so untracked local clutter does not count against the library
  tracked <- if (file.exists(file.path(path, ".git"))) .scry_git(path, "ls-files") else NULL
  all_files <- if (!is.null(tracked) && tracked$status == 0) {
    tracked$output
  } else {
    grep("^\\.git/", list.files(path, recursive = TRUE, all.files = TRUE), value = TRUE, invert = TRUE)
  }

  # Secrets must never be part of a shared library
  secret_files <- grep("(^|/)secrets\\.R$|\\.pem$", all_files, value = TRUE)
  if (length(secret_files) > 0) {
    errors <- c(errors, paste0("Secret files committed to the library: ",
                               paste(secret_files, collapse = ", ")))
  }

  r_files <- grep("\\.[Rr]$", all_files, value = TRUE)
  if (length(r_files) == 0) {
    errors <- c(errors, "No .R cleaning scripts found.")
  }

  # Source folder layout expected by clean()
  missing_sources <- .scry_sources[!dir.exists(file.path(path, .scry_sources))]
  if (length(missing_sources) > 0) {
    warnings <- c(warnings, paste0("Missing source folders: ", paste(missing_sources, collapse = ", "),
                                   " (clean() will find no scripts for these sources)"))
  }

  in_source <- grepl(paste0("^(", paste(.scry_sources, collapse = "|"), ")/[^/]+\\.[Rr]$"), r_files)
  if (any(!in_source)) {
    warnings <- c(warnings, paste0("Scripts outside the source folders, not reachable by clean(): ",
                                   paste(r_files[!in_source], collapse = ", ")))
  }

  # Duplicates across source folders make clean("name") ambiguous
  dups <- .scry_find_duplicates(path)
  if (!is.null(dups)) {
    by_name <- split(dups$source, dups$script)
    errors <- c(errors, paste0(
      "Duplicate script names across source folders: ",
      paste(sprintf("%s (%s)", names(by_name), vapply(by_name, paste, "", collapse = ", ")),
            collapse = "; ")
    ))
  }

  # Naming conventions
  script_names <- tools::file_path_sans_ext(basename(r_files[in_source]))
  bad_case <- script_names[!vapply(script_names, .scry_check_snake_case, logical(1))]
  if (length(bad_case) > 0) {
    warnings <- c(warnings, paste0("Script names not in snake_case: ", paste(bad_case, collapse = ", ")))
  }

  no_clean_df <- character()
  for (script in r_files[in_source]) {
    name <- tools::file_path_sans_ext(basename(script))
    code <- tryCatch(readLines(file.path(path, script), warn = FALSE), error = function(e) character())
    assigns <- grepl(sprintf("(^|[^A-Za-z0-9_.])%s_clean\\s*(<-|=)|assign\\(\\s*[\"']%s_clean[\"']",
                             name, name), code)
    if (!any(assigns)) {
      no_clean_df <- c(no_clean_df, script)
    }
  }
  if (length(no_clean_df) > 0) {
    warnings <- c(warnings, paste0("Scripts that never assign a <name>_clean data frame: ",
                                   paste(no_clean_df, collapse = ", ")))
  }

  .scry_report_library(path, errors, warnings, quiet)
}

#' Print and return validate_clean_library() results
#' @keywords internal
#' @noRd
.scry_report_library <- function(path, errors, warnings, quiet) {
  if (!quiet) {
    for (e in errors) message("\u2717 ", e)
    for (w in warnings) message("! ", w)
    if (length(errors) == 0) {
      message("\u2713 Cleaning-script library at ", path, " passed validation",
              if (length(warnings) > 0) sprintf(" with %d warning(s)", length(warnings)) else "", ".")
    }
  }
  invisible(list(valid = length(errors) == 0, errors = errors, warnings = warnings))
}

# ---------------------------------------------------------------------------
# Setup steps
# ---------------------------------------------------------------------------

#' Run every check that can fail before scry() writes anything
#'
#' @return A list with repo_name and pem_file (NULL if no .pem should be copied).
#' @keywords internal
#' @noRd
.scry_git_preflight <- function(path, clean_repo, clean_branch, github, github_account,
                                pem, replace_clean) {
  .scry_require_cli("git")
  .scry_git_identity(path)

  repo_name <- basename(normalizePath(path, mustWork = FALSE))
  if (!.scry_check_camel_case(repo_name)) {
    msg <- sprintf(
      "Project folder '%s' is not camelCase (e.g. myNewAnalysis, starting with a lowercase letter, no underscores).",
      repo_name
    )
    if (github) stop(msg, " Rename the folder before creating a GitHub repository.", call. = FALSE)
    warning(msg, call. = FALSE)
  }

  if (github) {
    .scry_require_cli("gh")
    .scry_gh_authenticated()
    message("Checking if ", github_account, " exists on GitHub...")
    if (!.scry_gh_account_exists(github_account)) {
      stop("\u2717 ", github_account, " not found on GitHub. Please supply a valid github_account.",
           call. = FALSE)
    }
    message("\u2713 ", github_account, " found on GitHub.")
    if (.scry_gh_repo_exists(github_account, repo_name)) {
      stop("A GitHub repository named ", github_account, "/", repo_name, " already exists.", call. = FALSE)
    }
    if (file.exists(file.path(path, ".git")) &&
        .scry_git(path, "remote", "get-url", "origin")$status == 0) {
      stop("This repository already has an 'origin' remote; github = TRUE would replace it.", call. = FALSE)
    }
  }

  # .pem: an explicit path must exist; the default location is optional
  pem_file <- NULL
  if (!is.null(pem)) {
    pem_file <- path.expand(pem)
    if (!file.exists(pem_file)) stop("The .pem file ", pem, " does not exist.", call. = FALSE)
  } else {
    default_pem <- path.expand("~/Downloads/rds-combined-ca-bundle.pem")
    if (file.exists(default_pem)) pem_file <- default_pem
  }

  if (!is.null(clean_repo)) {
    existing <- .scry_clean_submodule(path)
    if (!is.null(existing)) {
      if (!.scry_same_url(existing$url, clean_repo)) {
        stop("clean/ is already a submodule of ", existing$url, ", not ", clean_repo, ".", call. = FALSE)
      }
    } else {
      # The library must be reachable (and the branch must exist) before anything is written
      ls_args <- c("ls-remote", if (!is.null(clean_branch)) "--heads", shQuote(clean_repo),
                   if (!is.null(clean_branch)) shQuote(clean_branch))
      remote <- .scry_run("git", ls_args)
      .scry_check(remote, paste0("Accessing ", clean_repo))
      if (!is.null(clean_branch) && length(remote$output) == 0) {
        stop("Branch '", clean_branch, "' not found in ", clean_repo, ".", call. = FALSE)
      }

      clean_dir <- file.path(path, "clean")
      scripts <- if (dir.exists(clean_dir)) list.files(clean_dir, pattern = "\\.[Rr]$", recursive = TRUE) else character()
      if (length(scripts) > 0 && !replace_clean) {
        stop(
          "clean/ already contains ", length(scripts), " script(s):\n  ",
          paste(file.path("clean", scripts), collapse = "\n  "),
          "\nUse replace_clean = TRUE to move them to a clean.bak-<timestamp>/ folder ",
          "and replace clean/ with ", clean_repo, ".",
          call. = FALSE
        )
      }
    }
  }

  list(repo_name = repo_name, pem_file = pem_file)
}

#' Initialise a git repository on the main branch if path is not one already
#' @return TRUE if a new repository was created.
#' @keywords internal
#' @noRd
.scry_git_init <- function(path) {
  if (file.exists(file.path(path, ".git"))) {
    return(FALSE)
  }
  message("Initializing new Git repository...")
  .scry_check(.scry_git(path, "init", "-q"), "git init")
  # Works on every git version, unlike `git init -b main`
  .scry_check(.scry_git(path, "symbolic-ref", "HEAD", "refs/heads/main"), "Setting branch to main")
  TRUE
}

#' Replace clean/ with a git submodule and validate it, rolling back on failure
#' @return The path of the backup folder, or NULL if none was made.
#' @keywords internal
#' @noRd
.scry_add_clean_submodule <- function(path, clean_repo, clean_branch) {
  if (!is.null(.scry_clean_submodule(path))) {
    message("clean/ is already a submodule of ", clean_repo, "; skipping.")
    return(invisible(NULL))
  }

  clean_dir <- file.path(path, "clean")
  backup_dir <- NULL
  if (dir.exists(clean_dir)) {
    tracked <- .scry_git(path, "ls-files", "--", "clean")$output
    if (length(tracked) > 0) {
      .scry_check(.scry_git(path, "rm", "-r", "-q", "--cached", "--", "clean"), "Unstaging clean/")
    }
    if (length(list.files(clean_dir, pattern = "\\.[Rr]$", recursive = TRUE)) > 0) {
      backup_dir <- file.path(path, paste0("clean.bak-", format(Sys.time(), "%Y%m%d-%H%M%S")))
      if (!file.rename(clean_dir, backup_dir)) stop("Could not move clean/ to ", backup_dir, call. = FALSE)
      message("Moved existing clean/ scripts to ", basename(backup_dir), "/")
    } else {
      unlink(clean_dir, recursive = TRUE)
    }
  }

  restore <- function() {
    if (!is.null(backup_dir)) {
      file.rename(backup_dir, clean_dir)
    } else {
      for (dir in file.path(clean_dir, .scry_sources)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        file.create(file.path(dir, ".gitkeep"))
      }
    }
  }

  message("Adding ", clean_repo, " as the clean/ submodule...")
  # git >= 2.38 refuses file-protocol submodules unless explicitly allowed
  is_local <- dir.exists(path.expand(clean_repo))
  added <- .scry_git(path,
    if (is_local) c("-c", "protocol.file.allow=always"),
    "submodule", "add", if (!is.null(clean_branch)) c("-b", shQuote(clean_branch)),
    shQuote(clean_repo), "clean"
  )
  if (added$status != 0) {
    restore()
    .scry_check(added, paste0("Adding submodule ", clean_repo))
  }

  result <- validate_clean_library(clean_dir)
  if (!result$valid) {
    .scry_git(path, "submodule", "deinit", "-f", "-q", "--", "clean")
    .scry_git(path, "rm", "-f", "-q", "--", "clean")
    unlink(file.path(path, ".git", "modules", "clean"), recursive = TRUE)
    unlink(clean_dir, recursive = TRUE)
    if (file.exists(file.path(path, ".gitmodules")) && nrow(.scry_gitmodules(path)) == 0) {
      .scry_git(path, "rm", "-f", "-q", "--", ".gitmodules")
      unlink(file.path(path, ".gitmodules"))
    }
    restore()
    stop(clean_repo, " does not conform to the wizaRdry cleaning-script library layout; submodule removed.",
         call. = FALSE)
  }

  invisible(backup_dir)
}

#' Copy the .pem file into the project root (it is gitignored and blocked by the hook)
#' @keywords internal
#' @noRd
.scry_copy_pem <- function(path, pem_file) {
  if (is.null(pem_file)) {
    message("No .pem file found in ~/Downloads; add it to the project root later if you use MongoDB.")
    return(invisible(NULL))
  }
  target <- file.path(path, basename(pem_file))
  if (!file.exists(target)) {
    file.copy(pem_file, target)
    message("Copied ", basename(pem_file), " to the project root.")
  }
  invisible(target)
}

#' Write a README with clone-with-submodules instructions if none exists
#' @keywords internal
#' @noRd
.scry_write_readme <- function(path, repo_name, github_account, has_clean_submodule) {
  readme_file <- file.path(path, "README.md")
  if (file.exists(readme_file)) {
    return(invisible(FALSE))
  }

  clone <- sprintf(
    "git clone --recurse-submodules -j4 git@github.com:%s/%s.git && cd %s && git config core.hooksPath .githooks",
    github_account, repo_name, repo_name
  )

  writeLines(c(
    paste0("# ", repo_name),
    "",
    paste0("Welcome to the **", repo_name, "** repository!"),
    "",
    "## \u2705 Getting Started",
    "",
    "### Clone Repository with Submodules:",
    "",
    "```",
    clone,
    "```",
    "",
    "The last command enables the Git hook that tracks collaborators and prevents secrets from being committed.",
    "",
    if (has_clean_submodule) c(
      "# \U0001F9E9 Submodules",
      "",
      "Data cleaning scripts live in the `clean/` submodule. Pull updates for the repository and its submodules with:",
      "",
      "```r",
      "wizaRdry::scry.sync()",
      "```",
      "",
      "---",
      ""
    ),
    "# \U0001F510 Secrets",
    "Add `secrets.R` and `rds-combined-ca-bundle.pem` to the repository root.",
    "",
    "**\u26a0\ufe0f Do not add additional secrets or modify .gitignore.**",
    "",
    "---",
    "",
    "# \U0001F4E1 APIs",
    "The wizaRdry package includes functions for creating data frames from REDCap, Qualtrics, MongoDB, SQL and Oracle.",
    "",
    "- `clean(\"bprs\")` requests cleaned data from any API.",
    "- `meld(bprs_clean, demo_clean)` merges data from any API.",
    "- `to.csv(bprs_clean)` exports a data frame to `./tmp`.",
    "",
    "---",
    "",
    "# \U0001F9FC Data Cleaning Scripts",
    "Data cleaning scripts are located in `./clean` and organized by source.",
    "",
    "- Use **lower snake case** for variable names.",
    "- Keep all raw item-level responses.",
    "- Always name cleaned data frames using the `_clean` suffix.",
    "",
    "## \U0001F4DD NDA Required Variables",
    "These variables *must* be retained in all data:",
    "```",
    "src_subject_id, subjectkey, phenotype, visit, site, sex",
    "```"
  ), readme_file)
  message("Created README.md")
  invisible(TRUE)
}

#' Install the pre-commit hook into .githooks/ and point git at it
#' @keywords internal
#' @noRd
.scry_install_hooks <- function(path) {
  hooks_dir <- file.path(path, ".githooks")
  dir.create(hooks_dir, showWarnings = FALSE)
  hook_file <- file.path(hooks_dir, "pre-commit")

  writeLines(c(
    "#!/usr/bin/env bash",
    "# wizaRdry pre-commit hook (installed by scry())",
    "# 1. Blocks secrets.R and *.pem files from being committed",
    "# 2. Prepends an auto-populated collaborator header to staged .R files",
    "",
    "marker=\"###\"",
    "",
    "# Check for forbidden files first so nothing is rewritten on a rejected commit",
    "forbidden=$(git diff --cached --name-only --diff-filter=ACMR | grep -E '(^|/)secrets\\.R$|\\.pem$')",
    "if [ -n \"$forbidden\" ]; then",
    "    echo \"Error: Forbidden file(s) staged:\"",
    "    echo \"$forbidden\"",
    "    echo \"Aborting commit.\"",
    "    exit 1",
    "fi",
    "",
    "git diff --cached --name-only --diff-filter=ACMR | grep -E '\\.[rR]$' | while IFS= read -r file; do",
    "    temp_file=$(mktemp)",
    "    {",
    "        printf '%s Collaborators (auto-populated):\\n%s\\n' \"$marker\" \"$marker\"",
    "        collaborators=$(git log --format='%aN (%aE)' -- \"$file\" | sort -u)",
    "        if [ -z \"$collaborators\" ]; then",
    "            collaborators=\"$(git config user.name) ($(git config user.email))\"",
    "        fi",
    "        printf '%s\\n' \"$collaborators\" | sed \"s/^/$marker /\"",
    "        # Drop only the existing header block (leading lines that start with the marker)",
    "        awk -v m=\"$marker\" 'body || index($0, m) != 1 { body = 1; print }' \"$file\"",
    "    } > \"$temp_file\"",
    "    mv \"$temp_file\" \"$file\"",
    "    git add \"$file\"",
    "done",
    "",
    "exit 0"
  ), hook_file)
  Sys.chmod(hook_file, mode = "0755")

  # core.hooksPath works the same on every platform (no symlinks needed on Windows)
  .scry_check(.scry_git(path, "config", "core.hooksPath", ".githooks"), "Installing Git hooks")
  message("Installed Git pre-commit hook (.githooks/pre-commit)")
  invisible(hook_file)
}

#' Commit what scry() set up
#'
#' A new repository gets everything (secrets are gitignored and blocked by the hook).
#' An existing repository only gets the paths scry() touched, so unrelated work is left alone.
#' @keywords internal
#' @noRd
.scry_commit <- function(path, fresh_repo) {
  if (fresh_repo) {
    .scry_check(.scry_git(path, "add", "-A"), "Staging files")
    .scry_check(.scry_git(path, "commit", "-q", "-m", shQuote("initialized analysis")), "Initial commit")
    message("Committed: initialized analysis")
    return(invisible(TRUE))
  }

  candidates <- c(".gitmodules", "clean", ".githooks", "README.md", ".gitignore",
                  list.files(path, pattern = "\\.Rproj$"))
  paths <- candidates[file.exists(file.path(path, candidates))]
  changed <- .scry_git(path, "status", "--porcelain", "--", paths)$output
  if (length(changed) == 0) {
    return(invisible(FALSE))
  }

  .scry_check(.scry_git(path, "add", "--", paths), "Staging files")
  .scry_check(.scry_git(path, "commit", "-q", "-m", shQuote("wizaRdry: add clean/ library and project setup"),
                        "--", paths), "Commit")
  message("Committed: wizaRdry: add clean/ library and project setup")
  invisible(TRUE)
}

#' Create the GitHub repository with gh and push, after confirmation
#' @keywords internal
#' @noRd
.scry_github_publish <- function(path, repo_name, github_account, private, skip_prompt) {
  slug <- paste0(github_account, "/", repo_name)
  visibility <- if (private) "private" else "public"
  if (!.scry_confirm(sprintf("Create %s GitHub repository %s and push to it?", visibility, slug), skip_prompt)) {
    message("Skipped GitHub repository creation. Nothing was pushed.")
    return(invisible(FALSE))
  }

  message("Creating repository on GitHub...")
  created <- .scry_run("gh", c(
    "repo", "create", shQuote(slug), paste0("--", visibility),
    "--source", shQuote(normalizePath(path)), "--remote", "origin", "--push"
  ))
  if (created$status != 0) {
    stop(
      "Failed to create or push to ", slug, ":\n", paste(created$output, collapse = "\n"),
      "\nOnce the problem is fixed, push with `git push -u origin main`.",
      call. = FALSE
    )
  }
  message("Changes successfully pushed to GitHub repository ", slug, ".")
  invisible(TRUE)
}

#' Run every git/GitHub step of scry() after its files are written
#' @keywords internal
#' @noRd
.scry_git_setup <- function(path, preflight, clean_repo, clean_branch, github, github_account,
                            private, hooks, skip_prompt) {
  fresh_repo <- .scry_git_init(path)

  backup_dir <- NULL
  if (!is.null(clean_repo)) {
    backup_dir <- .scry_add_clean_submodule(path, clean_repo, clean_branch)
  }

  .scry_copy_pem(path, preflight$pem_file)
  .scry_write_readme(path, preflight$repo_name, github_account,
                     has_clean_submodule = !is.null(.scry_clean_submodule(path)))
  if (hooks) {
    .scry_install_hooks(path)
  }
  .scry_commit(path, fresh_repo)

  if (github) {
    .scry_github_publish(path, preflight$repo_name, github_account, private, skip_prompt)
  }

  invisible(list(fresh_repo = fresh_repo, backup_dir = backup_dir))
}

# ---------------------------------------------------------------------------
# Sync
# ---------------------------------------------------------------------------

#' Pull updates for a wizaRdry project and its submodules
#'
#' Pulls the project repository, then updates every submodule (such as a \code{clean/}
#' script library added by \code{scry(clean_repo = ...)}) to the latest commit of its tracked
#' branch. Also makes sure the project's Git hooks are enabled for this clone.
#'
#' @param path Path to the project root. Defaults to the current working directory.
#'
#' @return Invisible TRUE if every update succeeded, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' scry.sync()
#' }
#'
#' @export
scry.sync <- function(path = ".") {
  .scry_require_cli("git")
  if (!file.exists(file.path(path, ".git"))) {
    stop(normalizePath(path, mustWork = FALSE), " is not the root of a git repository.", call. = FALSE)
  }

  ok <- TRUE
  repo_name <- basename(normalizePath(path))

  message("Updating parent repository: ", repo_name)
  pulled <- .scry_git(path, "pull")
  if (pulled$status != 0) {
    message("Error updating the parent repository:\n", paste(pulled$output, collapse = "\n"))
    ok <- FALSE
  }

  modules <- .scry_gitmodules(path)
  if (nrow(modules) == 0) {
    message(".gitmodules file not found or empty.")
  }
  for (submodule_path in modules$path) {
    message("Updating submodule: ", submodule_path)
    updated <- .scry_git(path, "submodule", "update", "--init", "--remote", "--merge", "--", shQuote(submodule_path))
    if (updated$status != 0) {
      message("Error updating submodule at path: ", submodule_path, "\n", paste(updated$output, collapse = "\n"))
      ok <- FALSE
    }
  }

  if (dir.exists(file.path(path, ".githooks")) &&
      .scry_git(path, "config", "core.hooksPath")$status != 0) {
    .scry_git(path, "config", "core.hooksPath", ".githooks")
    message("Enabled Git hooks (.githooks/)")
  }

  if (ok) {
    message("Update complete.")
    changed <- .scry_git(path, "status", "--porcelain", "--", modules$path)$output
    if (length(changed) > 0) {
      message("Submodules moved to newer commits; commit the change to record it for collaborators.")
    }
  } else {
    message("Update finished with errors.")
  }
  invisible(ok)
}

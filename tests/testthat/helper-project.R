# Shared helpers for pathway tests. Each test bootstraps a disposable wizaRdry
# project in a tempdir, writes a fixture-backed script into clean/{api}/ or
# nda/{api}/, and runs the workflow non-interactively.

# Fixture compliant with every testSuite check:
# - super required fields present (ndaRequiredVariablesExist needs
#   src_subject_id, phenotype, site, subjectkey, sex, interview_date, interview_age)
# - interview_age within checkInterviewAge's 144-840 month range
# - non-NDA columns prefixed with the measure name (checkColumnPrefix);
#   visit/arm/state are on the excluded nda_required_variables list
make_fixture_df <- function(measure, n = 3, qualtrics = FALSE) {
  df <- data.frame(
    src_subject_id = sprintf("SUB%03d", seq_len(n)),
    subjectkey     = sprintf("NDAR_INV%08d", seq_len(n)),
    interview_date = rep("06/01/2024", n),
    interview_age  = rep(300L, n),
    sex            = rep_len(c("F", "M"), n),
    phenotype      = rep("CHR", n),
    site           = rep("Yale", n),
    visit          = rep("baseline", n),
    arm            = rep(1L, n),
    state          = rep("complete", n),
    stringsAsFactors = FALSE
  )
  df[[paste0(measure, "_score")]] <- seq_len(n)
  if (qualtrics) df$ResponseId <- sprintf("R_%05d", seq_len(n))
  df
}

# NDA-pathway fixture: only columns defined in the mocked structure, so
# process_unexpected_fields() never needs interactive field mapping.
make_nda_fixture_df <- function(measure, n = 3) {
  make_fixture_df(measure, n)[, c(
    "src_subject_id", "subjectkey", "interview_date",
    "interview_age", "sex", paste0(measure, "_score")
  )]
}

# Bootstrap a temp project and cd into it (clean()/nda() use relative paths).
# Pre-seeding .wizaRdry_prefs is mandatory: prompt guards are
# `if (!skip_prompt | !user_prefs$auto_*)`, so without auto_* = TRUE a
# readline() fires even with skip_prompt = TRUE and spins forever headless.
local_wizardry_project <- function(env = parent.frame()) {
  proj <- withr::local_tempdir(.local_envir = env)
  saveRDS(
    list(shown_tree = TRUE, auto_create = TRUE, auto_clean = TRUE,
         auto_nda = TRUE, auto_nda_template = TRUE, auto_csv = TRUE,
         auto_rds = TRUE, auto_sav = TRUE),
    file.path(proj, ".wizaRdry_prefs")
  )
  suppressMessages(scry("testproj", path = proj, skip_prompt = TRUE))
  withr::local_dir(proj, .local_envir = env)
  proj
}

# Write a script that loads the fixture instead of calling a remote API.
# Dispatch is purely directory-based, so this exercises the full pathway
# for the given interface without network access.
write_pathway_script <- function(proj, pathway = c("clean", "nda"), api, measure, df) {
  pathway <- match.arg(pathway)
  rds_name <- paste0(measure, "_fixture.rds")
  saveRDS(df, file.path(proj, rds_name))
  lines <- sprintf('%s <- readRDS("%s")', measure, rds_name)
  if (pathway == "clean") {
    lines <- c(lines, sprintf("%s_clean <- %s", measure, measure))
  }
  writeLines(lines, file.path(proj, pathway, api, paste0(measure, ".R")))
}

# Scripts are sourced into .GlobalEnv, and nda() additionally syncs data
# frames into the package environment; both must be cleaned between tests.
local_globalenv_cleanup <- function(measures, env = parent.frame()) {
  withr::defer({
    objs <- intersect(
      c(measures, paste0(measures, "_clean")),
      ls(.GlobalEnv, all.names = TRUE)
    )
    if (length(objs) > 0) rm(list = objs, envir = .GlobalEnv)
    wiz_env <- tryCatch(.pkg_env$.wizaRdry_env, error = function(e) NULL)
    if (is.environment(wiz_env)) {
      rm(list = ls(wiz_env, all.names = TRUE), envir = wiz_env)
    }
  }, envir = env)
}

skip_if_no_nda_mock <- function() {
  testthat::skip_if_not_installed("webfakes")
  # Setup files load into a child of the helper env, so look up the mock from
  # the caller's environment (test code can see it; this closure cannot).
  testthat::skip_if_not(
    exists("nda_mock_url", envir = parent.frame()),
    "NDA mock server not running"
  )
}

# Live layer gating: WIZARDRY_LIVE_PROJECT points at a real, fully configured
# wizaRdry project (config.yml + secrets.R). Unset => all live tests skip.
skip_if_no_live_project <- function() {
  dir <- Sys.getenv("WIZARDRY_LIVE_PROJECT", "")
  testthat::skip_if(
    !nzchar(dir) || !dir.exists(dir),
    "Set WIZARDRY_LIVE_PROJECT to a configured wizaRdry project to run live tests"
  )
}

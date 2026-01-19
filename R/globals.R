# globals.R
# NOTE: .wizaRdry_env is intentionally assigned to globalenv() for cross-function
# data sharing in the NDA validation workflow. This pattern is used throughout
# the codebase for maintaining dataframes across validation stages and is necessary
# for backward compatibility with legacy code (ndaRequest.R, ndaValidator.R).
# The assignment is guarded to prevent repeated creation of the environment.

utils::globalVariables(c(
  # Existing variables
  "connectionString", "apiKeys", "baseUrls", "config",
  "uri", "token", "dob", "surveyIds", "pb", "value",
  "nda_base_url",
  # Additional variables from the NOTE
  "duplicates", "nda_base_url", "src_institution_id",
  "subject_dob", "mongo_conn", "checkKeys", "createExtract",
  "View", "write.csv", "write.table", "ymd", "mdy",
  ".wizaRdry_env",  # Environment for NDA validation workflow
  "SUPER_REQUIRED_FIELDS"  # NDA super required fields constant
))

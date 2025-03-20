# globals.R
utils::globalVariables(c(
  # Existing variables
  "connectionString", "apiKeys", "baseUrls", "config",
  "uri", "token", "dob", "surveyIds", "pb", "value",
  "api_base_url",

  # Additional variables from the NOTE
  "duplicates", "nda_base_url", "src_institution_id",
  "subject_dob", "mongo_conn", "checkKeys", "createExtract",
  "View", "write.csv", "write.table", "ymd", "mdy"
))

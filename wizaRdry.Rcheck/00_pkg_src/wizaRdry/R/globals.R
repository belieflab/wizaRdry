# globals.R
# Global variables used by wizaRdry package (CRAN compliant)

utils::globalVariables(c(
  # Existing variables
  "connectionString", "apiKeys", "baseUrls", "config",
  "uri", "token", "dob", "surveyIds", "pb", "value",
  "nda_base_url",
  # Additional variables
  "duplicates", "nda_base_url", "src_institution_id",
  "subject_dob", "mongo_conn", "checkKeys", "createExtract",
  "View", "write.csv", "write.table", "ymd", "mdy",
  "SUPER_REQUIRED_FIELDS",  # NDA super required fields constant
  "NdaDataStructure",  # R6 class for NDA field definitions
  "DCC_REQUIRED_FIELDS",  # DCC required fields constant
  "DCC_RECOMMENDED_FIELDS",  # DCC recommended fields constant
  "DCC_FIELDS"  # All DCC fields constant
))

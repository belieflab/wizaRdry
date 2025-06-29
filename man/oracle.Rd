% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getOracle.R
\name{oracle}
\alias{oracle}
\title{Fetch data from Oracle database to be stored in a data frame}
\usage{
oracle(
  table_name = NULL,
  ...,
  fields = NULL,
  where_clause = NULL,
  join_primary_keys = TRUE,
  custom_query = NULL,
  max_rows = NULL,
  date_format = NULL,
  batch_size = 1000,
  exclude_pii = TRUE,
  interview_date = NULL,
  all = FALSE,
  schema = NULL
)
}
\arguments{
\item{table_name}{Name of the SQL table or view to query}

\item{...}{Optional column names to filter for. Only rows with non-missing values
in ALL specified columns will be returned.}

\item{fields}{Optional vector of specific fields to select}

\item{where_clause}{Optional WHERE clause to filter results (without the "WHERE" keyword)}

\item{join_primary_keys}{Boolean, whether to join with the primary keys table (default: TRUE)}

\item{custom_query}{Optional custom SQL query to execute instead of building one}

\item{max_rows}{Optional limit on the number of rows to return}

\item{date_format}{Optional format for date fields (default uses ISO format)}

\item{batch_size}{Number of records to retrieve per batch for large datasets}

\item{exclude_pii}{Default TRUE to remove all fields marked as identifiable}

\item{interview_date}{Optional; can be either:
- A date string in various formats (ISO, US, etc.) to filter data up to that date
- A boolean TRUE to return only rows with non-NA interview_date values}

\item{all}{Logical; if TRUE, use LEFT OUTER JOIN instead of INNER JOIN (default: FALSE),
similar to the 'all' parameter in base R's merge() function}

\item{schema}{Optional schema name to use for table qualification}
}
\value{
A data frame containing the requested SQL data
}
\description{
Retrieves data from an Oracle table or view and optionally joins it with a primary keys table
as specified in the configuration.
}
\examples{
\dontrun{
# Get data from a specific table
data <- oracle("participants")

# Get data with a where clause
survey_data <- oracle("vw_surveyquestionresults",
                  where_clause = "resultidentifier = 'NRS'")

# Get all records, including those without matching primary key
all_data <- oracle("candidate", all = TRUE)

# Specify schema explicitly
schema_data <- oracle("survey_results", schema = "STUDY_DATA")
}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataParse.R
\name{mongo.rune}
\alias{mongo.rune}
\title{Parse composite MongoDB collection into component data frames by variable prefix}
\usage{
mongo.rune(collection, db_name = NULL, lower = TRUE)
}
\arguments{
\item{collection}{Character string specifying the Mongo collection}

\item{db_name}{Character string specifying the Mongo database}

\item{lower}{default TRUE convert prefixes to lower case}
}
\value{
Creates multiple dataframes in the global environment, one for each survey
detected in the data. Each dataframe is named after its survey prefix.
}
\description{
This function fetches a MongoDB collection containing multiple collections and
separates it into individual data frames for each collection detected in the data.
It identifies the appropriate identifier column (e.g., participantId, workerId)
and splits the data based on column name prefixes.
}
\details{
The function performs the following steps:
\itemize{
\item Retrieves the raw Qualtrics data using the getSurvey() function
\item Identifies which identifier column to use (participantId, workerId, PROLIFIC_PID, or src_subject_id)
\item Determines survey prefixes by analyzing column names
\item Creates separate dataframes for each survey prefix found
\item Assigns each dataframe to the global environment with names matching the survey prefixes
}
}
\examples{
\dontrun{
# Parse a Qualtrics export containing multiple surveys
mongo.rune("combined_surveys", label = FALSE)

# After running, access individual survey dataframes directly:
head(pss)  # Access the PSS survey dataframe
head(cesd) # Access the CESD survey dataframe
}

}

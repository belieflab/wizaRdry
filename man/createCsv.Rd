% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createCsv.R
\name{createCsv}
\alias{createCsv}
\title{Alias for 'to.csv'}
\usage{
createCsv(df, df_name = NULL, path = ".", skip_prompt = TRUE)
}
\arguments{
\item{df}{Data frame to be exported to CSV format.}

\item{df_name}{Optional; a custom file name for the saved CSV file.
If not provided, the name of the data frame variable is used.
The function adds the ".csv" extension automatically.}

\item{path}{Character string specifying the directory path where the "tmp" folder
and CSV file should be created. Defaults to the current working directory.}

\item{skip_prompt}{Logical. If TRUE (default), skips the confirmation prompt. If FALSE,
will prompt for confirmation unless the user has previously chosen to remember their preference.}
}
\value{
Invisible TRUE if successful. The function writes a CSV file to the specified path
and prints a message indicating the file's location.
}
\description{
This is a legacy alias for the 'to.csv' function to maintain compatibility with older code.
}
\examples{
\dontrun{
createCsv(prl01)
}
}

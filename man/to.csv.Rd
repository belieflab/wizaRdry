% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createCsv.R
\name{to.csv}
\alias{to.csv}
\title{Alias for 'createCsv'}
\usage{
to.csv(df, df_name = NULL)
}
\arguments{
\item{df}{DataFrame to be exported to CSV format.}

\item{df_name}{Optional; a custom file name for the saved CSV file.
If not provided, the name of the DataFrame variable is used.
The function adds the ".csv" extension automatically.}
}
\value{
The function writes a CSV file to the specified path and prints a message
indicating the file's location. This function does not return a value.
}
\description{
This is a legacy alias for the 'createCsv' function to maintain compatibility with older code.
}
\examples{
\dontrun{
to.csv(prl01)
}
}

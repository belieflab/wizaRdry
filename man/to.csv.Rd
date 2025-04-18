% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createCsv.R
\name{to.csv}
\alias{to.csv}
\title{Create CSV File from a DataFrame}
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
This function exports a given R DataFrame to a CSV file format.
The resulting file is saved in the "./tmp/" directory. If a filename
is not specified, the function uses the name of the DataFrame variable.
The ".csv" extension is appended automatically to the filename.
}
\examples{
\dontrun{
# Create a sample dataframe
sample_df <- data.frame(
  id = 1:3,
  name = c("Alice", "Bob", "Charlie")
)
to.csv(sample_df)
}
}
\author{
Joshua Kenney \href{mailto:joshua.kenney@yale.edu}{joshua.kenney@yale.edu}
}

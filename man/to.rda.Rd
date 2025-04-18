% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createRda.R
\name{to.rda}
\alias{to.rda}
\title{Create RDS File from a DataFrame}
\usage{
to.rda(df, df_name = NULL)
}
\arguments{
\item{df}{DataFrame to be exported to RDS format.}

\item{df_name}{Optional; a custom file name for the saved RDS file.
If not provided, the name of the DataFrame variable is used.
The function adds the ".Rda" extension automatically.}
}
\value{
The function writes an RDS file to the specified path and prints a message
indicating the file's location. This function does not return a value.
}
\description{
This function exports a given R DataFrame to an RDS file format.
The resulting file is saved in the "./tmp/" directory. If a filename
is not specified, the function uses the name of the DataFrame variable.
The ".Rda" extension is appended automatically to the filename.
}
\examples{
\dontrun{
# Create a sample dataframe
sample_df <- data.frame(
  id = 1:3,
  name = c("Alice", "Bob", "Charlie")
)
to.rda(sample_df)
}
}
\author{
Joshua Kenney \href{mailto:joshua.kenney@yale.edu}{joshua.kenney@yale.edu}
}

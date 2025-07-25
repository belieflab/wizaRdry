% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createSpss.R
\name{to.sav}
\alias{to.sav}
\title{Create .sav SPSS file from a data frame}
\usage{
to.sav(df, df_name = NULL, path = ".", skip_prompt = TRUE)
}
\arguments{
\item{df}{Data frame to be exported to SPSS format.}

\item{df_name}{Optional; custom file name for the saved SPSS file. If not provided,
the name of the data frame variable will be used. The ".sav" extension will
be appended automatically.}

\item{path}{Character string specifying the directory path where the "tmp" folder
and SPSS file should be created. Defaults to the current working directory.}

\item{skip_prompt}{Logical. If TRUE (default), skips the confirmation prompt. If FALSE,
will prompt for confirmation unless the user has previously chosen to remember their preference.}
}
\value{
Invisible TRUE if successful. Writes an SPSS file to the designated path and prints a message indicating
the file's location.
}
\description{
This function takes a R data frame and writes it to an SPSS file
using the Haven package. The resulting file will be stored in the
"tmp" directory with a default name derived from the data frame variable name,
but can be customized if desired. The function will prompt for confirmation
before creating the file, with an option to remember the user's preference for future calls.
}
\examples{
\dontrun{
# Create a sample data frame
sample_df <- data.frame(
  id = 1:3,
  score = c(85, 92, 78),
  group = c("A", "B", "A")
)

# Basic usage with prompt
to.sav(sample_df)

# Custom filename
to.sav(sample_df, "participants_data")

# Skip the confirmation prompt
to.sav(sample_df, skip_prompt = TRUE)

# Save in a different directory
to.sav(sample_df, path = "path/to/project")
}

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataMerge.R
\name{dataMerge}
\alias{dataMerge}
\title{Alias for 'meld'}
\usage{
dataMerge(
  ...,
  by = NULL,
  all = TRUE,
  no.dups = FALSE,
  csv = FALSE,
  rdata = FALSE,
  spss = FALSE
)
}
\arguments{
\item{...}{Clean data frames to be merged.}

\item{by}{A vector of strings specifying the column names to be used as merge keys. If NULL,
the function automatically determines common keys from the provided data frames.}

\item{all}{Logical; if TRUE, performs an OUTER JOIN. If FALSE, performs an INNER JOIN.}

\item{no.dups}{Logical; if TRUE, duplicates are removed post-merge.}

\item{csv}{Logical; if TRUE, the merged data frame is exported as a CSV file.}

\item{rdata}{Logical; if TRUE, the merged data frame is saved as an Rda file.}

\item{spss}{Logical; if TRUE, the merged data frame is exported as an SPSS file.}
}
\value{
A merged data frame based on the specified or common candidate keys.
}
\description{
This is a legacy alias for the 'meld' function to maintain compatibility with older code.
}
\examples{
\dontrun{
merged <- dataMerge(df1_clean, df2_clean)
}
}

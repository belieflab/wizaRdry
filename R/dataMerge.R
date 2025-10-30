#' Merge two or more data frames magically according to their candidate key
#'
#' This function simplifies the process of merging multiple cleaned data frames by automatically determining 
#' common merge keys or utilizing user-specified keys. Supports both inner and outer join methods, 
#' and offers options for exporting the merged data.
#'
#' @param ... Clean data frames to be merged.
#' @param by A vector of strings specifying the column names to be used as merge keys. If NULL, 
#'           the function automatically determines common keys from the provided data frames.
#' @param all Logical; if TRUE, performs an OUTER JOIN. If FALSE, performs an INNER JOIN.
#' @param no.dups Logical; if TRUE, duplicates are removed post-merge.
#' @param csv Logical; if TRUE, the merged data frame is exported as a CSV file.
#' @param rdata Logical; if TRUE, the merged data frame is saved as an Rda file.
#' @param spss Logical; if TRUE, the merged data frame is exported as an SPSS file.
#'
#' @examples
#' \dontrun{
#' # Create sample dataframes for demonstration
#' df1 <- data.frame(
#'   src_subject_id = c("S001", "S002", "S003"),
#'   visit = c(1, 2, 1),
#'   measure1 = c(10, 15, 12),
#'   stringsAsFactors = FALSE
#' )
#' 
#' df2 <- data.frame(
#'   src_subject_id = c("S001", "S002", "S004"),
#'   visit = c(1, 2, 2),
#'   measure2 = c(85, 92, 78),
#'   stringsAsFactors = FALSE
#' )
#' 
#' # Perform an OUTER JOIN using default keys:
#' merged1 <- meld(df1, df2, all = TRUE)
#' 
#' # Perform an INNER JOIN using specified keys:
#' merged2 <- meld(df1, df2, by = "src_subject_id", all = FALSE)
#' }
#' @return A merged data frame based on the specified or common candidate keys.
#' @author Joshua Kenney <joshua.kenney@yale.edu>
#' @export
meld <- function(..., by = NULL, all = TRUE, no.dups = FALSE, csv = FALSE, rdata = FALSE, spss = FALSE) {
  
  # Inform about the type of join being performed
  message(ifelse(all, "Performing an OUTER JOIN.", "Performing an INNER JOIN."))
  
  config <- validate_config()
  
  if (config$study_alias == "capr") {
    # NDA variables suitable for merging fromr capr
    super_key <- c("src_subject_id", "subjectkey", "phenotype", "visit", "week", "sex", "site", "arm")
  } else {
    super_key <- c("src_subject_id", "subjectkey", "phenotype", "visit", "week", "sex", "site", "arm", "state", "PROLIFIC_PID", "participantId", "workerId", "rat_id")
  }
  
  # Load custom scripts if any
  
  data_list <- list(...)
  
  #if one inputs a list of dataframes then we need to unpack the list using the
  #following code. For example you can now do this:
  #create a list of dfs to merge
  #  dfs_to_merge = NULL
  #  for (name in variables_to_merge){
  #      dfs_to_merge[[name]]<-base::get(name)
  #      }
  #then input that list into dataMerge
  #  merged_df<-dataMerge(dfs_to_merge)
  
  if (length(data_list) == 1){
    data_list = data_list[[1]]
  }
  
  # Preprocess data frames: Remove specified columns and ungroup
 # data_list <- lapply(data_list, function(df) {
#    # Remove 'interview_date' and 'interview_age' columns
#    df <- df[setdiff(names(df), c("interview_date", "interview_age"))]
#    return(df)
#  })
  
  # Determine the keys to use for merging
  if (is.null(by)) {
    by <- Reduce(intersect, lapply(data_list, function(df) intersect(super_key, names(df))))
    message("Detected common candidate keys for merge: ", toString(by))
  } else {
    by <- by
    message("Using user-specified keys for merge: ", toString(by))
  }
  # Perform the merging process
  dfs <- Reduce(function(x, y) base::merge(x, y, by = by,
                                            all = all, no.dups = no.dups), data_list)
  
  # Export merged data if requested
  if (csv) { createCsv(dfs, "merged_dfs.csv") }
  if (rdata) { createRds(dfs, "merged_dfs") }
  if (spss) { createSpss(dfs, "merged_dfs.sav") }
  
  
  return(dfs)
}


#' Alias for 'meld' (DEPRECATED)
#'
#' This function is deprecated. Please use 'meld' instead.
#' This is a legacy alias for the 'meld' function to maintain compatibility with older code.
#'
#' @inheritParams meld
#' @inherit meld return
#' @export
#' @examples
#' \dontrun{
#' # DEPRECATED - use meld() instead
#' merged <- dataMerge(df1_clean, df2_clean)
#' }
dataMerge <- function(...) {
  .Deprecated("meld", package = "wizaRdry")
  meld(...)
}

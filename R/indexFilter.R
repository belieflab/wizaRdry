#' Case-insensitive regex mask for .index() pattern search
#'
#' Shared helper used by the `.index()` discovery methods to filter their
#' listings by a user-supplied search string.
#'
#' @param x Character vector to test.
#' @param pattern Single regex string, or NULL for no filtering.
#' @return Logical vector the same length as `x`; NA matches are coerced to
#'   FALSE so they never accidentally pass the filter.
#' @noRd
index_grep <- function(x, pattern) {
  if (is.null(pattern)) return(rep(TRUE, length(x)))
  if (!is.character(pattern) || length(pattern) != 1L) {
    stop("`pattern` must be a single character string.", call. = FALSE)
  }
  m <- grepl(pattern, x, ignore.case = TRUE)
  m[is.na(m)] <- FALSE
  m
}

# Define the loading animation function
show_loading_animation <- function() {
  cat("Loading ")
  pb <- txtProgressBar(min = 0, max = 20, style = 3)
  
  for (i in 1:20) {
    Sys.sleep(0.1) # Simulate some computation time
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
}

# Define the progress callback function
progress_callback <- function(count, total) {
  setTxtProgressBar(pb, count) # Update the loading animation
}

initializeLoadingAnimation <- function(max_progress) {
  # cat("Loading...\n")
  pb <- txtProgressBar(min = 0, max = max_progress, style = 3)
  return(pb)
}

updateLoadingAnimation <- function(pb, current_progress) {
  setTxtProgressBar(pb, current_progress)
}

completeLoadingAnimation <- function(pb) {
  close(pb)
}


#' Initialize or Update Loading Animation
#'
#' Initializes a new loading animation or updates an existing one based on the progress.
#'
#' @param step Current step (chunk being processed).
#' @param total_steps Total number of chunks.
#' @param pb An existing txtProgressBar object; if NULL, a new progress bar is created.
#' @return A txtProgressBar object representing the current state of the progress bar.
#' @examples
#' pb <- show_loading_animation(step = 1, total_steps = 20)  # Start new animation
#' pb <- show_loading_animation(step = 2, total_steps = 20, pb = pb)  # Update animation
#' @export
show_loading_animation_with_chunks <- function(step = NULL, total_steps = NULL, pb = NULL) {
  if (is.null(pb)) {
    # cat("Loading:\n")  # Comment this out if you don't want the static "Loading..." message
    pb <- txtProgressBar(min = 0, max = total_steps, style = 3)
  }
  if (!is.null(step) && !is.null(total_steps)) {
    setTxtProgressBar(pb, step)
  }
  return(pb)
}


#' Plot the tracking result
#'
#' This function plots the tracking data created with "track"
#'
#' @param tracking The output of "track"
#' @param ... Additional parameters passed to "plot"
#' @export
inspect_tracking = function(tracking, ...) {
  # Display the tracked path
  plot(tracking$x, tracking$y, type="l",
       asp=1, xlab="x [px]", ylab="y [px]",
       ...)
}

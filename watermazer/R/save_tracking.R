#' Save the tracking result
#'
#' This function save the tracking data created with "track" as a csv file
#'
#' @param path The path of the output csv file
#' @param tracking The output of "track"
#' @export
save_tracking = function(path, tracking) {
  # Save tracking data to a text file
  write.csv(tracking, path, row.names=F)
}

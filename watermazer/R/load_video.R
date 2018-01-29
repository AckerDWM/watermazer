#' Load a video
#'
#' This function loads a video file
#'
#' @param im_path Path to the input file
#' @param fps Desired frames per second
#' @param duration Length of the video segment in seconds
#' @param start_frame The first frame to import
#' @return An image with the extracted frames along the "z" coordinates
#' @import magrittr
#' @export
load_video = function(im_path, fps=5, duration=60, start_frame=1) {
  # load the video
  imager::load.video(
    im_path, maxSize=4, skip.to=start_frame,
    fps=fps, frames=duration*fps) %>%
    imager::grayscale() %>%
    imager::imsplit(axis="z")
}

#' Create a representative video
#'
#' This function creates a video with the tracking data created with "track"
#' overlayed on corresponding frames of the raw video used for tracking
#'
#' @param tracking The output of "track"
#' @param in_path The path of the original video file
#' @param out_path The path of the output video file
#' @param fps The "fps" value used when loading the video for tracking
#' @param duration The "duration" value used when loading the video for tracking
#' @param start_frame The "start_frame" value used when loading the videov
#' @import magrittr
#' @export
create_tracking_video = function(
tracking, in_path, out_path, fps=5, duration=60, start_frame=1) {
  # Make a video where the tracked position is marked by a red dot
  video_color =
    imager::load.video(
      in_path, maxSize=4, skip.to=start_frame,
      fps=fps, frames=duration*fps) %>%
    imager::imsplit("z")

  for (t in fps:(nrow(tracking)-fps)) {
    x = round(tracking$x[t])
    y = round(tracking$y[t])
    video_color[[t]][(x-5):(x+5), (y-5):(y+5), , 1] = 1
    video_color[[t]][(x-5):(x+5), (y-5):(y+5), , 2] = 0
    video_color[[t]][(x-5):(x+5), (y-5):(y+5), , 3] = 0
  }

  video_color %<>% imager::imappend("z")
  imager::save.video(video_color, fps=fps, fname=out_path)
}

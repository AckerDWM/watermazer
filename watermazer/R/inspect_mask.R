#' Display a masked video frame
#'
#' This function plots a video frame with areas outside of the mask removed
#'
#' @param video An image with the extracted frames along the "z" coordinates
#' @param mask A mask matrix made with create_mask
#' @param frame The video frame to show
#' @import magrittr
#' @export
inspect_mask = function(video, mask, frame) {
  # display a masked video frame
  video[[frame]] %>%
    {.[,][mask==1] = 1;.} %>%
    plot()
}

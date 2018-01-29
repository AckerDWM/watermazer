#' Create a mask
#'
#' Create a mask for the tracking area
#'
#' @param video An image with the extracted frames along the "z" coordinates
#' @param frame The frame number to use as the basis for the mask
#' @param threshold The luminance threshold, 0:1, used to create the mask
#' @param dilation The thickness of the band of mask edge pixels to crop
#' @return A mask matrix valued 0 inside the mask
#' @import magrittr
#' @export
select_mask = function(video, frame=1, threshold=.3, dilation=100) {
  # create mask
  frame = video[[frame]]

  labels = frame %>%
    {.[,]} %>%
    {.>threshold} %>%
    imager::as.cimg() %>%
    imager::label()

  tab = table(c(labels))
  arena_idx = names(tab)[which.max(tab)] %>% as.numeric()

  mat = frame[,]
  mat[,] = 0
  mat[labels[,] != arena_idx] = 1

  mask =
    mat %>%
    imager::as.cimg() %>%
    imager::erode_square(50) %>%
    imager::dilate_square(dilation) %>%
    {.[,]}
}

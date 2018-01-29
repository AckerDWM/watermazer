#' Track the mouse's position
#'
#' This function tracks the mouse's position at all frames in the video
#'
#' @param video An image with the extracted frames along the "z" coordinates
#' @param mask A mask matrix made with create_mask
#' @param smoothing if TRUE, smooth the tracking result with a rolling median filter
#' @return A dataframe with the mouse's centroid in x and y, in pixels, at each time and frame
#' @import magrittr
#' @export
track = function(video, mask, smoothing=F) {
  # Perform tracking
  centroids = lapply(video, function(frame) {
    labelled_im = # label dark objects
      frame %>%
      {.[,][mask==1] = 1;.} %>%
      imager::as.cimg() %>%
      imager::isoblur(1) %>%
      as.matrix() %>%
      tanh() %>%
      imager::threshold(.3) %>%
      imager::as.cimg() %>%
      imager::distance_transform(1) %>%
      imager::threshold(.5) %>%
      imager::erode_square(3) %>%
      imager::label()

    labels = # get label names
      labelled_im[,] %>%
      c() %>%
      unique()

    if (length(labels) == 0) return(c(NA, NA)) # error behavior

    object_sizes = sapply(labels, function(label) { # get biggest object
      (labelled_im[,] == label) %>% sum() %>% sum()
    })

    # get center of mass of biggest object
    coords = reshape2::melt(labelled_im[,] == which.max(object_sizes))
    x_centroid = coords %>% dplyr::filter(value) %$% mean(Var1)
    y_centroid = coords %>% dplyr::filter(value) %$% mean(Var2)
    centroid = c(x_centroid, y_centroid)

    return(centroid)
  }) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    dplyr::rename(x=X1, y=X2)

  # Kalman interpolation of missing frames
  centroids$x = imputeTS::na.kalman(centroids$x)
  centroids$y = imputeTS::na.kalman(centroids$y)

  # add frame number and time annotations
  centroids$frame = 1:nrow(centroids)
  centroids$time_seconds = centroids$frame/fps

  return(centroids)
}

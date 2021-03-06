% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inspect_mask.R
\name{inspect_mask}
\alias{inspect_mask}
\title{Display a masked video frame}
\usage{
inspect_mask(video, mask, frame)
}
\arguments{
\item{video}{An image with the extracted frames along the "z" coordinates}

\item{mask}{A mask matrix made with create_mask}

\item{frame}{The video frame to show}
}
\description{
This function plots a video frame with areas outside of the mask removed
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-ascii.R
\name{convert_to_grayscale}
\alias{convert_to_grayscale}
\title{Convert Image Array to Grayscale Matrix}
\usage{
convert_to_grayscale(img_array)
}
\arguments{
\item{img_array}{A numeric array representing the image (values 0-1).
Can be 3D (height x width x channels) or 2D (height x width).}
}
\value{
A 2D numeric matrix (height x width) of grayscale values (0-1).
Returns NULL if input is invalid.
}
\description{
Converts a 3D (color) or 2D (assumed grayscale) image array to a
2D grayscale matrix using the luminosity method.
}
\keyword{internal}

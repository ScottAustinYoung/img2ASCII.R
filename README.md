
# img2ASCII

The goal of img2ASCII is to convert images (JPEG or PNG) into ASCII text
representations. It provides both underlying functions for conversion
and a simple Shiny webb application for interactive use.

## Installation

You can install the development version of img2ASCII from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ScottAustinYoung/img2ASCII.R")
```

## Example

The primary way to use this package is via the included Shiny
application.

``` r
library(img2ASCII)
launch_img2ascii_app()
```

In this app you can: - Upload a JPG or PNG image. - Adjust the maximum
width of the generated ASCII array. - Adjust font size for the preview
window. - Download the result as a text file or as an image.

## License

This package is licensed under the terms of the GNU General Public
License v3.0.

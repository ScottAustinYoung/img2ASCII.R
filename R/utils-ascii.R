# R/utils-ascii.R

#' Convert Image Array to Grayscale Matrix
#'
#' Converts a 3D (color) or 2D (assumed grayscale) image array to a
#' 2D grayscale matrix using the luminosity method.
#'
#' @param img_array A numeric array representing the image (values 0-1).
#'   Can be 3D (height x width x channels) or 2D (height x width).
#'
#' @return A 2D numeric matrix (height x width) of grayscale values (0-1).
#'   Returns NULL if input is invalid.
#' @keywords internal
convert_to_grayscale <- function(img_array) {
  if (is.null(img_array) || !is.numeric(img_array)) return(NULL)
  d <- dim(img_array)

  if (length(d) == 2) {
    return(img_array) # Already grayscale
  } else if (length(d) == 3 && d[3] >= 3) {
    # Standard luminosity formula (assuming R=1, G=2, B=3)
    gray_matrix <- 0.3 * img_array[,,1] + 0.59 * img_array[,,2] + 0.11 * img_array[,,3]
    return(gray_matrix)
  } else if (length(d) == 3 && d[3] < 3) {
    # Handle cases like 2-channel images (e.g. grayscale + alpha) - use first channel
    return(img_array[,,1, drop = TRUE])
  }
  else {
    warning("Input array has unexpected dimensions for grayscale conversion.")
    return(NULL)
  }
}


#' Calculate Target Dimensions for ASCII Art
#'
#' Calculates appropriate height based on target width, original image
#' dimensions, and character aspect ratio.
#'
#' @param original_dims A numeric vector `c(height, width)` of the original image.
#' @param target_width The desired output width in characters.
#' @param original_width The original width in pixels (used as cap for target_width).
#' @param char_aspect_ratio The height/width ratio of characters (e.g., 2.2).
#'
#' @return A list containing `width` (integer) and `height` (integer) for the target ASCII dimensions.
#' @keywords internal
calculate_target_dims <- function(original_dims, target_width, original_width, char_aspect_ratio = 2.2) {

  # Cap target width by original width
  target_width_capped <- min(target_width, original_width)

  # Check for valid original dimensions before calculating aspect ratio
  if(is.null(original_dims) || length(original_dims) != 2 || original_dims[2] == 0) {
    original_aspect_ratio <- 1 # Default if invalid
  } else {
    original_aspect_ratio <- original_dims[1] / original_dims[2] # height / width
  }

  target_height <- round(target_width_capped * original_aspect_ratio / char_aspect_ratio)

  # Enforce minimum dimensions
  final_width <- max(10, round(target_width_capped))
  final_height <- max(10, round(target_height))

  return(list(width = final_width, height = final_height))
}


#' Downsample Grayscale Image
#'
#' Reduces the size of a grayscale image matrix using nearest neighbor sampling.
#'
#' @param gray_matrix The input grayscale matrix (values 0-1).
#' @param target_dims A list containing `width` and `height` (integers).
#'
#' @return The downsampled grayscale matrix.
#' @keywords internal
downsample_image <- function(gray_matrix, target_dims) {
  if (is.null(gray_matrix) || is.null(target_dims)) return(NULL)

  target_height <- target_dims$height
  target_width <- target_dims$width
  original_height <- nrow(gray_matrix)
  original_width <- ncol(gray_matrix)

  if(original_height == 0 || original_width == 0) return(NULL)

  row_indices <- round(seq(1, original_height, length.out = target_height))
  col_indices <- round(seq(1, original_width, length.out = target_width))

  # Clamp indices
  row_indices <- pmin(pmax(row_indices, 1), original_height)
  col_indices <- pmin(pmax(col_indices, 1), original_width)

  gray_small <- gray_matrix[row_indices, col_indices, drop = FALSE]
  return(gray_small)
}


#' Map Grayscale Matrix to ASCII Characters
#'
#' Converts a matrix of grayscale values (0-1) into lines of ASCII characters.
#'
#' @param gray_matrix The input grayscale matrix (values 0-1).
#' @param char_set A vector of characters ordered from darkest (index 1, value 0)
#'   to lightest (last index, value 1). If NULL, a default set is used.
#'   Note: The mapping assumes 0=dark, 1=light based on the formula used.
#'   The original script's character set implies 0=dark (@), 1=light (.).
#'
#' @return A character vector where each element is a line of ASCII art.
#' @keywords internal
map_to_ascii <- function(gray_matrix, char_set = NULL) {
  if (is.null(gray_matrix)) return(NULL)
  if (is.null(char_set)) {
    # Default set (darkest @ to lightest .) consistent with original script mapping
    char_set <- c("@", "#", "S", "%", "?", "*", "+", ";", ":", ",", ".")
  }
  num_chars <- length(char_set)
  if (num_chars == 0) stop("Character set cannot be empty.")

  # Ensure values are clamped [0, 1] before mapping
  gray_clamped <- pmin(pmax(gray_matrix, 0), 1)

  ascii_lines <- apply(gray_clamped, 1, function(row) {
    # Map value [0,1] to index [1, length(chars)]
    # 0 maps to index 1 (darkest char), 1 maps to index N (lightest char)
    char_indices <- pmin(pmax(round(row * (num_chars - 1)) + 1, 1), num_chars)
    paste(char_set[char_indices], collapse = "")
  })

  return(ascii_lines)
}


#' Save ASCII Art as an Image File
#'
#' Takes lines of ASCII art and renders them to a PNG or JPEG file.
#'
#' @param ascii_lines A character vector representing the ASCII art.
#' @param file_path The full path to save the output image file.
#' @param format Character string, either "png" or "jpeg".
#' @param char_aspect_ratio Numeric, the aspect ratio used for rendering characters.
#' @param char_width_px Integer, the base width of a character in pixels for rendering.
#' @param padding_px Integer, the padding around the text in the image in pixels.
#' @param bg Character string, background color for the image.
#' @param color Character string, text color.
#' @param font_family Character string, font family to use (should be monospace).
#' @param cex Numeric, character expansion factor for plotting.
#'
#' @return Invisible NULL. Writes the image to `file_path`.
#' @keywords internal
save_ascii_image <- function(ascii_lines, file_path, format = "png",
                             char_aspect_ratio = 2.2, char_width_px = 10, padding_px = 20,
                             bg = "white", color = "black", font_family = "mono", cex = 1) {

  if (is.null(ascii_lines) || length(ascii_lines) == 0) {
    warning("No ASCII lines provided to save_ascii_image.")
    return(invisible(NULL))
  }
  n_lines <- length(ascii_lines)
  # Use suppressWarnings for max() on empty string if ascii_lines has zero-length elements
  max_chars <- suppressWarnings(max(nchar(ascii_lines), na.rm = TRUE))
  if (!is.finite(max_chars)) max_chars <- 0 # Handle case where all lines might be empty

  # Calculate image dimensions
  char_height_px <- char_width_px * char_aspect_ratio
  width_px <- max_chars * char_width_px + (2 * padding_px)
  height_px <- n_lines * char_height_px + (2 * padding_px)

  # Ensure minimum dimensions for graphics device
  width_px <- max(width_px, 48) # Common minimum for some devices
  height_px <- max(height_px, 48)


  # Open graphics device
  if (tolower(format) == "png") {
    grDevices::png(file_path, width = width_px, height = height_px, units = "px", res = 96)
  } else if (tolower(format) == "jpeg" || tolower(format) == "jpg") {
    grDevices::jpeg(file_path, width = width_px, height = height_px, units = "px", res = 96, quality = 90)
  } else {
    stop("Unsupported image format specified: ", format)
  }

  # Ensure device is closed even if errors occur in plotting
  on.exit(grDevices::dev.off())

  # Plotting
  current_par <- graphics::par(mar = c(0, 0, 0, 0), bg = bg)
  on.exit(graphics::par(current_par), add = TRUE) # Restore graphics parameters on exit

  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::text(0.5, 0.5, paste(ascii_lines, collapse = "\n"),
                 family = font_family, col = color, adj = c(0.5, 0.5), cex = cex)

  # dev.off() is handled by on.exit
  return(invisible(NULL))
}

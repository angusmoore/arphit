library(magick)

#' Make slides in gif using gifski
#'
#' Flick through ggs in a gif
#'
#' @param gg_list List of gg objects
#' @param filename Path to save .gif to
#' @param delay Delay in seconds for each frame
#' @param loop Whether to loop through frames
#'
#' @return filename, if successful
#' @export
agg_slides_gifski <- function(gg_list, filename = NULL,
                              delay = 1, loop = TRUE) {

  if (!requireNamespace("gifski", quietly = TRUE)) {
    stop("Please install 'gifski' to use this feature.")
  }

  png_files <- sapply(seq_along(gg_list), function(idx, gg_list) {
    file_name <- file.path(tempdir(), paste0("animate-", idx, ".png"))
    agg_draw(gg_list[[idx]], file_name)
    file_name
  }, gg_list)
  on.exit(unlink(png_files))

  info <- magick::image_info(image_read(png_files[1]))
  width <- info$width[1]
  height <- info$height[1]

  if (is.null(filename)) {
    filename <- "agg.gif"
  }
  gifski::gifski(gif_file = filename, png_files = png_files,
                 width = width, height = height,
                 delay = delay, loop = loop)

}

#' Make slides in gif using magick
#'
#' Flick through ggs in a gif
#'
#' @param gg_list List of gg objects
#' @param filename Path to save .gif to
#' @param delay Delay in seconds for each frame
#' @param loop Whether to loop through frames
#'
#' @return filename, if successful
#' @export
agg_slides_magick <- function(gg_list, filename = NULL,
                                 delay = 1, loop = 0) {

  img_list <- lapply(seq_along(gg_list), function(idx, gg_list) {
    file_name <- file.path(tempdir(), paste0("animate-", idx, ".png"))
    agg_draw(gg_list[[idx]], file_name)
    list(path = file_name, img = image_read(file_name))
  }, gg_list)

  on.exit(unlink(sapply(img_list, "[[", "path")))

  animation <- magick::image_animate(
    image = do.call(c, lapply(img_list, "[[", "img")),
    fps = 1/delay,
    loop = loop
  )

  if (is.null(filename)) {
    filename <- "agg.gif"
  }

  magick::image_write(animation, path = filename)

  filename
}

#' Make slides in gif
#'
#' Flick through ggs in a gif
#'
#' @param gg_list List of gg objects
#' @param filename Path to save .gif to
#' @param delay Delay in seconds for each frame
#' @param loop Whether to loop through frames
#'
#' @return filename, if successful
#' @export
agg_slides <- function(gg_list, filename = NULL, delay = 1, loop = 0) {
  agg_slides_magick(gg_list, filename = filename, delay = delay, loop = loop)
}

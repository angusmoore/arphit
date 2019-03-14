library(magick)

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
agg_slides <- function(gg_list, filename = NULL, delay = 1, loop = 0) {

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

}

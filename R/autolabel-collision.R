get_underlay_bitmap <- function(gg, margins) {
  plot_device <- grDevices::dev.cur()
  gg$enable_autolabeller <- FALSE
  agg_draw(gg, filename = paste0(tempdir(), "/autolabel-temp.pdf"))
  image <- magick::image_read_pdf(paste0(tempdir(), "/autolabel-temp.pdf"))
  suppressWarnings(file.remove(paste0(tempdir(), "/autolabel-temp.pdf")))

  # Crop off the outer material
  top <- as.integer(CSI*margins$top*PNGDPI)
  bottom <- as.integer(CSI*margins$bottom*PNGDPI)
  left <- as.integer(CSI*margins$left*PNGDPI)
  right <- as.integer(CSI*margins$right*PNGDPI)
  crop_width <- magick::image_info(image)$width - left - right
  crop_height <- magick::image_info(image)$height - top - bottom
  image <- magick::image_crop(image, paste0(crop_width,"x",crop_height,"+",left,"+",top))

  # Convert to a logical matrix, true for non-white
  image_map <- magick::image_data(image, "gray")
  image_map <- drop(image_map)
  white_raw <- as.raw(255)

  # reset the active device
  grDevices::dev.set(plot_device)

  return(image_map != white_raw)
}

line_y_indices <- function(text, y, ylim, y_start, padding, line_n, total_lines, total_height) {
  lheight <- total_height / total_lines
  line_offset <- 0.5*total_height - (0.5+line_n-1)*lheight

  top <- y + (0.5*getstrheight(text, units = "inches") + line_offset) / (graphics::par("pin")[2])*(ylim$max-ylim$min)
  bottom <- y + ( - 0.5*getstrheight(text, units = "inches") + line_offset) / (graphics::par("pin")[2])*(ylim$max-ylim$min)

  if (line_n == 1)  top <- top + padding / (graphics::par("pin")[2])*(ylim$max-ylim$min)
  if (line_n == total_lines) bottom <- bottom - padding / (graphics::par("pin")[2])*(ylim$max-ylim$min)

  which(y_start < top & (y_start + y_start[2]-y_start[1]) > bottom)
}

create_text_bitmap <- function(x,y,text,xlim,ylim,dim,layout,p,padding = AUTOLABEL_PADDING) {
  x_scale <- graphics::par("mfrow")[2]
  y_scale <- graphics::par("mfrow")[1]

  x_start <- seq(from = xlim[1], to = xlim[2], length.out = round(dim[1]/x_scale) + 1)[1:round(dim[1]/x_scale)]
  y_start <- seq(from = ylim$max, to = ylim$min, length.out = round(dim[2]/y_scale) + 1)[2:(round(dim[2]/y_scale)+1)]

  left <- x - 0.5*(getstrwidth(text, units = "inches") + padding) / (graphics::par("pin")[1])*(xlim[2]-xlim[1])
  right <- x + 0.5*(getstrwidth(text, units = "inches") + padding) / (graphics::par("pin")[1])*(xlim[2]-xlim[1])
  x_indices <- which(x_start < right & (x_start + x_start[2]-x_start[1]) > left)

  lines <- stringr::str_split(text, stringr::fixed("\n"))[[1]]

  y_indices <-
    do.call(c, lapply(seq_along(lines), function(line_n)
      line_y_indices(
        lines[[line_n]],
        y,
        ylim,
        y_start,
        padding,
        line_n,
        length(lines),
        getstrheight(text, units = "inches")
      )))

  x_indices <- x_indices + round((dim[1]/x_scale) * (getlocation(p, layout)[2] - 1))
  y_indices <- y_indices + round((dim[2]/y_scale) * (getlocation(p, layout)[1] - 1))

  list(x=x_indices,y=c(y_indices,max(y_indices)+1))
}

shift_text_indices <- function(indices,x,y,x_text_anchor,y_text_anchor,x_scale,y_scale,xlim,ylim,dim,layout,p) {
  x_shift <- round((x - x_text_anchor) / (xlim[2] - xlim[1]) * (dim[1]/x_scale))
  y_shift <- -round((y - y_text_anchor) / (ylim$max - ylim$min) * (dim[2]/y_scale))

  x_indices <- indices$x + x_shift
  y_indices <- indices$y + y_shift

  return(list(x = x_indices, y = y_indices))
}

test_collision <- function(underlay, x_indices, y_indices, dim) {
  any(x_indices < 0) || any(x_indices > dim[1]) || any(y_indices < 0) ||
    any(y_indices > dim[2]) || any(underlay[x_indices, y_indices])
}

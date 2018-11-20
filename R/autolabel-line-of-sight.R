lineofsight <- function(x, y, a, b, los_mask, xlim, ylim) {
  !any(los_mask[create_arrow_bitmap(x, y, a, b, dim(los_mask), "1", "1", xlim, ylim)])
}

cartesian2linear <- function(r, c, dims) {
  r + (c-1)*dims[1]
}

create_arrow_bitmap <- function(tail.x,tail.y,head.x,head.y,dims,layout,p,xlim,ylim) {
  x_scale <- graphics::par("mfrow")[2]
  x_shift <- round(dims[1]/x_scale) * (getlocation(p, layout)[2] - 1)
  y_scale <- graphics::par("mfrow")[1]
  y_shift <- round(dims[2]/x_scale) * (getlocation(p, layout)[1] - 1)

  tail.x <- (tail.x - xlim[1])/(xlim[2]-xlim[1])*dims[1]
  head.x <- (head.x - xlim[1])/(xlim[2]-xlim[1])*dims[1]
  tail.y <- dims[2] - (tail.y - ylim$min)/(ylim$max-ylim$min)*dims[2]
  head.y <- dims[2] - (head.y - ylim$min)/(ylim$max-ylim$min)*dims[2]

  x_points <- floor(tail.x):ceiling(head.x)

  if (abs(tail.x - head.x) > 1e-5) {
    m <- (tail.y-head.y)/(tail.x-head.x)
    c <- tail.y - m*tail.x
    y_points <- round(m*x_points + c)
    y_points[y_points > max(head.y,tail.y)] <- max(head.y,tail.y)
    y_points[y_points < min(head.y,tail.y)] <- min(head.y,tail.y)
  } else {
    x_points <- round(head.x)
    y_points <- head.y:tail.y
  }

  linear_indices <- mapply(x_points, y_points, FUN = function(x,y) cartesian2linear(x,y,dims))

  # Do it for y points too - helps for very steep lines, which get poor coverage on y-points by my other method
  y_points <- floor(tail.y):ceiling(head.y)
  if (abs(tail.y - head.y) > 1e-5) {
    m <- (tail.x-head.x)/(tail.y-head.y)
    c <- tail.x - m*tail.y
    x_points <- round(m*y_points + c)
    x_points[x_points > max(head.x,tail.x)] <- max(head.x,tail.x)
    x_points[x_points < min(head.x,tail.x)] <- min(head.x,tail.x)
    linear_indices <-
      append(linear_indices,
             mapply(
               x_points,
               y_points,
               FUN = function(x, y)
                 cartesian2linear(x, y, dims)
             ))
  }

  return(linear_indices)
}

los_mask_series_draw <- function(series, exclude, xvals, ists, freq, data, xlim, ylim, bars, bar.stacked, log_scale, joined) {
  x <- getxvals(data, ists, xvals)
  colors <- as.list(rep("black",length(series)))
  barcol <- as.list(rep("white",length(series)))
  pch <- as.list(rep(NA,length(series)))
  names(colors) <- series
  names(barcol) <- series
  names(pch) <- series
  colors[exclude] <- "white"
  drawbars(c(1,1), series, bars, data, x, ists, freq, list(col = colors, barcol = barcol), xlim, ylim, bar.stacked, log_scale)
  drawlines(c(1,1), series, bars, data, x, list(col = colors, pch = pch), xlim, ylim, joined, log_scale)
}

create_los_mask <- function(series, panels, p, data, xvals, dims, xlim, ylim, bars, bar.stacked, layout, log_scale, joined) {
  plot_device <- grDevices::dev.cur()
  grDevices::png(
    filename = paste0(tempdir(), "/autolabel-los-mask.png"),
    width = dims[1] / graphics::par("mfrow")[2],
    height = dims[2] / graphics::par("mfrow")[1],
    res = PNGDPI
  )
  graphics::par(family = "sans", xaxs = "i", yaxs = "i", ps = 20, cex.main = (28/20), cex.axis = 1, las = 1, lheight = 1)
  graphics::par(omi = c(0,0,0,0), mar = c(0,0,0,0))
  graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "",
                 xlim = c(0,1), ylim = c(0, 1))

  # draw the lines on
  los_mask_series_draw(panels[[p]],
                       series,
                       xvals[[p]],
                       !is.null(xvals[[paste0(p, "ts")]]),
                       xvals[[paste0(p, "freq")]],
                       data[[p]],
                       xlim[[p]],
                       ylim[[p]],
                       bars[[p]],
                       bar.stacked,
                       log_scale,
                       joined)

  # Do RHS axes if necessary
  if (!is.null(other_axes(p, layout))) {
    p <- other_axes(p, layout)
    los_mask_series_draw(panels[[p]],
                         series,
                         xvals[[p]],
                         !is.null(xvals[[paste0(p, "ts")]]),
                         xvals[[paste0(p, "freq")]],
                         data[[p]],
                         xlim[[p]],
                         ylim[[p]],
                         bars[[p]],
                         bar.stacked,
                         log_scale,
                         joined)
  }
  grDevices::dev.off()
  image <- magick::image_read(paste0(tempdir(), "/autolabel-los-mask.png"))
  image_map <- magick::image_data(image, "gray")
  grDevices::dev.set(plot_device)
  image_map <- drop(image_map)
  white_raw <- as.raw(255)
  return(image_map != white_raw)
}

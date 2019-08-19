lineofsight <- function(x, y, a, b, los_mask, xlim, ylim) {
  !any(los_mask[create_arrow_bitmap(x, y, a, b, dim(los_mask), xlim, ylim)])
}

cartesian2linear <- function(r, c, dims) {
  r + (c - 1) * dims[1]
}

create_bounded_a_points <- function(a1, a2, dim) {
  if ((a1 <= 0 && a2 <= 0) || (a1 > dim && a2 > dim)) {
    integer(0)
  } else {
    # Bound the points by the dimensions
    a1 <- max(min(a1, dim), 1)
    a2 <- max(min(a2, dim), 1)

    floor(a1):ceiling(a2)
  }
}

line_segment_points <- function(a1, a2, b1, b2, dims) {
  a_points <- create_bounded_a_points(a1, a2, dims[1])

  if (abs(a1 - a2) > 1e-5) {
    m <- (b1 - b2) / (a1 - a2)
    c <- b1 - m * a1
    b_points <- round(m * a_points + c)
    b_points[b_points > max(b2, b1)] <- max(b2, b1)
    b_points[b_points < min(b2, b1)] <- min(b2, b1)
  } else {
    a_points <- round(a2)
    b_points <- b2:b1
  }

  keep_b <- b_points > 0 & b_points <= dims[2]
  b_points <- b_points[keep_b]

  if (length(a_points) > 1) {
    a_points <- a_points[keep_b]
  } else if (!any(keep_b)) {
    a_points <- integer(0)
  }

  list(a = a_points, b = b_points)
}

create_arrow_bitmap <- function(tail_x, tail_y, head_x, head_y,
                                dims, xlim, ylim) {
  tail_x <- (tail_x - xlim[1]) / (xlim[2] - xlim[1]) * dims[1]
  head_x <- (head_x - xlim[1]) / (xlim[2] - xlim[1]) * dims[1]
  tail_y <- dims[2] - (tail_y - ylim[1]) / (ylim[2] - ylim[1]) * dims[2]
  head_y <- dims[2] - (head_y - ylim[1]) / (ylim[2] - ylim[1]) * dims[2]

  out <- line_segment_points(tail_x, head_x, tail_y, head_y, dims)

  linear_indices <- mapply(out$a, out$b,
                           FUN = function(x, y) cartesian2linear(x, y, dims))
  if (length(linear_indices) == 0) {
    linear_indices <- c() # otherwise it's a list
  }

  # Do it for y points too - helps for very steep lines, which get poor coverage
  # on y-points by my other method
  out <- line_segment_points(tail_y, head_y, tail_x, head_x, rev(dims))
  if (length(out$a) > 0) {
    linear_indices <-
          append(linear_indices,
                 mapply(
                   out$b,
                   out$a,
                   FUN = function(x, y) cartesian2linear(x, y, dims)
                 ))
  }

  linear_indices
}

los_mask_series_draw <- function(exclude, data, xlim, ylim, stacked,
                                 log_scale, joined, layout) {
  if (!is_empty(data)) {
    # overwrite the attributes
    for (i in seq_along(data$series)) {
      if (identical(legend_entry(data$series[[i]]), legend_entry(exclude))) {
        data$series[[i]]$attributes$col <- "white"
      } else {
        data$series[[i]]$attributes$col <- "black"
      }
      data$series[[i]]$attributes$barcol <- "white"
      data$series[[i]]$attributes$pch <- NA
    }
    drawbars(c(1, 1), data, xlim, ylim, stacked, log_scale, layout == "1h")
    drawlines(c(1, 1), data, xlim, ylim, joined, log_scale, layout == "1h")
  }
}

create_los_mask <- function(series, data, p, dims, xlim, ylim,
                            stacked, layout, log_scale, joined) {
  plot_device <- grDevices::dev.cur()
  grDevices::png(
    filename = paste0(tempdir(), "/autolabel-los-mask.png"),
    width = dims[1] / graphics::par("mfrow")[2],
    height = dims[2] / graphics::par("mfrow")[1],
    res = PNGDPI
  )
  graphics::par(
    family = font_family(),
    xaxs = "i",
    yaxs = "i",
    ps = 20,
    cex.main = (28 / 20),
    cex.axis = 1,
    las = 1,
    lheight = 1
  )
  graphics::par(omi = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
  graphics::plot(
    0,
    lwd = 0,
    pch = NA,
    axes = FALSE,
    xlab = "",
    ylab = "",
    xlim = c(0, 1),
    ylim = c(0, 1)
  )

  # draw the lines on
  los_mask_series_draw(series,
                       data[[p]],
                       xlim[[p]],
                       c(ylim[[p]]$min, ylim[[p]]$max),
                       stacked,
                       log_scale,
                       joined,
                       layout)

  # Do RHS axes if necessary
  if (!is.null(other_axes(p, layout))) {
    p <- other_axes(p, layout)
    los_mask_series_draw(series,
                         data[[p]],
                         xlim[[p]],
                         c(ylim[[p]]$min, ylim[[p]]$max),
                         stacked,
                         log_scale,
                         joined,
                         layout)
  }
  grDevices::dev.off()
  image <- magick::image_read(paste0(tempdir(), "/autolabel-los-mask.png"))
  image_map <- magick::image_data(image, "gray")
  grDevices::dev.set(plot_device)
  image_map <- drop(image_map)
  white_raw <- as.raw(255)

  image_map != white_raw
}

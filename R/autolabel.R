convert_image_non_white <- function(image) {
  t(drop(magick::image_data(image, "gray") != "ff"))
}

get_underlay_bitmap <- function(gg, margins) {
  plot_device <- grDevices::dev.cur()
  gg$enable_autolabeller <- FALSE
  agg_draw(gg, filename = paste0(tempdir(), "\\autolabel-temp.png"))
  image <- magick::image_read(paste0(tempdir(), "\\autolabel-temp.png"))
  file.remove(paste0(tempdir(), "\\autolabel-temp.png"))
  grDevices::dev.set(plot_device)

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
  return(image_map != white_raw)
}

create_text_bitmap <- function(x,y,text,xlim,ylim,layout,log_scale,dim) {
  x_start <- seq(from = xlim[1], to = xlim[2], length.out = dim[1] + 1)[1:dim[1]]
  y_start <- seq(from = ylim$max, to = ylim$min, length.out = dim[2] + 1)[2:(dim[2]+1)]

  top <- y + 0.5*strheight(text)
  bottom <- y - 0.5*strheight(text)
  left <- x - 0.5*strwidth(text)
  right <- x + 0.5*strwidth(text)

  x_indices <- x_start < right & (x_start + x_start[2]-x_start[1]) > left
  y_indices <- y_start < top & (y_start + y_start[2]-y_start[1]) > bottom

  list(x=x_indices,y=y_indices)
}

test_collision <- function(underlay, x_indices, y_indices) {
  any(underlay[x_indices, y_indices])
}

distanceininches <- function(x, y, a, b) {
  height <- (y - b) / (graphics::par("usr")[4] - graphics::par("usr")[3]) * graphics::par("pin")[2]
  width <- (x - a) / (graphics::par("usr")[2] - graphics::par("usr")[1]) * graphics::par("pin")[1]
  return(sqrt(height^2 + width^2))
}

linesegment.intersect <- function(x1, y1, x2, y2, a1, b1, a2, b2) {
  if (is.na(x1) || is.na(y1) || is.na(x2) || is.na(y2) || is.na(a1) || is.na(b1) || is.na(a2) || is.na(b2)) {
    return(FALSE)
  }
  if (max(x1, x2) < min(a1, a2) || min(x1, x2) > max(a1, a2) || max(y1, y2) < min(b1, b2) || min(y1, y2) > max(b1, b2)) {
    return(FALSE)
  } else {
    return(((a1-x1)*(y2-y1) - (b1-y1)*(x2-x1)) * ((a2-x1)*(y2-y1) - (b2-y1)*(x2-x1)) <= 0 && ((x1-a1)*(b2-b1) - (y1-b1)*(a2-a1)) * ((x2-a1)*(b2-b1) - (y2-b1)*(a2-a1)) <= 0)
  }
}


lineofsight.point2point <- function(x, y, a, b, block.x, block.y) {
  for (i in 1:(length(block.x)-1)) {
    if (linesegment.intersect(x, y, a, b, block.x[i], block.y[i], block.x[i+1], block.y[i+1])) {
      return(FALSE)
    }
  }
  return(TRUE)
}

lineofsight <- function(x, y, a, b, xvar, data, otherseries) {
  los <- TRUE
  for (s in otherseries) {
    block.y <- data[[s]]
    los <- los && lineofsight.point2point(x, y, a, b, xvar, block.y)
  }
  return(los)
}

pointlinedistance <- function(x, y, x1, y1, x2, y2) {
  if(is.na(x) || is.na(y) || is.na(x1) || is.na(y1) || is.na(x2) || is.na(y2)) {
    return(list(dist = Inf, x = NA, y = NA))
  } else {
    A <- x - x1
    B <- y - y1
    C <- x2 - x1
    D <- y2 - y1

    dot <- A * C + B * D
    len_sq <- C * C + D * D
    param <- -1
    if (len_sq != 0) {
      param <- dot / len_sq
    }

    if (param < 0) {
      xx <- x1
      yy <- y1
    } else if (param > 1) {
      xx <- x2
      yy <- y2
    } else {
      xx <- x1 + param * C
      yy <- y1 + param * D
    }
    return (list(dist = distanceininches(x, y, xx, yy), x = xx, y = yy))
  }
}

get_distance <- function(a, b, data, series.x, series.y, otherseries) {

  distance <- Inf

  for (i in 1:(length(series.x)-1)) {
    tmp <- pointlinedistance(a, b, series.x[i], series.y[i], series.x[i+1], series.y[i+1])
    if (tmp$dist < distance) {
      distance <- tmp$dist
      los <- lineofsight(tmp$x, tmp$y, a, b, series.x, data, otherseries)
    }
  }

  return(list(distance = distance, los = TRUE))
}

evaluate_candidate <- function(x, y, label, otherseries, data, xvals, yvals, xlim, ylim, p, layout, log_scale, underlay_bitmap) {
  indices <- create_text_bitmap(x,y,label,xlim,ylim,layout,log_scale,dim(underlay_bitmap))
  if (!test_collision(underlay_bitmap, indices$x, indices$y)) {
    return(get_distance(x, y, data, xvals, yvals, otherseries))
  } else {
    return(list(distance=Inf))
  }
}

create_sequence_steps <- function(x, y, angle, xlim, ylim) {
  # Angle is in half rotations
  if (cospi(angle) > 0) {
    x_seq <- seq(from = x, by = cospi(angle), to = xlim[2])
  } else if (cospi(angle) < 0) {
    x_seq <- seq(from = x, by = cospi(angle), to = xlim[1])
  } else {
    x_seq <- rep(x, 100) # big number to be safe
  }
  if (sinpi(angle) > 0) {
    y_seq <-  seq(from = y, by = sinpi(angle), to = ylim$max)
  } else if (sinpi(angle) < 0) {
    y_seq <-  seq(from = y, by = sinpi(angle), to = ylim$min)
  } else {
    y_seq <- rep(y, 100) # big number to be safe
  }

  if (length(x_seq) <= length(y_seq)) {
    return(cbind(unname(x_seq), unname(y_seq[1:length(x_seq)])))
  } else {
    return(cbind(unname(x_seq[1:length(y_seq)]), unname(y_seq)))
  }
}

candidate_from_anchor <- function(x_anchor, y_anchor, label, otherseries, data, xvals, yvals, xlim, ylim, p, layout, log_scale, underlay_bitmap) {
  cat(".")
  points_to_try <-
    unique(do.call(rbind, lapply(seq(from = 0, to = 1.75, by = 0.25), function(i)
      create_sequence_steps(x_anchor, y_anchor, i, xlim, ylim))))

  for (i in 1:nrow(points_to_try)) {
    x <- points_to_try[i,1]
    y <- points_to_try[i,2]
    distance <- evaluate_candidate(x, y, label, otherseries, data, xvals, yvals, xlim, ylim, p, layout, log_scale, underlay_bitmap)
    if (is.finite(distance$distance)) {
      return(data.frame(x=x,y=y,distance=distance$distance,los=distance$los))
    }
  }
  return(data.frame(x=Inf,y=Inf,distance=Inf,los=FALSE))
}

find_best_candidate <- function(label, plot_bitmap, x, y, otherseries, data, xlim, ylim, p, layout, log_scale, underlay_bitmap) {
  if (length(x) > 10) {
    step <- length(x) / 10
    points <-
      as.integer(seq(from = step / 2,
          by = step,
          length.out = 10))

    x_anchors <- x[points]
    y_anchors <- y[points]
  } else {
    x_anchors <- x
    y_anchors <- y
  }

  candidate_closure <-
    function(x_anchor, y_anchor)
      candidate_from_anchor(x_anchor, y_anchor, label, otherseries, data, x, y, xlim, ylim, p, layout, log_scale, underlay_bitmap)

  label_options <-
    mapply(candidate_closure,
           x = x_anchors,
           y = y_anchors,
           SIMPLIFY = FALSE)
  label_options <- do.call(rbind, label_options)

  best_fit <-
    dplyr::filter(label_options,
                  rank(label_options$distance, ties.method = "first") == 1)
  return(best_fit)
}

autolabel_series <- function(series, label, p, gg, panels, xlim, ylim, margins, labels, xvals, data, attributes) {
  plot_bitmap <- get_underlay_bitmap(gg, margins)
  cat(paste0("Finding location for ", series, " ."))
  found_location <-
    find_best_candidate(
      label,
      plot_bitmap,
      getxvals(data[[p]], !is.null(xvals[[paste0(p, "ts")]]), xvals[[p]]),
      data[[p]][[series]],
      panels[[p]][panels[[p]]!=series],
      data[[p]],
      xlim[[p]],
      ylim[[p]],
      p,
      gg$layout,
      gg$log_scale,
      plot_bitmap
    )
  cat("\n")
  if (is.finite(found_location$x)) {
    newlabel <- list(
      text = label,
      color = attributes[[p]]$col[[series]],
      x = found_location$x,
      y = found_location$y,
      panel = p
    )
    # Draw on the new label
    graphics::par(mfg = getlocation(p ,gg$layout))
    graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "",
                   xlim = xlim[[p]], ylim = c(ylim[[p]]$min, ylim[[p]]$max))
    drawlabel(newlabel)
    return(newlabel)
  } else {
    warning(paste0("Unable to find location for label for series ", series))
  }
}

autolabel <- function(gg, panels, xlim, ylim, margins, labels, xvals, data, attributes) {
  for (p in names(panels)) {
    if (notalreadylabelled(p, labels)) {
      labelsmap <- createlabels(data[[p]], panels, p, gg$layout)
      for (series in names(labelsmap)) {
        new_label <- autolabel_series(series, labelsmap[[series]], p, gg, panels, xlim, ylim, margins, labels, xvals, data, attributes)
        # Add it to the gg object, so that the mask includes it for the next series
        if (!is.null(new_label)) gg$labels <- append(gg$labels, list(new_label))
      }
    }
  }
}

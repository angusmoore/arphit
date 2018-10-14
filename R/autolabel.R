get_underlay_bitmap <- function(gg, margins) {
  plot_device <- grDevices::dev.cur()
  gg$enable_autolabeller <- FALSE
  agg_draw(gg, filename = paste0(tempdir(), "/autolabel-temp.png"))
  image <- magick::image_read(paste0(tempdir(), "/autolabel-temp.png"))
  suppressWarnings(file.remove(paste0(tempdir(), "/autolabel-temp.png")))
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

create_text_bitmap <- function(x,y,text,xlim,ylim,dim) {
  x_start <- seq(from = xlim[1], to = xlim[2], length.out = dim[1] + 1)[1:dim[1]]
  y_start <- seq(from = ylim$max, to = ylim$min, length.out = dim[2] + 1)[2:(dim[2]+1)]

  top <- y + 0.5*strheight(text) + AUTOLABEL_PADDING / par("pin")[2]*(ylim$max-ylim$min)
  bottom <- y - 0.5*strheight(text) - AUTOLABEL_PADDING / par("pin")[2]*(ylim$max-ylim$min)
  left <- x - 0.5*strwidth(text) - AUTOLABEL_PADDING / par("pin")[1]*(xlim[1]-xlim[1])
  right <- x + 0.5*strwidth(text) + AUTOLABEL_PADDING / par("pin")[1]*(xlim[2]-xlim[1])

  x_indices <- x_start < right & (x_start + x_start[2]-x_start[1]) > left
  y_indices <- y_start < top & (y_start + y_start[2]-y_start[1]) > bottom

  list(x=x_indices,y=y_indices)
}

test_collision <- function(underlay, x_indices, y_indices) {
  any(underlay[x_indices, y_indices])
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

point_point_distance <- function(x, y, series.x, series.y) {
  y_inches_conversion <- 1 / (graphics::par("usr")[4] - graphics::par("usr")[3]) * graphics::par("pin")[2]
  x_inches_conversion <- 1 / (graphics::par("usr")[2] - graphics::par("usr")[1]) * graphics::par("pin")[1]

  distance <- tibble::tibble(xx=series.x,yy=series.y)
  distance$distance <- sqrt(((distance$xx-x)*x_inches_conversion)^2+((distance$yy-y)*y_inches_conversion)^2)

  distance[rank(distance$distance,ties.method="first")==1,]
}

point_line_distance <- function(x, y, series.x, series.y) {
  y_inches_conversion <- 1 / (graphics::par("usr")[4] - graphics::par("usr")[3]) * graphics::par("pin")[2]
  x_inches_conversion <- 1 / (graphics::par("usr")[2] - graphics::par("usr")[1]) * graphics::par("pin")[1]

  distance <- tibble::tibble(x1=series.x[1:(length(series.x)-1)],y1=series.y[1:(length(series.x)-1)],
         x2=series.x[2:length(series.x)],y2=series.y[2:length(series.x)])
  distance$dot <- (x-distance$x1)*(distance$x2-distance$x1) + (y-distance$y1)*(distance$y2-distance$y1)
  distance$len_sq <- (distance$x2-distance$x1)^2 + (distance$y2-distance$y1)^2
  distance$param <- dplyr::if_else(distance$len_sq == 0, -1, distance$dot / distance$len_sq)

  distance$xx <- dplyr::case_when(distance$param < 0 ~ distance$x1,
                                  distance$param > 0 ~ distance$x2,
                                  TRUE ~ distance$x1 + distance$param*(distance$x2-distance$x1))

  distance$yy <- dplyr::case_when(distance$param < 0 ~ distance$y1,
                                  distance$param > 0 ~ distance$y2,
                                  TRUE ~ distance$y1 + distance$param*(distance$y2-distance$y1))

  distance$distance <- sqrt(((distance$xx-x)*x_inches_conversion)^2+((distance$yy-y)*y_inches_conversion)^2)

  distance <- distance[c("xx", "yy", "distance")]
  distance[rank(distance$distance,ties.method="first")==1,]
}

get_distance_series_type <- function(x, y, series.x, series.y, series_type) {
  if (series_type == "line") {
    return(point_line_distance(x, y, series.x, series.y))
  } else if (series_type == "point") {
    return(point_point_distance(x, y, series.x, series.y))
  } else if (series_type == "bar") {
    stop("Bar distance not implemented")
  } else {
    stop("Unknown series type.")
  }
}

get_distance <- function(a, b, data, series.x, series.y, thisseries, otherseries, series_types) {
  result <- get_distance_series_type(a,b,series.x,series.y, series_types[[thisseries]])
  los <- lineofsight(result$xx, result$yy, a, b, series.x, data, otherseries)
  next_closest <- sapply(otherseries, function(series) get_distance_series_type(a,b,series.x,data[[series]],series_types[[series]])$distance)

  return(list(distance = result$distance, los = los, next_closest = min(next_closest)))
}

evaluate_candidate <- function(x, y, label, series, otherseries, series_types, data, xvals, yvals, xlim, ylim, p, underlay_bitmap) {
  indices <- create_text_bitmap(x,y,label,xlim,ylim,dim(underlay_bitmap))
  if (!test_collision(underlay_bitmap, indices$x, indices$y)) {
    distance <- get_distance(x, y, data, xvals, yvals, series, otherseries, series_types)
    return(data.frame(x=x,y=y,distance=distance$distance,los=distance$los,next_closest=distance$next_closest))
  } else {
    return(data.frame(x=x,y=y,distance=Inf,los=FALSE,next_closest=NA))
  }
}

candidate_from_x_anchor <- function(x, label, series, otherseries, series_types, data, xvals, yvals, xlim, ylim, p, log_scale, underlay_bitmap) {
  cat(".")
  step <- (ylim$max - ylim$min)/AUTOLABEL_YSTEPS
  points_to_try <- seq(from = ylim$min+step, by = step, length.out = AUTOLABEL_YSTEPS-1)

  candidates <- lapply(points_to_try, function(y) evaluate_candidate(x, y, label, series, otherseries, series_types, data, xvals, yvals, xlim, ylim, p,  underlay_bitmap))

  do.call(rbind, candidates)
}

find_best_candidate <- function(label, plot_bitmap, x, y, series, otherseries, series_types, data, xlim, ylim, p, log_scale, underlay_bitmap) {

  step <- (xlim[2] - xlim[1])/AUTOLABEL_XSTEPS
  x_anchors <- seq(from = xlim[1] + 1.5*step, by = step, length.out = AUTOLABEL_XSTEPS-2)

  candidate_closure <-
    function(x_anchor)
      candidate_from_x_anchor(x_anchor, label, series, otherseries, series_types, data, x, y, xlim, ylim, p, log_scale, underlay_bitmap)

  label_options <-
    lapply(FUN = candidate_closure,
           X = x_anchors)
  label_options <- do.call(rbind, label_options)

  best_fit <-
    dplyr::filter(label_options,
                  rank(label_options$distance, ties.method = "first") == 1)
  return(best_fit)
}

get_series_type <- function(series, bars, lty) {
  if (series %in% bars) {
    return("bar")
  } else if (lty[[series]] == 0) {
    return("point")
  } else {
    return("line")
  }
}

get_series_types <- function(series_list, attributes, bars) {
  series_types <- lapply(series_list, function(x) get_series_type(x, bars, attributes$lty))
  names(series_types) <- series_list
  return(series_types)
}

autolabel_series <- function(series, label, p, plot_bitmap, panels, xlim, ylim, margins, labels, xvals, data, attributes, bars, layout, log_scale) {
  series_types <- get_series_types(panels[[p]], attributes[[p]], bars)
  cat(paste0("Finding location for ", series, " ."))
  found_location <-
    find_best_candidate(
      label,
      plot_bitmap,
      getxvals(data[[p]], !is.null(xvals[[paste0(p, "ts")]]), xvals[[p]]),
      data[[p]][[series]],
      series,
      panels[[p]][panels[[p]]!=series],
      series_types,
      data[[p]],
      xlim[[p]],
      ylim[[p]],
      p,
      log_scale,
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
    graphics::par(mfg = getlocation(p, layout))
    graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "",
                   xlim = xlim[[p]], ylim = c(ylim[[p]]$min, ylim[[p]]$max))
    drawlabel(newlabel)
    return(newlabel)
  } else {
    warning(paste0("Unable to find location for label for series ", series))
    return(NULL)
  }
}

autolabel <- function(gg, panels, xlim, ylim, margins, labels, xvals, data, attributes, bars) {
  if (any(sapply(names(panels), function(p) notalreadylabelled(p, labels)))) {
    plot_bitmap <- get_underlay_bitmap(gg, margins)
  }
  for (p in names(panels)) {
    if (notalreadylabelled(p, labels)) {
      labelsmap <- createlabels(data[[p]], panels, p, gg$layout)
      for (series in names(labelsmap)) {
        new_label <- autolabel_series(series, labelsmap[[series]], p, plot_bitmap, panels, xlim, ylim, margins, labels, xvals, data, attributes, bars, gg$layout, gg$log_scale)
        # Add it to the gg object, so that the mask includes it for the next series
        if (!is.null(new_label)) {
          # Mask out the new label in the bitmap
          indices <- create_text_bitmap(new_label$x,new_label$y,new_label$text,xlim[[p]],ylim[[p]],dim(plot_bitmap))
          plot_bitmap[indices$x, indices$y] <- TRUE
        }
      }
    }
  }
}

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

create_text_bitmap <- function(x,y,text,xlim,ylim,dim,layout,p,padding = AUTOLABEL_PADDING) {
  x_scale <- graphics::par("mfrow")[2]
  x_shift <- round(dim[1]/x_scale) * (getlocation(p, layout)[2] - 1)
  y_scale <- graphics::par("mfrow")[1]
  y_shift <- round(dim[2]/x_scale) * (getlocation(p, layout)[1] - 1)

  x_start <- seq(from = xlim[1], to = xlim[2], length.out = round(dim[1]/x_scale) + 1)[1:round(dim[1]/x_scale)]
  y_start <- seq(from = ylim$max, to = ylim$min, length.out = round(dim[2]/y_scale) + 1)[2:(round(dim[2]/y_scale)+1)]

  top <- y + 0.5*graphics::strheight(text) + padding / grDevices::dev.size()[2]*(ylim$max-ylim$min)
  bottom <- y - 0.5*graphics::strheight(text) - padding / grDevices::dev.size()[2]*(ylim$max-ylim$min)
  left <- x - 0.5*graphics::strwidth(text) - padding / grDevices::dev.size()[1]*(xlim[2]-xlim[1])
  right <- x + 0.5*graphics::strwidth(text) + padding / grDevices::dev.size()[1]*(xlim[2]-xlim[1])

  x_indices <- which(x_start < right & (x_start + x_start[2]-x_start[1]) > left) + x_shift
  y_indices <- which(y_start < top & (y_start + y_start[2]-y_start[1]) > bottom) + y_shift


  list(x=x_indices,y=y_indices)
}

test_collision <- function(underlay, x_indices, y_indices) {
  any(underlay[x_indices, y_indices])
}

lineofsight <- function(x, y, a, b, xvar, data, otherseries) {
  los <- TRUE
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

  if (length(otherseries) == 0) {
    # TODO: Find a way of measuring distance to series on RHS panels
    next_closest <- Inf
  }
  return(list(distance = result$distance, los = los, next_closest = min(next_closest)))
}

evaluate_candidate <- function(x, y, label, series, otherseries, series_types, data, xvals, yvals, xlim, ylim, layout, p, underlay_bitmap) {
  indices <- create_text_bitmap(x,y,label,xlim,ylim,dim(underlay_bitmap),layout,p)
  if (!test_collision(underlay_bitmap, indices$x, indices$y)) {
    distance <- get_distance(x, y, data, xvals, yvals, series, otherseries, series_types)
    return(data.frame(x=x,y=y,distance=distance$distance,los=distance$los,next_closest=distance$next_closest))
  } else {
    return(data.frame(x=x,y=y,distance=Inf,los=FALSE,next_closest=NA))
  }
}

candidate_from_x_anchor <- function(x, label, series, otherseries, series_types, data, xvals, yvals, xlim, ylim, layout, p, log_scale, underlay_bitmap) {
  cat(".")
  step <- (ylim$max - ylim$min)/AUTOLABEL_YSTEPS
  points_to_try <- seq(from = ylim$min+step, by = step, length.out = AUTOLABEL_YSTEPS-1)

  candidates <- lapply(points_to_try, function(y) evaluate_candidate(x, y, label, series, otherseries, series_types, data, xvals, yvals, xlim, ylim, layout, p,  underlay_bitmap))

  do.call(rbind, candidates)
}

label_selection <- function(label_options) {
  if (any(is.finite(label_options$distance))) {
    return(dplyr::filter(label_options,
                  rank(label_options$distance, ties.method = "first") == 1))
  } else {
    return(NULL)
  }
}

find_best_candidate <- function(label, plot_bitmap, x, y, series, otherseries, series_types, data, xlim, ylim, layout, p, log_scale, underlay_bitmap) {

  step <- (xlim[2] - xlim[1])/AUTOLABEL_XSTEPS
  x_anchors <- seq(from = xlim[1] + 1.5*step, by = step, length.out = AUTOLABEL_XSTEPS-2)

  candidate_closure <-
    function(x_anchor)
      candidate_from_x_anchor(x_anchor, label, series, otherseries, series_types, data, x, y, xlim, ylim, layout, p, log_scale, underlay_bitmap)

  label_options <-
    lapply(FUN = candidate_closure,
           X = x_anchors)
  label_options <- do.call(rbind, label_options)

  return(label_selection(label_options))
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

autolabel_fallback <- function(label, xlim, ylim, underlay_bitmap, layout, p) {
  x_up <- seq(from = mean(xlim), to = max(xlim), length.out = AUTOLABEL_FALLBACK_STEPS)
  x_down <- seq(from = mean(xlim), to = min(xlim), length.out = AUTOLABEL_FALLBACK_STEPS)
  y_up <- seq(from = mean(c(ylim$min,ylim$max)), to = ylim$max, length.out = AUTOLABEL_FALLBACK_STEPS)
  y_down <- seq(from = mean(c(ylim$min,ylim$max)), to = ylim$min, length.out = AUTOLABEL_FALLBACK_STEPS)

  x_steps <- c(rbind(x_up,x_down))
  y_steps <- c(rbind(y_up,y_down))

  for (x in x_steps) {
    cat("x")
    for (y in y_steps) {
      indices <- create_text_bitmap(x,y,label,xlim,ylim,dim(underlay_bitmap),layout,p,padding=AUTOLABEL_FALLBACK_PADDING)
      if (!test_collision(underlay_bitmap, indices$x, indices$y)) {
        return(data.frame(x=x,y=y,distance=0,los=FALSE,next_closest=Inf))
      }
    }
  }
  return(NULL)
}

autolabel_series <- function(series, label, otherseries, p, plot_bitmap, panels, xlim, ylim, margins, labels, xvals, data, attributes, bars, layout, log_scale) {
  series_types <- get_series_types(panels[[p]], attributes[[p]], bars)
  cat(paste0("Finding location for ", series, " ."))
  found_location <-
    find_best_candidate(
      label,
      plot_bitmap,
      getxvals(data[[p]], !is.null(xvals[[paste0(p, "ts")]]), xvals[[p]]),
      data[[p]][[series]],
      series,
      otherseries,
      series_types,
      data[[p]],
      xlim[[p]],
      ylim[[p]],
      layout,
      p,
      log_scale,
      plot_bitmap
    )
  if (is.null(found_location)) {
    found_location <- autolabel_fallback(label, xlim[[p]], ylim[[p]], plot_bitmap, layout, p)
  }
  cat("\n")
  if (!is.null(found_location)) {
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
  out <- createlabels(panels, bars, attributes, gg$layout)
  labelsmap <- out$series_to_labels
  labels_panels <- out$series_to_panels
  for (p in names(panels)) {
    for (series in names(labelsmap)) {
        if (p %in% labels_panels[[series]]) {
        new_label <-
          autolabel_series(
            series,
            labelsmap[[series]],
            panels[[p]][panels[[p]] != series],
            p,
            plot_bitmap,
            panels,
            xlim,
            ylim,
            margins,
            labels,
            xvals,
            data,
            attributes,
            bars,
            gg$layout,
            gg$log_scale
          )
        # Add it to the gg object, so that the mask includes it for the next series
        if (!is.null(new_label)) {
          # Mask out the new label in the bitmap
          indices <- create_text_bitmap(new_label$x,new_label$y,new_label$text,xlim[[p]],ylim[[p]],dim(plot_bitmap),gg$layout,p)
          plot_bitmap[indices$x, indices$y] <- TRUE
          # And remove it from the list
          labels_panels[[series]] <- NULL
        }
      }
    }
  }
}

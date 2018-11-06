evaluate_candidate <- function(x, y, label, series, otherseries, series_types, data, xvals, yvals, xlim, ylim, layout, p, underlay_bitmap, bars, bars.stacked) {
  indices <- create_text_bitmap(x,y,label,xlim,ylim,dim(underlay_bitmap),layout,p)
  if (!test_collision(underlay_bitmap, indices$x, indices$y)) {
    distance <- get_distance(x, y, data, xvals, yvals, series, otherseries, series_types, bars, bars.stacked)
    return(data.frame(x=x,y=y,distance=distance$distance,los=distance$los,next_closest=distance$next_closest))
  } else {
    return(data.frame(x=x,y=y,distance=Inf,los=FALSE,next_closest=NA))
  }
}

candidate_from_x_anchor <- function(x, label, series, otherseries, series_types, data, xvals, yvals, xlim, ylim, layout, p, log_scale, underlay_bitmap, bars, bars.stacked) {
  cat(".")
  step <- (ylim$max - ylim$min)/AUTOLABEL_YSTEPS
  points_to_try <- seq(from = ylim$min+step, by = step, length.out = AUTOLABEL_YSTEPS-1)

  candidates <- lapply(points_to_try, function(y) evaluate_candidate(x, y, label, series, otherseries, series_types, data, xvals, yvals, xlim, ylim, layout, p,  underlay_bitmap, bars, bars.stacked))

  do.call(rbind, candidates)
}

find_candidates <- function(label, plot_bitmap, x, y, series, otherseries, series_types, data, xlim, ylim, layout, p, log_scale, underlay_bitmap, bars, bars.stacked) {

  step <- (xlim[2] - xlim[1])/AUTOLABEL_XSTEPS
  x_anchors <- seq(from = xlim[1] + 1.5*step, by = step, length.out = AUTOLABEL_XSTEPS-2)

  candidate_closure <-
    function(x_anchor)
      candidate_from_x_anchor(x_anchor, label, series, otherseries, series_types, data, x, y, xlim, ylim, layout, p, log_scale, underlay_bitmap, bars, bars.stacked)

  label_options <-
    lapply(FUN = candidate_closure,
           X = x_anchors)
  label_options <- do.call(rbind, label_options)

  return(label_selection(label_options))
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

autolabel_series <- function(series, label, otherseries, p, plot_bitmap, panels, xlim, ylim, margins, labels, xvals, data, attributes, bars, layout, log_scale, bars.stacked) {
  series_types <- get_series_types(panels[[p]], attributes[[p]], bars)
  cat(paste0("Finding location for ", series, " ."))
  found_location <-
    find_candidates(
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
      plot_bitmap,
      bars,
      bars.stacked
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
  out <- createlabels(panels, bars, attributes, gg$layout, labels)
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
            bars[[p]],
            gg$layout,
            gg$log_scale,
            gg$stacked
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

evaluate_candidate <- function(x, y, label, series, otherseries, series_types, data, xvals, yvals, xlim, ylim, layout, p, underlay_bitmap, los_mask, bars, bars.stacked) {
  indices <- create_text_bitmap(x,y,label,xlim,ylim,dim(underlay_bitmap),layout,p)
  if (!test_collision(underlay_bitmap, indices$x, indices$y)) {
    distance <- get_distance(x, y, data, xvals, yvals, series, otherseries, series_types, bars, bars.stacked, los_mask, xlim, ylim)
    result <- data.frame(x=x,y=y,distance=distance$distance,los=distance$los,next_closest=distance$next_closest,xx=distance$xx,yy=distance$yy)
    return(assign_selection_groups(result))
  } else {
    return(data.frame(x=x,y=y,distance=Inf,los=FALSE,next_closest=NA,xx=NA,yy=NA,selection_group=10000))
  }
}

candidate_from_x_anchor <- function(x, label, series, otherseries, series_types, data, xvals, yvals, xlim, ylim, layout, p, log_scale, underlay_bitmap, los_mask, bars, bars.stacked, quiet) {
  if (!quiet) cat(".")
  step <- (ylim$max - ylim$min)/AUTOLABEL_YSTEPS
  points_to_try <- seq(from = ylim$min+step, by = step, length.out = AUTOLABEL_YSTEPS-1)

  candidates <- lapply(points_to_try, function(y) evaluate_candidate(x, y, label, series, otherseries, series_types, data, xvals, yvals, xlim, ylim, layout, p,  underlay_bitmap, los_mask, bars, bars.stacked))

  do.call(rbind, candidates)
}

find_candidates <- function(label, plot_bitmap, x, y, series, otherseries, series_types, data, xlim, ylim, layout, p, log_scale, underlay_bitmap, los_mask, bars, bars.stacked, quiet) {

  step <- (xlim[2] - xlim[1])/AUTOLABEL_XSTEPS
  x_anchors <- seq(from = xlim[1] + 1.5*step, by = step, length.out = AUTOLABEL_XSTEPS-2)

  candidate_closure <-
    function(x_anchor)
      candidate_from_x_anchor(x_anchor, label, series, otherseries, series_types, data, x, y, xlim, ylim, layout, p, log_scale, underlay_bitmap, los_mask, bars, bars.stacked, quiet)

  label_options <-
    lapply(FUN = candidate_closure,
           X = x_anchors)
  label_options <- do.call(rbind, label_options)
  label_options <- label_options[is.finite(label_options$distance),]
  return(label_selection(label_options))
}

autolabel_fallback <- function(label, xlim, ylim, underlay_bitmap, layout, p, quiet) {
  x_up <- seq(from = mean(xlim), to = max(xlim), length.out = AUTOLABEL_FALLBACK_STEPS)
  x_down <- seq(from = mean(xlim), to = min(xlim), length.out = AUTOLABEL_FALLBACK_STEPS)
  y_up <- seq(from = mean(c(ylim$min,ylim$max)), to = ylim$max, length.out = AUTOLABEL_FALLBACK_STEPS)
  y_down <- seq(from = mean(c(ylim$min,ylim$max)), to = ylim$min, length.out = AUTOLABEL_FALLBACK_STEPS)

  x_steps <- c(rbind(x_up,x_down))
  y_steps <- c(rbind(y_up,y_down))

  for (x in x_steps) {
    if (!quiet) cat("x")
    for (y in y_steps) {
      indices <- create_text_bitmap(x,y,label,xlim,ylim,dim(underlay_bitmap),layout,p,padding=AUTOLABEL_FALLBACK_PADDING)
      if (!test_collision(underlay_bitmap, indices$x, indices$y)) {
        return(data.frame(x=x,y=y,distance=0,los=TRUE,next_closest=Inf))
      }
    }
  }
  return(NULL)
}

autolabel_series <- function(series, label, otherseries, p, plot_bitmap, los_mask, panels, xlim, ylim, margins, labels, xvals, data, attributes, bars, layout, log_scale, bars.stacked, quiet) {
  series_types <- get_series_types(panels[[p]], attributes[[p]], bars)
  if (!quiet) cat(paste0("Finding location for ", series, " ."))
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
      los_mask,
      bars,
      bars.stacked,
      quiet
    )
  if (is.null(found_location)) {
    found_location <- autolabel_fallback(label, xlim[[p]], ylim[[p]], plot_bitmap, layout, p, quiet)
  }
  if (!quiet) cat("\n")
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
    return(list(label=newlabel,arrow=add_arrow(found_location, attributes[[p]]$col[[series]])))
  } else {
    warning(paste0("Unable to find location for label for series ", series))
    return(NULL)
  }
}

autolabel <- function(gg, panels, xlim, ylim, margins, labels, xvals, data, attributes, bars, quiet) {
  if (any(sapply(names(panels), function(p) notalreadylabelled(p, labels)))) {
    plot_bitmap <- get_underlay_bitmap(gg, margins)
  }
  out <- createlabels(panels, bars, attributes, gg$layout, labels)
  labelsmap <- out$series_to_labels
  labels_panels <- out$series_to_panels
  for (p in names(panels)) {
    for (series in names(labelsmap)) {
      if (p %in% labels_panels[[series]]) {
        # Create LOS mask
        los_mask <-
          create_los_mask(
            series,
            panels,
            p,
            data,
            xvals,
            dim(plot_bitmap),
            xlim,
            ylim,
            bars,
            gg$stacked,
            gg$layout,
            gg$log_scale,
            gg$joined
          )
        # Find a label
        new_label <-
          autolabel_series(
            series,
            labelsmap[[series]],
            panels[[p]][panels[[p]] != series],
            p,
            plot_bitmap,
            los_mask,
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
            gg$stacked,
            quiet
          )
        # Add it to the mask, so that the mask includes it for the next series
        if (!is.null(new_label$label)) {
          # Mask out the new label in the bitmap
          indices <-
            create_text_bitmap(
              new_label$label$x,
              new_label$label$y,
              new_label$label$text,
              xlim[[p]],
              ylim[[p]],
              dim(plot_bitmap),
              gg$layout,
              p
            )
          plot_bitmap[indices$x, indices$y] <- TRUE
          # And remove it from the list
          labels_panels[[series]] <- NULL
          if (!is.null(new_label$arrow)) {
            # Mask out the new label in the bitmap too if needed
            arrow_mask <-
              create_arrow_bitmap(
                new_label$arrow$tail.x,
                new_label$arrow$tail.y,
                new_label$arrow$head.x,
                new_label$arrow$head.y,
                dim(plot_bitmap),
                gg$layout,
                p,
                xlim[[p]],
                ylim[[p]]
              )
            plot_bitmap <- arrow_mask | plot_bitmap
          }
        }
      }
    }
  }
}

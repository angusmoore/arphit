autolabel_series_type <- function (series) {
  if (series$geomtype == "line" && series$attributes$lty == 0) {
    "point"
  } else {
    series$geomtype
  }
}

evaluate_candidate <- function(x, y, text_indices, x_text_anchor, y_text_anchor, x_scale, y_scale, series, data, xlim, ylim, layout, p, underlay_bitmap, los_mask, bars.stacked, inches_conversion) {

  dim <- dim(underlay_bitmap)
  indices <- shift_text_indices(text_indices,x,y,x_text_anchor,y_text_anchor,x_scale,y_scale,xlim,ylim,dim,layout,p)

  if (!test_collision(underlay_bitmap, indices$x, indices$y, dim)) {

    series_distances <- lapply(seq_along(data$series),
                               function(s)
                                 get_distance_series_type(
                                   x,
                                   y,
                                   get_x_plot_locations(series_x_values(data, s), data),
                                   series_values(data, s),
                                   autolabel_series_type(data$series[[s]]),
                                   data,
                                   bars.stacked,
                                   inches_conversion
                                 ))

    series_index <- which(sapply(data$series, function(x) identical(x, series)))

    distance <- series_distances[[series_index]]
    distance$los <- lineofsight(distance$xx, distance$yy, x, y, los_mask, xlim, ylim)
    series_distances[[series_index]] <- NULL
    next_closest <- which.min(sapply(series_distances, function(s) s$distance))

    distance$x <- x
    distance$y <- y
    if (length(next_closest) == 1) {
      distance$next_closest <- series_distances[[next_closest]]$distance
    } else {
      distance$next_closest <- Inf # Happens for panels with LHS/RHS axes if there
      # is only one series on one of the axes, as we aren't measuring
      # distance to those series on the other axis, but we are labelling, since
      # there are two series in the panel
    }

    distance$selection_group <- assign_selection_group(distance$distance, distance$next_closest, distance$los)
    return(distance)
  } else {
    return(NULL)
  }
}

divide_y_axis <- function(ylim, divisor, axis_points, axis_step, remove_axis) {
    new_points <- seq(from = ylim[1] + axis_step/divisor, by = axis_step/divisor, to = ylim[2] - axis_step/divisor)
    if (remove_axis) {
      return(setdiff(new_points, axis_points))
    } else {
      return(new_points)
    }
}

autolabeller_y_points <- function(ylim, ylim_nsteps, has_linebreak) {
  axis_step <- (ylim[2] - ylim[1])/(ylim_nsteps-1)
  axis_points <- seq(from = ylim[1] + axis_step, by = axis_step, to = ylim[2] - axis_step)

  for (divisor in seq(from = 2, by = 2, to = AUTOLABEL_YSTEPS)) {
    candidate_points <- divide_y_axis(ylim, divisor, axis_points, axis_step, !has_linebreak)
    if (length(candidate_points) > AUTOLABEL_YSTEPS) return(candidate_points)
  }
}

candidate_from_x_anchor <- function(x, label_indices, x_text_anchor, y_text_anchor, x_scale, y_scale, series, data, xlim, ylim, ylim_nsteps, layout, p, log_scale, underlay_bitmap, los_mask, bars.stacked, quiet, inches_conversion, has_linebreak) {
  if (!quiet) cat(".")

  points_to_try <- autolabeller_y_points(ylim, ylim_nsteps, has_linebreak)

  candidates <-
    dplyr::bind_rows(lapply(points_to_try, function(y)
      evaluate_candidate(
        x,
        y,
        label_indices,
        x_text_anchor,
        y_text_anchor,
        x_scale,
        y_scale,
        series,
        data,
        xlim,
        ylim,
        layout,
        p,
        underlay_bitmap,
        los_mask,
        bars.stacked,
        inches_conversion
      )))

  return(candidates)
}

find_candidates <- function(label, plot_bitmap, series, data, xlim, ylim, ylim_nsteps, layout, p, log_scale, underlay_bitmap, los_mask, bars.stacked, quiet, inches_conversion) {
  step <- (xlim[2] - xlim[1])/AUTOLABEL_XSTEPS
  x_anchors <- seq(from = xlim[1] + 1.5*step, by = step, length.out = AUTOLABEL_XSTEPS-2)

  x_text_anchor <- mean(xlim)
  y_text_anchor <- mean(ylim)
  x_scale <- graphics::par("mfrow")[2]
  y_scale <- graphics::par("mfrow")[1]
  text_indices <- create_text_bitmap(x_text_anchor,y_text_anchor,label$label,xlim,ylim,dim(underlay_bitmap),layout,p)

  label_options <- list()
  for (x_anchor in x_anchors) {
    label_options <-
      append(label_options,
             list(
               candidate_from_x_anchor(
                 x_anchor,
                 text_indices,
                 x_text_anchor,
                 y_text_anchor,
                 x_scale,
                 y_scale,
                 series,
                 data,
                 xlim,
                 ylim,
                 ylim_nsteps,
                 layout,
                 p,
                 log_scale,
                 underlay_bitmap,
                 los_mask,
                 bars.stacked,
                 quiet,
                 inches_conversion,
                 stringr::str_detect(label$label,stringr::fixed("\n"))
               )
             ))
  }
  label_options <- dplyr::bind_rows(label_options)
  if (nrow(label_options) > 0) {
    label_options <- label_options[is.finite(label_options$distance),]
    return(label_selection(label_options))
  } else {
    return(NULL)
  }
}

autolabel_fallback <- function(label, xlim, ylim, underlay_bitmap, layout, p, quiet) {
  x_up <- seq(from = mean(xlim), to = max(xlim), length.out = AUTOLABEL_FALLBACK_STEPS)
  x_down <- seq(from = mean(xlim), to = min(xlim), length.out = AUTOLABEL_FALLBACK_STEPS)
  y_up <- seq(from = mean(ylim), to = max(ylim), length.out = AUTOLABEL_FALLBACK_STEPS)
  y_down <- seq(from = mean(ylim), to = min(ylim), length.out = AUTOLABEL_FALLBACK_STEPS)

  x_steps <- c(rbind(x_up,x_down)) # This interleaves the numbers
  y_steps <- c(rbind(y_up,y_down))

  dim <- dim(underlay_bitmap)
  for (x in x_steps) {
    if (!quiet) cat("x")
    for (y in y_steps) {
      indices <- create_text_bitmap(x,y,label,xlim,ylim,dim,layout,p,padding=AUTOLABEL_FALLBACK_PADDING)
      if (!test_collision(underlay_bitmap, indices$x, indices$y, dim)) {
        return(data.frame(x=x,y=y,distance=0,los=TRUE,next_closest=Inf))
      }
    }
  }
  return(NULL)
}

autolabel_series <- function(label, series, p, data, plot_bitmap, los_mask, xlim, ylim, margins, layout, log_scale, bars.stacked, quiet, inches_conversion, arrow_lines, arrow_bars) {
  if (!quiet) cat(paste0("Finding location for ", stringr::str_replace(label$name, stringr::fixed("\n"), " "), " ."))

  found_location <-
    find_candidates(
      label,
      plot_bitmap,
      series,
      data[[p]],
      xlim[[p]],
      c(ylim[[p]]$min, ylim[[p]]$max),
      ylim[[p]]$nsteps,
      layout,
      p,
      log_scale,
      plot_bitmap,
      los_mask,
      bars.stacked,
      quiet,
      inches_conversion
    )
  if (is.null(found_location)) {
    found_location <- autolabel_fallback(label$label, xlim[[p]], c(ylim[[p]]$min, ylim[[p]]$max),
                                         plot_bitmap, layout, p, quiet)
  }
  if (!quiet) cat("\n")
  if (!is.null(found_location)) {
    newlabel <- list(
      text = label$label,
      colour = label$series$attributes$col,
      x = found_location$x,
      y = found_location$y,
      panel = p
    )
    # Draw on the new label
    graphics::par(mfg = getlocation(p, layout))
    graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "",
                   xlim = xlim[[p]], ylim = c(ylim[[p]]$min, ylim[[p]]$max))
    drawlabel(newlabel)
    if ((series$geomtype != "bar" && series$geomtype != "waterfall") && arrow_lines) {
      return(list(label=newlabel,arrow=add_arrow(found_location, newlabel$text, label$col, p, inches_conversion)))
    } else if ((series$geomtype == "bar" || series$geomtype == "waterfall") && arrow_bars ) {
      return(list(label=newlabel,arrow=add_arrow(found_location, newlabel$text, label$fill, p, inches_conversion)))
    } else {
      return(list(label=newlabel))
    }
  } else {
    warning(paste0("Unable to find location for label for series ",
                   stringr::str_replace(label$name, stringr::fixed("\n"), " ")))
    return(NULL)
  }
}

autolabel <- function(gg, data, xlim, ylim, margins, labels, quiet, arrow_lines, arrow_bars, ignore_existing_labels) {
  inches_conversion <-
    list(
      y = 1 / (graphics::par("usr")[4] - graphics::par("usr")[3]) * graphics::par("pin")[2],
      x = 1 / (graphics::par("usr")[2] - graphics::par("usr")[1]) * graphics::par("pin")[1]
    )
  series_to_label <- createlabels(data, gg$layout, labels, ylim, ignore_existing_labels)

  if (length(series_to_label) > 0) {
    plot_bitmap <- get_underlay_bitmap(gg, margins)
  }

  for (label in series_to_label) {
    for (p in label$panels) {
      # Create LOS mask
      los_mask <- create_los_mask(label$series,data,p,dim(plot_bitmap),xlim,
                                  ylim,gg$stacked, gg$layout,gg$log_scale,gg$joined)
      # Find a label
      new_label <-
        autolabel_series(
          label,
          label$series,
          p,
          data,
          plot_bitmap,
          los_mask,
          xlim,
          ylim,
          margins,
          gg$layout,
          gg$log_scale,
          gg$stacked,
          quiet,
          inches_conversion,
          arrow_lines,
          arrow_bars
        )
      # Add it to the mask, so that the mask includes it for the next series
      if (!is.null(new_label$label)) {
        plot_bitmap <- add_new_label_to_mask(plot_bitmap, new_label, c(ylim[[p]]$min, ylim[[p]]$max), xlim[[p]], p, gg$layout)
        break # no need to continue looping over possible panels
      }
    }
  }
}

getlocation <- function(p, layout) {
  if (layout == "1") {
    l <- c(1,1)
  } else if (layout == "2h") {
    l <- switch(p,
                "1" = c(1,1),
                "2" = c(1,1),
                "3" = c(2,1),
                "4" = c(2,1))
  } else if (layout == "2v") {
    l <- switch(p,
                "1" = c(1,1),
                "2" = c(1,2))
  } else if (layout == "2b2") {
    l <- switch(p,
                "1" = c(1,1),
                "2" = c(1,2),
                "3" = c(2,1),
                "4" = c(2,2))
  } else if (layout == "3v") {
    l = switch(p,
               "1" = c(1,1),
               "2" = c(1,2),
               "3" = c(1,3))
  } else if (layout == "3h") {
    l = switch(p,
               "1" = c(1,1),
               "2" = c(1,1),
               "3" = c(2,1),
               "4" = c(2,1),
               "5" = c(3,1),
               "6" = c(3,1))
  } else if (layout == "3b2") {
    l = switch(p,
               "1" = c(1,1),
               "2" = c(1,2),
               "3" = c(2,1),
               "4" = c(2,2),
               "5" = c(3,1),
               "6" = c(3,2))
  } else if (layout == "4h") {
    l = switch(p,
               "1" = c(1,1),
               "2" = c(1,1),
               "3" = c(2,1),
               "4" = c(2,1),
               "5" = c(3,1),
               "6" = c(3,1),
               "7" = c(4,1),
               "8" = c(4,1))
  } else if (layout == "4b2") {
    l = switch(p,
               "1" = c(1,1),
               "2" = c(1,2),
               "3" = c(2,1),
               "4" = c(2,2),
               "5" = c(3,1),
               "6" = c(3,2),
               "7" = c(4,1),
               "8" = c(4,2))
  } else {
    stop(paste0("Unknown layout option ", layout,
                ". Options are 1, 2h, 2v, 2b2, 3v, 3h, 3b2, 4h, 4b2."),
         call. = FALSE)
  }
}

drawpaneltitle <- function(paneltitle, panelsubtitle) {
  if (!is.null(paneltitle)) {
    graphics::mtext(paneltitle, line = -0.6, padj = 1)
  }
  if (!is.null(panelsubtitle)) {
    if (!is.null(paneltitle)) {
      extra_lines <- stringr::str_count(paneltitle, "\n")
    } else {
      extra_lines <- 0
    }
    graphics::mtext(panelsubtitle, line = (-(2.2+1.5*extra_lines)), cex = (18/20), padj = 1)
  }
}

drawaxislabels <- function(ylabel, xlabel, p, layout, xtickmargin, leftmargin) {
  if (!is.null(ylabel)) {
    side <- getsides(p, layout)
    if (!is.na(side) && side == 2) {
      graphics::mtext(text = ylabel, side = side, line = leftmargin - 2, las = 3)
    }
  }
  if (!is.null(xlabel) && needxlabels(p, layout)) {
    graphics::mtext(text = xlabel, side = 1, line = (xtickmargin-0.5))
  }
}

getsides <- function(p, layout) {
  if (layout == "1" || layout == "2v") {
    if (p == "1") {
      side <- 2
    } else if (p == "2") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p), call. = FALSE)
    }
  } else if (layout == "2h" || layout == "2b2") {
    if (p == "1" || p == "3") {
      side <- 2
    } else if (p == "2" || p == "4") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p), call. = FALSE)
    }
  } else if (layout == "3v") {
    if (p == "1") {
      side <- 2
    } else if (p == "2") {
      side <- NA
    } else if (p == "3") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p), call. = FALSE)
    }
  } else if (layout == "3h" || layout == "3b2") {
    if (p == "1" || p == "3" || p == "5") {
      side <- 2
    } else if (p == "2" || p == "4" || p == "6") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p), call. = FALSE)
    }
  } else if (layout == "4h" || layout == "4b2") {
    if (p == "1" || p == "3" || p == "5" || p == "7") {
      side <- 2
    } else if (p == "2" || p == "4" || p == "6" || p == "8") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p), call. = FALSE)
    }
  } else  {
    stop(paste0("Unknown layout option ", layout,
               ". Options are 1, 2h, 2v, 2b2, 3h, 3v, 3b2, 4h, 4b2."),
         call. = FALSE)
  }
  return(side)
}

dropfirstxlabel <- function(p, layout, dropxlabel, ts, at, label, xlim) {
  if ((layout == "2v" && p == "2") ||
      (layout == "2b2" && p == "4") ||
      (layout == "3v" && (p == "2" || p == "3")) ||
      (layout == "3b2" && p == "6") ||
      (layout == "4b2" && p == "8")) {
    if (is.na(dropxlabel)) {
      if (ts) {
        label_left_border <- at - 0.5*getstrwidth(label, units = "user")
        overlap <- (label_left_border - xlim[1]) / (xlim[2] - xlim[1])
        return(overlap < -0.015) # allow labels to overlap up to 1.5 per cent. the last year margin is 3 per cent so they should not overlap
      } else {
        return(FALSE)
      }
    } else {
      return(dropxlabel)
    }
  } else {
    return(FALSE)
   }
}

needxlabels <- function(p, layout) {
  if (layout == "1") {
    return(p == "1")
  } else if (layout == "2h") {
    return(p == "3")
  } else if (layout == "2v" || layout == "3v") {
    return(TRUE)
  } else if (layout == "2b2") {
    return(p == "3" || p == "4")
  } else if (layout == "3b2") {
    return(p == "5" || p == "6")
  } else if (layout == "3h") {
    return(p == "5")
  } else if (layout == "4h") {
    return(p == "7")
  } else if (layout == "4b2") {
    return(p == "7" || p == "8")
  } else {
    stop(paste("Unknown layout option ", layout,
               ". Options are 1, 2h, 2v, 2b2, 3h, 3v, 3b2, 4h, 4b2.",
               sep = ""),
         call. = FALSE)
  }
}

dropbottomlabel <- function(p, layout) {
  if (layout == "1" || layout == "2v" || layout == "3v") {
    return(FALSE)
  } else if (layout == "2h" || layout == "2b2") {
    return(p == "1" || p == "2")
  } else if (layout == "3h" || layout == "3b2") {
    return(p == "1" || p == "2" || p == "3" || p == "4")
  } else if (layout == "4h" || layout == "4b2") {
    return(p == "1" || p == "2" || p == "3" || p == "4" || p == "5" || p == "6")
  } else {
    stop(paste("Unknown layout option ", layout,
               ". Options are 1, 2h, 2v, 2b2.",
               sep = ""),
         call. = FALSE)
  }
}

needgrid <- function(p, layout) {
  if (layout == "1") {
    return(p == "1")
  } else if (layout == "2h") {
    return(p == "1" || p == "3")
  } else if (layout == "3h") {
    return(p == "1" || p == "3" || p == "5")
  } else if (layout == "4h") {
    return(p == "1" || p == "3" || p == "5" || p == "7")
  } else if (layout == "2v" || layout == "2b2" || layout == "3v" || layout == "3b2" || layout == "4b2") {
    return(TRUE)
  } else {
    stop(paste0("Unknown layout option ", layout,
                ". Options are 1, 2h, 2v, 2b2, 3v, 3h, 3b2, 4h, 4b2."),
         call. = FALSE)
  }
}

tickadjustment <- function(layout) {
  return(switch(layout,
                "1" = 1,
                "2h" = 2,
                "2v" = 3/2,
                "2b2" = 2,
                "3h" = 3,
                "3v" = 2,
                "3b2" = 3,
                "4h" = 4,
                "4b2" = 4))
}

yticks_to_draw <- function(labels_drop, p, layout) {
  if (dropbottomlabel(p, layout)) {
    labels_drop[2:(length(labels_drop)-1)]
  } else {
    labels_drop[1:(length(labels_drop)-1)]
  }
}

gridsandborders <- function(p, layout, yunits, xunits, yticks, xlabels, ylim, xlim, dropxlabel, srt, ts) {
  side <- getsides(p, layout)
  xlab <- needxlabels(p, layout)

  ## Draw the axis scale
  # Drop the first label
  labels_drop <- yticks_to_draw(yticks, p, layout)

  if (!is.na(side)) {
    graphics::axis(side, at = labels_drop, labels = labels_drop,
                   tck = 0, cex.lab = 1, mgp = c(3, 0.2, 0))
    # Add units
    graphics::mtext(text = yunits, side = side, at = ylim$max, line = 0.2, cex = 1, padj = 1)
  }

  ## Draw the x-axis
  if (xlab) {
    # Draw x ticks and labels
    graphics::axis(1, xlabels$ticks, tck = tickadjustment(layout)*DEFAULTTICKLENGTH, labels = FALSE)
    if (dropfirstxlabel(p, layout, dropxlabel, ts, xlabels$at[1], xlabels$labels[1], xlim)) {
      at <- xlabels$at[2:length(xlabels$at)]
      labels <- xlabels$labels[2:length(xlabels$labels)]
    } else {
      at <- xlabels$at
      labels <- xlabels$labels
    }
    if (!is.null(xunits)) {
      at <- xlabels$at[1:(length(xlabels$at)-1)]
      labels <- xlabels$labels[1:(length(xlabels$labels)-1)]
      graphics::mtext(text = xunits, side = 1, at = xlim[2], line = 0, cex = 1, padj = 1)
    }

    # Calculate what one line is in user coordinates

    if (!graphics::par("ylog")) {
      y <- ylim$min - inchesasuser_height(0.8 * CSI)
    } else {
      y <- log10(ylim$min) - inchesasuser_height(0.8 * CSI)
      y <- 10^(y)
    }

    if (srt == 0) {
      adj <- c(0.5, 1)
    } else {
      adj <- c(1, 0.5)
    }

    graphics::text(x = at, y = y, labels = labels, cex = 1, adj = adj, srt = srt, xpd = NA)
  }

  ## Draw the grid
  if (needgrid(p, layout)) {
    # graphics::grid(nx = FALSE, ny = (ylim$nsteps - 1), col = "lightgray", lty = "solid", lwd = 1)
    lapply(yticks[2:(length(yticks)-1)],
           function(x) graphics::abline(h = x, col = "lightgray", lty = "solid", lwd = 1))
    # Add a solid zero line if needed
    if (0 %in% yticks) {
      graphics::axis(1, pos = 0, c(xlim[1], xlim[2]), labels = FALSE, tck = 0, lwd = 1)
    }
  }

  if (!is.na(side)) {
    ## Draw the outer bouding box
    graphics::axis(side, c(ylim$min, ylim$max), labels = FALSE, tck = 0, lwd = 1)
  }
  # Add a line on the right. This is irrelevant/duplicate for some graphs, but for vertical multipanels it adds the divider
  if (is.na(side) || side != 4) {
    graphics::axis(4, c(ylim$min, ylim$max), labels = FALSE, tck = 0, lwd = 1)
  }

  # Draw top and bottom line, will often double up but better to overdo than under
  graphics::axis(1, c(xlim[1], xlim[2]), tck = 0, labels = FALSE, lwd = 1)
  graphics::axis(3, c(xlim[1], xlim[2]), tck = 0, labels = FALSE, lwd = 1)
}

drawshading <- function(shading, data) {
  for (s in shading) {
    x_to <- get_x_plot_locations(series_x_values(data, which(series_names(data) == s$to)), data)
    x_from <- get_x_plot_locations(series_x_values(data, which(series_names(data) == s$from)), data)
    y_to <- series_values(data, which(series_names(data) == s$to))
    y_from <- series_values(data, which(series_names(data) == s$from))
    shading_data <- data.frame(x = c(x_to,rev(x_from)),
                               y = c(y_to,rev(y_from)))
    shading_data <- shading_data[!is.na(shading_data$x) & !is.na(shading_data$y), ]
    graphics::polygon(shading_data$x,shading_data$y,col = s$colour, border = NA)
  }
}

drawlines <- function(l, data, xlim, ylim, joined, log_scale) {
  for (i in seq_along(data$series)) {
    s <- data$series[[i]]
    x <- get_x_plot_locations(s$x, data)
    if (!s$bar) {
      graphics::par(mfg = l)
      if (joined) {
        nas <- is.na(x) | is.na(s$y)
        plotx <- x[!nas]
        s$y <- s$y[!nas]
      } else {
        plotx <- x
      }
      graphics::par(cex = s$attributes$pointsize)
      graphics::plot(
        plotx,
        s$y,
        type = "o",
        col = s$attributes$col,
        xlim = xlim,
        ylim = c(ylim$min, ylim$max),
        axes = FALSE,
        xlab = "",
        ylab = "",
        pch = s$attributes$pch,
        lty = s$attributes$lty,
        lwd = s$attributes$lwd,
        log = log_scale
      )
      graphics::par(cex = 1)
    }
  }
}

as.barplot.x <- function(bp.data, x, xlim, bar.stacked, log_scale) {
  if (ncol(bp.data) > 1) {
    bp <- graphics::barplot(bp.data, plot = FALSE, xaxs = "i", yaxs = "i", beside = (!bar.stacked))
    if (!bar.stacked) {
      # We get a matrix, with rows for each data series. Need to collapse to just the centre of the x points
      bp <- apply(bp, 2, mean)
    }

    points <- data.frame(as.x = c(bp[1], bp[length(bp)]), time = c(x[1], x[ncol(bp.data)]))
    fit <- stats::lm(as.x ~ time, data = points)

    x1 <- stats::predict(fit, data.frame(time = c(xlim[1])))
    x2 <- stats::predict(fit, data.frame(time = c(xlim[2])))

    return(c(x1,x2))
  } else {
    if (length(bp.data) == 1) {
      return(c(0,1.4))
    } else {
      bp <- graphics::barplot(bp.data, plot = FALSE, xaxs = "i", yaxs = "i", beside = (!bar.stacked))
      return(c(0,max(bp)+min(bp)))
    }
  }
}

drawbars <- function(l, data, xlim, ylim, bar.stacked, log_scale) {
  out <- get_bar_data(data)
  bardata <- out$bardata
  colours <- out$colours
  bordercol <- out$bordercol

  if (ncol(bardata) > 0) {
    out <- convert_to_plot_bardata(bardata, data)
    bardata_p <- out$p
    bardata_n <- out$n
    bar_x_loc <- out$x

    xlim <- as.barplot.x(bardata_p, bar_x_loc, xlim, bar.stacked)
    graphics::par(mfg = l)
    graphics::barplot(bardata_p, col = colours, border = bordercol, xlim = xlim, ylim = c(ylim$min, ylim$max), xlab = "", ylab = "", axes = FALSE, beside = (!bar.stacked), log = log_scale, names.arg = NULL)
    graphics::par(mfg = l)
    graphics::barplot(bardata_n, col = colours, border = bordercol, xlim = xlim, ylim = c(ylim$min, ylim$max), xlab = "", ylab = "", axes = FALSE, beside = (!bar.stacked), log = log_scale)
  }
}

drawpanel <- function(p, data, shading, bgshadings, margins, layout, yunits, xunits, yticks, xlabels, ylim, xlim, paneltitle, panelsubtitle, yaxislabel, xaxislabel, bar.stacked, dropxlabel, joined, srt, log_scale) {
  # Basic set up
  graphics::par(mar = c(0, 0, 0, 0))
  l <- getlocation(p, layout)

  # Do we need an x unit
  if (data$ts || !is.scatter(data$x) || is.null(data)) {
    xunits <- NULL
  }

  # Start the plot with a blank plot, used for panels with no series
  graphics::par(mfg = l)
  graphics::plot(
    1,
    lwd = 0,
    pch = NA,
    axes = FALSE,
    xlab = "",
    ylab = "",
    xlim = xlim,
    ylim = c(ylim$min, ylim$max),
    log = log_scale
  )

  drawbgshadings(bgshadings, p)

  gridsandborders(p, layout, yunits, xunits, yticks, xlabels, ylim, xlim, dropxlabel, srt, data$ts)

  if (!is_empty(data)) drawbars(l, data, xlim, ylim, bar.stacked, log_scale)

  # Reset the plot after the bars (which use different axis limits), otherwise lines and shading occur in the wrong spot
  graphics::par(mfg = l)
  graphics::plot(
    1,
    lwd = 0,
    pch = NA,
    axes = FALSE,
    xlab = "",
    ylab = "",
    xlim = xlim,
    ylim = c(ylim$min, ylim$max),
    log = log_scale
  )

  if(!is_empty(data)) {
    drawshading(shading, data)
    drawlines(l, data, xlim, ylim, joined, log_scale)
  }

  drawpaneltitle(paneltitle, panelsubtitle)
  drawaxislabels(yaxislabel, xaxislabel, p, layout, margins$xtickmargin, margins$left)
}

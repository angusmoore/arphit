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
    stop(paste0("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2, 3v, 3h, 3b2, 4h, 4b2."))
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
      stop(paste("Layout ", layout, " does not have panel ", p, sep = ""))
    }
  } else if (layout == "2h" || layout == "2b2") {
    if (p == "1" || p == "3") {
      side <- 2
    } else if (p == "2" || p == "4") {
      side <- 4
    } else {
      stop(paste("Layout ", layout, " does not have panel ", p, sep = ""))
    }
  } else if (layout == "3v") {
    if (p == "1") {
      side <- 2
    } else if (p == "2") {
      side <- NA
    } else if (p == "3") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p))
    }
  } else if (layout == "3h" || layout == "3b2") {
    if (p == "1" || p == "3" || p == "5") {
      side <- 2
    } else if (p == "2" || p == "4" || p == "6") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p))
    }
  } else if (layout == "4h" || layout == "4b2") {
    if (p == "1" || p == "3" || p == "5" || p == "7") {
      side <- 2
    } else if (p == "2" || p == "4" || p == "6" || p == "8") {
      side <- 4
    } else {
      stop(paste0("Layout ", layout, " does not have panel ", p))
    }
  } else  {
    stop(paste("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2, 3h, 3v, 3b2, 4h, 4b2.", sep = ""))
  }
  return(side)
}

dropfirstxlabel <- function(p, layout, dropxlabel) {
  if ((layout == "2v" && p == "2") ||
      (layout == "2b2" && p == "4") ||
      (layout == "3v" && (p == "2" || p == "3")) ||
      (layout == "3b2" && p == "6") ||
      (layout == "4b2" && p == "8")) {
    return(dropxlabel)
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
    stop(paste("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2, 3h, 3v, 3b2, 4h, 4b2.", sep = ""))
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
    stop(paste("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2.", sep = ""))
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
    stop(paste0("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2, 3v, 3h, 3b2, 4h, 4b2."))
  }
}

inchesasuser <- function(x) {
  x*graphics::strheight("AA",units="user")/graphics::strheight("AA",units="inches")
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

gridsandborders <- function(p, layout, yunits, xunits, yticks, xlabels, ylim, xlim, dropxlabel, srt) {
  side <- getsides(p, layout)
  xlab <- needxlabels(p, layout)

  ## Draw the axis scale
  # Drop the first label
  labels_drop <- yticks

  if (dropbottomlabel(p, layout)) {
    labels_drop <- labels_drop[2:(length(labels_drop)-1)]
  } else {
    labels_drop <- labels_drop[1:(length(labels_drop)-1)]
  }

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
    if (dropfirstxlabel(p, layout, dropxlabel)) {
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

    if (srt == 0) {
      adj <- c(0.5, 0)
      # Calculate what one line is in user coordinates
      y <- inchesasuser(1.8 * CSI)
    } else {
      adj <- c(1, 0.5)
      # Calculate what one line is in user coordinates
      y <- inchesasuser(0.8 * CSI)
    }

    y <- ylim$min - y
    graphics::text(x = at, y = y, labels = labels, cex = 1, adj = adj, srt = srt, xpd = NA)
  }

  ## Draw the grid
  if (needgrid(p, layout)) {
    graphics::grid(nx = FALSE, ny = (ylim$nsteps - 1), col = "lightgray", lty = "solid", lwd = 1)
    # Add a zero line if needed
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

drawshading <- function(shading, data, x) {
  for (s in shading) {
    shading_data <- data.frame(x = c(x,rev(x)),
                               y = c(data[, s$to, drop = TRUE],rev(data[, s$from, drop = TRUE])))
    shading_data <- shading_data[!is.na(shading_data$x) & !is.na(shading_data$y), ]
    graphics::polygon(shading_data$x,shading_data$y,col = s$color, border = NA)
  }
}

drawlines <- function(l, series, bars, data, x, attributes, xlim, ylim, joined) {
  for (s in series) {
    if (!(s %in% bars)) {
      graphics::par(mfg = l)
      if (stats::is.ts(data)) {
        y <- as.vector(data[, s])
      } else {
        y <- data[[s]]
      }
      if (joined) {
        nas <- is.na(x) | is.na(y)
        plotx <- x[!nas]
        y <- y[!nas]
      } else {
        plotx <- x
      }
      graphics::plot(plotx, y, type = "o", col = attributes$col[[s]], xlim = xlim, ylim = c(ylim$min, ylim$max), axes = FALSE, xlab = "", ylab = "", pch = attributes$pch[[s]], lty = attributes$lty[[s]], lwd = attributes$lwd[[s]])
    }
  }
}

as.barplot.x <- function(bp.data, x, xlim, bar.stacked) {
  bp <- graphics::barplot(t(as.matrix(bp.data)), plot = FALSE, xaxs = "i", yaxs = "i", beside = (!bar.stacked))
  if (!bar.stacked) {
    # We get a matrix, with rows for each data series. Need to collapse
    bp <- apply(bp, 2, mean)
  }
  last <- nrow(bp.data)
  if (is.null(last)) {
    # only one series in the TS, which mean nrow doesn't work
    last <- length(bp.data)
  }
  points <- data.frame(as.x = c(bp[1], bp[length(bp)]), time = c(x[1], x[last]))
  fit <- stats::lm(as.x ~ time, data = points)

  x1 <- stats::predict(fit, data.frame(time = c(xlim[1])))
  x2 <- stats::predict(fit, data.frame(time = c(xlim[2])))

  return(c(x1,x2))
}

drawbars <- function(l, series, bars, data, x, attributes, xlim, ylim, bar.stacked) {
  barcolumns <- c()
  colors <- c()
  bordercol <- c()

  for (s in series) {
    if (s %in% bars) {
      barcolumns <- append(barcolumns, s)
      colors <- append(colors, attributes$col[[s]])
      bordercol <- append(bordercol, attributes$barcol[[s]])
    }
  }
  if (length(barcolumns) > 0) {
    bardata <- t(as.matrix(data[, barcolumns]))
    bardata[is.na(bardata)] <- 0 # singletons don't show otherwise (#82)
    # Split into positive and negative (R doesn't stack well across axes)
    bardata_p <- bardata
    bardata_n <- bardata
    bardata_p[bardata[, ] > 0] <- 0
    bardata_n[bardata[, ] <= 0] <- 0

    xlim <- as.barplot.x(data[, barcolumns], x, xlim, bar.stacked)
    graphics::par(mfg = l)
    graphics::barplot(bardata_p, col = colors, border = bordercol, xlim = xlim, ylim = c(ylim$min, ylim$max), xlab = "", ylab = "", axes = FALSE, beside = (!bar.stacked))
    graphics::par(mfg = l)
    graphics::barplot(bardata_n, col = colors, border = bordercol, xlim = xlim, ylim = c(ylim$min, ylim$max), xlab = "", ylab = "", axes = FALSE, beside = (!bar.stacked))
  }
}

getxvals <- function(data, ists, xvals) {
  if (stats::is.ts(data) || ists || is.scatter(xvals) || is.null(data)) {
    # time series or scatter
    return(xvals)
  } else if (!is.null(xvals)) {
    if (is.numeric(xvals)) {
      return(xvals + 0.5*min(diff(xvals)))
    } else {
      # Categorical data, offset by half
      return(1:length(xvals) + 0.5)
    }
  } else {
    return(xvals)
  }
}

drawpanel <- function(p, series, bars, data, xvals, ists, shading, bgshadings, margins, layout, attributes, yunits, xunits, yticks, xlabels, ylim, xlim, paneltitle, panelsubtitle, yaxislabel, xaxislabel, bar.stacked, dropxlabel, joined, srt) {
  # Basic set up
  graphics::par(mar = c(0, 0, 0, 0))
  l <- getlocation(p, layout)

  # Handling the x variables
  x <- getxvals(data, ists, xvals)

  # Do we need an x unit
  if (stats::is.ts(data) || ists || !is.scatter(xvals) || is.null(data)) {
    xunits <- NULL
  }

  # Start the plot with a blank plot, used for panels with no series
  graphics::par(mfg = l)
  graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "", xlim = xlim, ylim = c(ylim$min, ylim$max))

  drawbgshadings(bgshadings, p)

  gridsandborders(p, layout, yunits, xunits, yticks, xlabels, ylim, xlim, dropxlabel, srt)

  drawbars(l, series, bars, data, x, attributes, xlim, ylim, bar.stacked)

  # Reset the plot after the bars (which use different axis limits), otherwise lines and shading occur in the wrong spot
  graphics::par(mfg = l)
  graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "", xlim = xlim, ylim = c(ylim$min, ylim$max))
  drawshading(shading, data, x)
  drawlines(l, series, bars, data, x, attributes, xlim, ylim, joined)

  drawpaneltitle(paneltitle, panelsubtitle)
  drawaxislabels(yaxislabel, xaxislabel, p, layout, margins$xtickmargin, margins$left)
}

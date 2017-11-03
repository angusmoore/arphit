getlocation <- function(p, layout) {
  if (layout == "1") {
    l <- c(1,1)
  } else if (layout == "2h") {
    if (p == "1" || p == "2") {
      l <- c(1,1)
    } else {
      l <- c(2,1)
    }
  } else if (layout == "2v") {
    if (p == "1") {
      l <- c(1,1)
    } else {
      l <- c(1,2)
    }
  } else if (layout == "2b2") {
    if (p == "1") {
      l <- c(1,1)
    } else if (p == "2") {
      l <- c(1,2)
    } else if (p == "3") {
      l <- c(2,1)
    } else {
      l <- c(2,2)
    }
  } else {
    stop(paste("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2.", sep = ""))
  }
}

drawpaneltitle <- function(paneltitle, panelsubtitle) {
  if (!is.null(paneltitle)) {
    graphics::mtext(paneltitle, line = -1.6)
  }
  if (!is.null(panelsubtitle)) {
    message("I'm not adjusting for multi-line panel titles")
    graphics::mtext(panelsubtitle, line = -2.8, cex = (18/20))
  }
}

getsides <- function(p, panels, layout) {
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
  } else  {
    stop(paste("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2.", sep = ""))
  }
  return(side)
}

dropfirstxlabel <- function(p, layout, dropxlabel) {
  if ((layout == "2v" && p == "2") || (layout == "2b2" && p == "4")) {
    return(dropxlabel)
  } else {
    return(FALSE)
   }
}

needxlabels <- function(p, layout) {
  if (layout == "1") {
    if (p == "1") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (layout == "2h") {
    if (p == "3") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (layout == "2v") {
    return(TRUE)
  } else if (layout == "2b2") {
    if (p == "3" || p == "4") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    stop(paste("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2.", sep = ""))
  }
}

dropbottomlabel <- function(p, layout) {
  if (layout == "1" || layout == "2v") {
    return(FALSE)
  } else if (layout == "2h" || layout == "2b2") {
    if (p == "1" || p == "2") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    stop(paste("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2.", sep = ""))
  }
}

needgrid <- function(p, layout) {
  if (layout == "1") {
    if (p == "1") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (layout == "2h") {
    if (p == "1" || p == "3") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else if (layout == "2v" || layout == "2b2") {
    return(TRUE)
  } else {
    stop(paste("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2.", sep = ""))
  }
}

gridsandborders <- function(p, panels, layout, portrait, scaleunits, ticks, xlabels, ylim, xlim, dropxlabel) {
  side <- getsides(p, panels, layout)
  xlab <- needxlabels(p, layout)

  # Drop the first label
  labels_drop <- ticks[[p]]

  if (dropbottomlabel(p, layout)) {
    labels_drop <- labels_drop[2:(length(labels_drop)-1)]
  } else {
    labels_drop <- labels_drop[1:(length(labels_drop)-1)]
  }

  graphics::axis(side, at = labels_drop, labels = labels_drop, tck = 0, cex.lab = 1, mgp = c(3, 0.2, 0))
  graphics::axis(side, c(ylim[[p]]$min, ylim[[p]]$max), labels = FALSE, tck = 0)

  # Add a line on the right. This is irrelevant/duplicate for some graphs, but for vertical multipanels it adds the divider
  if (side == 2) {
    graphics::axis(4, c(ylim[[p]]$min, ylim[[p]]$max), labels = FALSE, tck = 0)
  }

  # Add units
  graphics::mtext(text = scaleunits[[p]], side = side, at = ylim[[p]]$max, line = 0.2, cex = 1, padj = 1)

  if (xlab) {
    # Draw ticks and labels
    graphics::axis(1, xlim[[p]][1]:xlim[[p]][2], tck = DEFAULTTICKLENGTH, labels = FALSE)
    if (dropfirstxlabel(p, layout, dropxlabel)) {
      at = xlabels[[p]]$at[2:length(xlabels[[p]]$at)]
      labels = xlabels[[p]]$labels[2:length(xlabels[[p]]$labels)]
    } else {
      at = xlabels[[p]]$at
      labels = xlabels[[p]]$labels
    }
    graphics::axis(1, xlim[[p]][1]:xlim[[p]][2], tck = 0, at = at, labels = labels, cex.lab = 1, mgp = c(3, 0.6, 0))
  }

  # Draw top and bottom line, will often double up but better to overdo than under
  graphics::axis(1, xlim[[p]][1]:xlim[[p]][2], tck = 0, labels = FALSE)
  graphics::axis(3, xlim[[p]][1]:xlim[[p]][2], tck = 0, labels = FALSE)

  if (needgrid(p, layout)) {
    graphics::grid(nx = FALSE, ny = (ylim[[p]]$nsteps - 1), col = "lightgray", lty = "solid")
    # Add a zero line if needed
    if (0 %in% ticks[[p]]) {
      graphics::axis(1, pos = 0, xlim[[p]][1]:xlim[[p]][2], labels = FALSE, tck = 0)
    }
  }
}

drawshading <- function(shading, data) {
  x <- stats::time(data) + 1.0/(2*stats::frequency(data))
  for (s in shading) {
    graphics::polygon(c(x,rev(x)),c(data[, s$to],rev(data[, s$from])),col = s$color, border = NA)
  }
}

drawlines <- function(p, l, panels, data, attributes, xlim, ylim) {
  for (s in panels$panels[[p]]) {
    if (!s %in% names(panels$bars)) {
      graphics::par(mfg = l)
      shiftedxlim <- xlim[[p]] - 1.0/(2*stats::frequency(data))
      graphics::plot(data[, s], type = "o", col = attributes$col[[s]], xlim = shiftedxlim, ylim = c(ylim[[p]]$min, ylim[[p]]$max), axes = FALSE, xlab = "", ylab = "", pch = attributes$pch[[s]], lty = attributes$lty[[s]], lwd = attributes$lwd[[s]])
    }
  }
}

as.barplot.x <- function(bp.data, xlim, bar.stacked) {
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
  points <- data.frame(as.x = c(bp[1], bp[length(bp)]), time = c(stats::time(bp.data)[1], stats::time(bp.data)[last]))
  fit <- stats::lm(as.x ~ time, data = points)

  x1 <- stats::predict(fit, data.frame(time = c(xlim[1])))
  x2 <- stats::predict(fit, data.frame(time = c(xlim[2])))

  return(c(x1,x2))
}

drawbars <- function(p, l, panels, data, attributes, xlim, ylim, bar.stacked) {
  barcols <- c()
  colors <- c()
  bordercol <- c()

  for (s in panels$panels[[p]]) {
    if (s %in% names(panels$bars)) {
      barcols <- append(barcols, s)
      colors <- append(colors, attributes$col[[s]])
      bordercol <- append(bordercol, attributes$bordercol[[s]])
    }
  }
  if (length(barcols) > 0) {
    bardata <- t(as.matrix(data[, barcols]))
    # Split into positive and negative (R doesn't stack well across axes)
    bardata_p <- bardata
    bardata_n <- bardata
    bardata_p[bardata[, ] > 0] <- 0
    bardata_n[bardata[, ] <= 0] <- 0

    shiftedxlim <- as.barplot.x(data[, barcols], xlim[[p]] - 1.0/(2*stats::frequency(data)), bar.stacked)
    graphics::par(mfg = l)
    graphics::barplot(bardata_p, col = colors, border = bordercol, xlim = shiftedxlim, ylim = c(ylim[[p]]$min, ylim[[p]]$max), xlab = "", ylab = "", axes = FALSE, beside = (!bar.stacked))
    graphics::par(mfg = l)
    graphics::barplot(bardata_n, col = colors, border = bordercol, xlim = shiftedxlim, ylim = c(ylim[[p]]$min, ylim[[p]]$max), xlab = "", ylab = "", axes = FALSE, beside = (!bar.stacked))
  }
}

drawpanel <- function(p, panels, data, shading, bgshadings, margins, layout, portrait, attributes, scaleunits, ticks, xlabels, ylim, xlim, paneltitles, panelsubtitles, bar.stacked, dropxlabel) {
  graphics::par(mar = c(0, 0, 0, 0))
  l <- getlocation(p ,layout)

  # Start the plot with a blank plot, used for panels with no series
  graphics::par(mfg = l)
  graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "", xlim = xlim[[p]], ylim = c(ylim[[p]]$min, ylim[[p]]$max))

  drawbgshadings(bgshadings, p, data)

  gridsandborders(p, panels, layout, portrait, scaleunits, ticks, xlabels, ylim, xlim, dropxlabel)

  drawbars(p, l, panels, data, attributes, xlim, ylim, bar.stacked)

  # Reset the plot after the bars (which use different axis limits), otherwise lines and shading occur in the wrong spot
  graphics::par(mfg = l)
  graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "", xlim = xlim[[p]], ylim = c(ylim[[p]]$min, ylim[[p]]$max))
  drawshading(shading[[p]], data)
  drawlines(p, l, panels, data, attributes, xlim, ylim)


  drawpaneltitle(paneltitles[[p]], panelsubtitles[[p]])
}

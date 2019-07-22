

pretty_format_numbers <- function(labels) {
  n_decimals <-
    max(sapply(stringr::str_split(
      formatC(
        labels,
        format = "f",
        drop0trailing = TRUE,
        digits = 10
      ),
      "\\."
    ),
    function(x) {
      if (length(x) == 2) {
        nchar(x[[2]])
      } else {
        return(0)
      }
    }))
  formatC(labels,
          format = "f",
          digits = n_decimals,
          big.mark = ",")
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

drawaxislabels_hlayout <- function(ylabel, xlabel, p, layout, tickmargin, leftmargin) {
  if (!is.null(ylabel)) {
    side <- getsides(p, layout)
    if (!is.na(side) && side == 1) {
      graphics::mtext(text = ylabel, side = side, line = tickmargin + 0.2)
    }
  }
  if (!is.null(xlabel) && needxlabels(p, layout)) {
    graphics::mtext(text = xlabel, side = xaxisside(layout), line = leftmargin - 1.8, las = 3)
  }
}

drawaxislabels <- function(ylabel, xlabel, p, layout, xtickmargin, leftmargin) {
  if (layout == "1h") return(drawaxislabels_hlayout(ylabel, xlabel, p, layout, xtickmargin, leftmargin))
  if (!is.null(ylabel)) {
    side <- getsides(p, layout)
    if (!is.na(side) && side == 2) {
      graphics::mtext(text = ylabel, side = side, line = leftmargin - 1.8, las = 3)
    }
  }
  if (!is.null(xlabel) && needxlabels(p, layout)) {
    graphics::mtext(text = xlabel, side = xaxisside(layout), line = xtickmargin + 0.2)
  }
}

yticks_to_draw <- function(labels_drop, p, layout) {
  if (dropbottomlabel(p, layout)) {
    labels_drop[2:(length(labels_drop)-1)]
  } else {
    labels_drop[1:(length(labels_drop)-1)]
  }
}

xaxisside <- function(layout) {
  if (layout == "1h") {
    2
  } else {
    1
  }
}

format_same_dp <- function(labels) {
  n_decimals <- max(sapply(stringr::str_split(labels, "\\."),
                           function(x) {
                             if (length(x) == 2) {
                               nchar(x[[2]])
                             } else {
                               return(0)
                             }
                           }))
  formatC(labels, format = "f", digits = n_decimals)
}

drawyaxis <- function(p, layout, yunits, yticks, ylim) {
  side <- getsides(p, layout)
  # Drop the first label
  labels_drop_at <- yticks_to_draw(yticks, p, layout)
  labels_drop_formatted <- format_same_dp(labels_drop_at)

  if (!is.na(side)) {
    if (layout != "1h") {
      mgp <- c(3, 0.2, 0)
      units_shift <- 0.2
    } else {
      mgp <- c(3, 0.7, 0)
      units_shift <- -0.3
    }
    graphics::axis(side, at = labels_drop_at, labels = labels_drop_formatted,
                   tck = 0, cex.lab = 1, mgp = mgp)
    # Add units
    graphics::mtext(text = yunits, side = side, at = ylim[2], line = units_shift, cex = 1, padj = 1)
  }
}

drawxaxis <- function(p, layout, xunits, xlabels, xlim, ylim, dropxlabel, srt, ts) {
  if (needxlabels(p, layout)) {
    # Draw x ticks and labels
    graphics::axis(xaxisside(layout), xlabels$ticks, tck = tickadjustment(layout)*DEFAULTTICKLENGTH, labels = FALSE)
    if (dropfirstxlabel(p, layout, dropxlabel, ts, xlabels$at[1], xlabels$labels[1], xlim)) {
      at <- xlabels$at[2:length(xlabels$at)]
      labels <- xlabels$labels[2:length(xlabels$labels)]
    } else {
      at <- xlabels$at
      labels <- xlabels$labels
    }

    # Draw x axis unit if required
    if (!is.null(xunits)) {
      at <- at[1:(length(at)-1)]
      labels <- labels[1:(length(labels)-1)]
      graphics::mtext(text = xunits, side = xaxisside(layout), at = xlim[2], line = 0, cex = 1, padj = 1)
    }

    # Calculate what one line is in user coordinates
    if (layout != "1h") {
      oneline <- inchesasuser_height(0.8 * CSI)
    } else {
      oneline <- inchesasuser_width(0.8 * CSI)
    }
    if (!graphics::par("ylog")) {
      y <- ylim[1] - oneline
    } else {
      y <- log10(ylim[1]) - oneline
      y <- 10^(y)
    }

    if (srt == 0) {
      adj <- c(0.5, 1)
    } else {
      adj <- c(1, 0.5)
    }
    if (layout != "1h") {
      graphics::text(x = at, y = y, labels = labels, cex = 1, adj = adj, srt = srt, xpd = NA)
    } else {
      graphics::text(x = y, y = at, labels = labels, cex = 1, adj = rev(adj), srt = srt, xpd = NA)
    }
  }
}

drawborder <- function(p, layout, xlim, ylim, horiz) {
  if (horiz) return(drawborder(p, layout, ylim, xlim, FALSE))
  if (!is.na(getsides(p, layout))) {
    ## Draw the outer bouding box
    graphics::axis(1, xlim, labels = FALSE, tck = 0, lwd = 1)
    graphics::axis(2, ylim, labels = FALSE, tck = 0, lwd = 1)
    graphics::axis(4, ylim, labels = FALSE, tck = 0, lwd = 1)
  }
  graphics::axis(3, xlim, labels = FALSE, tck = 0, lwd = 1)
}

drawgrid <- function(p, layout, yticks, xlim) {
  side <- getsides(p, layout)

  ## Draw the grid
  if (needgrid(p, layout)) {
    if (layout == "1h") {
      lapply(yticks[2:(length(yticks)-1)],
             function(x) graphics::abline(v = x, col = "lightgray", lty = "solid", lwd = 1))
    } else {
      lapply(yticks[2:(length(yticks)-1)],
             function(x) graphics::abline(h = x, col = "lightgray", lty = "solid", lwd = 1))
    }
    # Add a solid zero line if needed
    if (0 %in% yticks) {
      graphics::axis(xaxisside(layout), pos = 0, c(xlim[1], xlim[2]), labels = FALSE, tck = 0, lwd = 1)
    }
  }
}

gridsandborders <- function(p, layout, yunits, xunits, yticks, xlabels, ylim, xlim, dropxlabel, srt, ts) {
  drawyaxis(p, layout, yunits, yticks, ylim)
  drawxaxis(p, layout, xunits, xlabels, xlim, ylim, dropxlabel, srt, ts)
  drawgrid(p, layout, yticks, xlim)
  drawborder(p, layout, xlim, ylim, layout == "1h")
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

interleave <- function(a, b) {
  idx <- order(c(seq_along(a), seq_along(b)))
  c(a,b)[idx]
}

drawstep <- function(s, plotx, ploty, xlim, ylim, log_scale, horiz) {
  plotx <- interleave(plotx, plotx[2:length(plotx)])
  ploty <- interleave(ploty, ploty[1:length(ploty)-1])

  drawline(s, plotx, ploty, xlim, ylim, log_scale, horiz)
}

rotate_log_scale <- function(log_scale) {
  if (log_scale == "") {
    ""
  } else if (log_scale == "x") {
    "y"
  } else if (log_scale == "y") {
    "x"
  } else {
    "xy"
  }
}

drawline <- function(s, plotx, ploty, xlim, ylim, log_scale, horiz) {
  if (horiz) {
    drawline(s, ploty, plotx, ylim, xlim, rotate_log_scale(log_scale), FALSE) # flip x and y
  } else {
    graphics::plot(
      x = plotx,
      y = ploty,
      type = "o",
      col = s$attributes$col,
      xlim = xlim,
      ylim = ylim,
      axes = FALSE,
      xlab = "",
      ylab = "",
      pch = s$attributes$pch,
      lty = s$attributes$lty,
      lwd = s$attributes$lwd,
      log = log_scale
    )
  }
}

drawlines <- function(l, data, xlim, ylim, joined, log_scale, horiz) {
  for (i in seq_along(data$series)) {
    s <- data$series[[i]]
    x <- get_x_plot_locations(s$x, data)
    graphics::par(mfg = l)
    graphics::par(cex = s$attributes$pointsize)
    if (joined) {
      nas <- is.na(x) | is.na(s$y)
      plotx <- x[!nas]
      s$y <- s$y[!nas]
    } else {
      plotx <- x
    }
    if (s$geomtype == "line") drawline(s, plotx, s$y, xlim, ylim, log_scale, horiz)
    if (s$geomtype == "step") drawstep(s, plotx, s$y, xlim, ylim, log_scale, horiz)
    graphics::par(cex = 1)
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

drawbar <- function(l, bardata, colours, bordercol, xlim, ylim, bar.stacked, log_scale, horiz) {
  if (any(bardata != 0)) { # otherwise no point - and doing so causes errors with log scale plots
    graphics::par(mfg = l)
    graphics::barplot(
      bardata,
      col = colours,
      border = bordercol,
      xlim = xlim,
      ylim = ylim,
      xlab = "",
      ylab = "",
      axes = FALSE,
      beside = (!bar.stacked),
      log = log_scale,
      names.arg = NULL,
      horiz = horiz
    )
  }
}

drawbars <- function(l, data, xlim, ylim, bar.stacked, log_scale, horiz) {
  out <- get_bar_data(data)
  bardata <- out$bardata
  colours <- out$colours
  bordercol <- out$bordercol

  if (ncol(bardata) > 0) {
    out <- convert_to_plot_bardata(bardata, data)
    bardata_p <- out$p
    bardata_n <- out$n
    bar_x_loc <- out$x

    if ((log_scale == "y" || log_scale == "xy") && any(bardata_n != 0)) {
      stop("y log scale plots cannot have negative data (in one of your bar series)")
    }

    xlim <- as.barplot.x(bardata_p, bar_x_loc, xlim, bar.stacked)
    if (!horiz) {
      drawbar(l, bardata_p, colours, bordercol, xlim, ylim, bar.stacked, log_scale, horiz)
      drawbar(l, bardata_n, colours, bordercol, xlim, ylim, bar.stacked, log_scale, horiz)
    }  else {
      drawbar(l, bardata_p, colours, bordercol, ylim, xlim, bar.stacked, rotate_log_scale(log_scale), horiz)
      drawbar(l, bardata_n, colours, bordercol, ylim, xlim, bar.stacked, rotate_log_scale(log_scale), horiz)
    }
  }
}

draw_blankplot <- function(l, xlim, ylim, log_scale, horiz) {
  if (horiz == TRUE) {
    draw_blankplot(l, ylim, xlim, rotate_log_scale(log_scale), FALSE) # flip the axis limits
  } else {
    graphics::par(mfg = l)
    graphics::plot(
      1,
      lwd = 0,
      pch = NA,
      axes = FALSE,
      xlab = "",
      ylab = "",
      xlim = xlim,
      ylim = ylim,
      log = log_scale
    )
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
  draw_blankplot(l, xlim, ylim, log_scale, layout == "1h")

  drawbgshadings(bgshadings, p)

  gridsandborders(p, layout, yunits, xunits, yticks, xlabels, ylim, xlim, dropxlabel, srt, data$ts)

  if (!is_empty(data)) drawbars(l, data, xlim, ylim, bar.stacked, log_scale, layout == "1h")

  # Reset the plot after the bars (which use different axis limits), otherwise lines and shading occur in the wrong spot
  draw_blankplot(l, xlim, ylim, log_scale, layout == "1h")

  if(!is_empty(data)) {
    drawshading(shading, data)
    drawlines(l, data, xlim, ylim, joined, log_scale, layout == "1h")
  }

  drawpaneltitle(paneltitle, panelsubtitle)
  drawaxislabels(yaxislabel, xaxislabel, p, layout, margins$xtickmargin, margins$left)
}

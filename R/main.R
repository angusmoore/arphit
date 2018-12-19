
agg_draw_internal <- function(gg, filename) {

  series <- get_series_names(gg$data, gg$x)

  # Determine the x values for each panel
  xvals <- get_x_values(gg$data, gg$x)
  out <- arrange_data(gg$data, gg$x, xvals)
  data <- out$data
  xvals <- out$xvals

  # Handle panels
  panels <- handlepanels(series, gg$layout)

  # handle series attributes
  attributes <- handleattributes(panels, gg$col, gg$pch, gg$lty, gg$lwd, gg$barcol, gg$pointsize)

  # Units and scales
  yunits <- handleunits(panels, gg$yunits, gg$layout)
  xunits <- handlexunits(panels, gg$xunits)
  yaxislabels <- handleaxislabels(gg$yaxislabels, panels)
  xaxislabels <- handleaxislabels(gg$xaxislabels, panels)

  if (length(gg$xlim)==0 && (gg$log_scale == "x" || gg$log_scale == "xy")) {
    stop("You must manually set x axis limits for log scale plots.")
  }
  xlim <- xlimconform(panels, gg$xlim, xvals, data)
  xlabels <- handlexlabels(panels, xlim, xvals, data, gg$layout, gg$showallxlabels)

  if (length(gg$ylim)==0 && (gg$log_scale == "y" || gg$log_scale == "xy")) {
    stop("You must manually set y axis limits for log scale plots.")
  }
  ylim <- ylimconform(panels, gg$ylim, data, gg$bars, gg$layout, gg$stacked, xvals, xlim)
  yticks <- handleticks(data, ylim)

  # Handle shading
  shading <- handleshading(gg$shading, panels)

  # handle annotations
  labels <- gg$labels
  arrows <- gg$arrows

  # Get number of legend cols (if needed)
  if (gg$legend) {
    legend.cr <- determinelegendcols(panels, gg$legend.ncol)
    legend.nrow <- legend.cr$r
    legend.ncol <- legend.cr$c
  } else {
    legend.nrow <- 0
  }

  # Format titles, footnotes and sources
  footnotes <- formatfn(gg$footnotes, gg$plotsize[2] - WIDTHSPACESNOTES)
  sources <- formatsrcs(gg$sources, gg$plotsize[2] - WIDTHSPACESSOURCES)
  if (!is.null(gg$title)) {
    title <- splitoverlines(gg$title, gg$plotsize[2]+MINIMUMSIDEPADDING, 28/20)
  } else {
    title <- NULL
  }
  if (!is.null(gg$subtitle)) {
    subtitle <- splitoverlines(gg$subtitle, gg$plotsize[2]+MINIMUMSIDEPADDING, 1)
  } else {
    subtitle <- NULL
  }

  # Conform panel titles
  paneltitles <- conformpaneltitles(panels, gg$paneltitles, gg$layout, gg$plotsize[2])
  panelsubtitles <- conformpaneltitles(panels, gg$panelsubtitles, gg$layout, gg$plotsize[2])

  # Now need to start the canvas
  device <- finddevice(filename)
  margins <- figuresetup(filename, device, panels, xlabels, yticks, yunits, title, subtitle, footnotes, sources, yaxislabels, xaxislabels, legend.nrow, gg$plotsize, gg$portrait, gg$layout, gg$srt)
  handlelayout(gg$layout)

  # Plot each panel
  for (p in names(panels)) {
    drawpanel(
      p,
      panels[[p]],
      gg$bars[[p]],
      data[[p]],
      xvals[[p]],
      !is.null(xvals[[paste0(p, "ts")]]),
      xvals[[paste0(p, "freq")]],
      shading[[p]],
      gg$bgshading,
      margins,
      gg$layout,
      attributes[[p]],
      yunits[[p]],
      xunits[[p]],
      yticks[[p]],
      xlabels[[p]],
      ylim[[p]],
      xlim[[p]],
      paneltitles[[p]],
      panelsubtitles[[p]],
      yaxislabels[[p]],
      xaxislabels[[p]],
      gg$stacked,
      gg$dropxlabel,
      gg$joined,
      gg$srt,
      gg$log_scale
    )
  }

  # Draw outer material
  # For easy alignment of legend, footnotes and sources just draw a unit square over the graph
  graphics::par(mfrow=c(1,1))
  graphics::par(mfg = c(1,1))
  graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "",
                 xlim = c(0,1), ylim = c(0, 1))
  drawtitle(title, subtitle)
  if (gg$legend) {

    drawlegend(panels, gg$bars, attributes, legend.ncol, margins$xtickmargin, length(xaxislabels)>0)
  }
  drawnotes(footnotes, sources, margins$notesstart)
  handlelayout(gg$layout) # Put the correct layout back

  for (p in names(panels)) {
    # Fraw all the annotations
    l <- getlocation(p ,gg$layout)
    graphics::par(mfg = l)
    graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "",
                   xlim = xlim[[p]], ylim = c(ylim[[p]]$min, ylim[[p]]$max))

    drawannotationlines(gg$lines, p)
    drawarrows(arrows, p)
    drawlabels(labels, p)
  }


  if (gg$enable_autolabeller) {
    # Finally, if desired, run the autolabeller
    autolabel(gg, panels, xlim, ylim, margins, labels, xvals, data, attributes, gg$bars, gg$autolabel_quiet)
  }

  if (!is.null(device)) {
    grDevices::dev.off() # Close files, but not if we're using default device.
  }
}

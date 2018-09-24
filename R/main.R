
agg_draw_internal <- function(data, series = NULL, x = NULL, layout = "1", bars = NULL, filename = NULL, shading = NULL, title = NULL, subtitle = NULL, paneltitles = NULL, panelsubtitles = NULL, yaxislabels = NULL, xaxislabels = NULL, footnotes = NULL, sources = NULL, yunits = NULL, xunits = NULL, labels = NULL, arrows = NULL, bgshading = NULL, lines = NULL, col = NULL, pch = NULL, lty = NULL, lwd = NULL, barcol = NULL, xlim = NULL, ylim = NULL, legend = FALSE, legend.ncol = NA, plotsize = LANDSCAPESIZE, portrait = FALSE, bar.stacked = TRUE, dropxlabel = FALSE, joined = TRUE, srt = 0, showallxlabels = NULL, enable_autolabeller = FALSE) {

  out <- handledata(series, data, x)
  data <- out$data
  series <- out$series

  # Determine the x values for each panel
  xvals <- get_x_values(data, x)
  out <- arrange_data(data, x, xvals)
  data <- out$data
  xvals <- out$xvals

  # Handle panels
  panels <- handlepanels(series, layout)
  bars <- handlebars(panels, bars)

  # handle series attributes
  attributes <- handleattributes(panels, col, pch, lty, lwd, barcol)

  # Units and scales
  yunits <- handleunits(panels, yunits, layout)
  xunits <- handlexunits(panels, xunits)
  ylim <- ylimconform(panels, ylim, data, layout)
  yticks <- handleticks(data, panels, ylim)
  xlim <- xlimconform(panels, xlim, xvals, data)
  xlabels <- handlexlabels(panels, xlim, xvals, data, layout, showallxlabels)
  yaxislabels <- handleaxislabels(yaxislabels, panels)
  xaxislabels <- handleaxislabels(xaxislabels, panels)

  # Handle shading
  shading <- handleshading(shading, panels)

  # handle annotations
  labels <- sanitychecklabels(labels)
  arrows <- sanitycheckarrows(arrows)
  bgshading <- sanitycheckbgshading(bgshading)
  lines <- sanitychecklines(lines)

  # Get number of legend cols (if needed)
  if (legend) {
    legend.cr <- determinelegendcols(panels, legend.ncol)
    legend.nrow <- legend.cr$r
    legend.ncol <- legend.cr$c
  } else {
    legend.nrow <- 0
  }

  # Format titles, footnotes and sources
  footnotes <- formatfn(footnotes, plotsize[2] - WIDTHSPACESNOTES)
  sources <- formatsrcs(sources, plotsize[2] - WIDTHSPACESSOURCES)
  if (!is.null(title)) {
    title <- splitoverlines(title, plotsize[2]+MINIMUMSIDEPADDING, 28/20)
  }
  if (!is.null(subtitle)) {
    subtitle <- splitoverlines(subtitle, plotsize[2]+MINIMUMSIDEPADDING, 1)
  }

  # Conform panel titles
  paneltitles <- conformpaneltitles(panels, paneltitles, layout, plotsize[2])
  panelsubtitles <- conformpaneltitles(panels, panelsubtitles, layout, plotsize[2])

  # Now need to start the canvas
  device <- finddevice(filename)
  margins <- figuresetup(filename, device, panels, xlabels, yticks, yunits, title, subtitle, footnotes, sources, yaxislabels, xaxislabels, legend.nrow, plotsize, portrait, layout, srt)
  handlelayout(layout)

  # Plot each panel
  for (p in names(panels)) {
    drawpanel(
      p,
      panels[[p]],
      bars[[p]],
      data[[p]],
      xvals[[p]],
      !is.null(xvals[[paste0(p, "ts")]]),
      xvals[[paste0(p, "freq")]],
      shading[[p]],
      bgshading,
      margins,
      layout,
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
      bar.stacked,
      dropxlabel,
      joined,
      srt
    )
  }

  # Draw outer material
  # For easy alignment of legend, footnotes and sources just draw a unit square over the graph
  graphics::par(mfrow=c(1,1))
  graphics::par(mfg = c(1,1))
  graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "",
                 xlim = c(0,1), ylim = c(0, 1))
  drawtitle(title, subtitle)
  if (legend) {

    drawlegend(panels, bars, attributes, legend.ncol, margins$xtickmargin, length(xaxislabels)>0)
  }
  drawnotes(footnotes, sources, margins$notesstart)
  handlelayout(layout) # Put the correct layout back

  if (enable_autolabeller) {
    autogenlabel <- autolabel(xvals, data, panels, shading, layout, xlim, ylim, attributes, bgshading, lines, arrows, labels)
    labels <- append(labels, autogenlabel$labels)
    arrows <- append(arrows, autogenlabel$arrows)
  }

  for (p in names(panels)) {
    # Finally, draw all the annotations
    l <- getlocation(p ,layout)
    graphics::par(mfg = l)
    graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "",
                   xlim = xlim[[p]], ylim = c(ylim[[p]]$min, ylim[[p]]$max))

    drawannotationlines(lines, p)
    drawarrows(arrows, p)
    drawlabels(labels, p)
  }

  if (!is.null(device)) {
    grDevices::dev.off() # Close files, but not if we're using default device.
  }
}

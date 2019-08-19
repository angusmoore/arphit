
agg_draw_internal <- function(gg, filename) {

  inval_panel <- !names(gg$data) %in% permitted_panels(gg$layout)
  if (any(inval_panel)) {
    inval_panel <-  names(gg$data)[inval_panel]
    stop(
      paste0(
        "Your chosen layout (",
        gg$layout,
        ") does not have a panel ",
        inval_panel,
        "."
      ),
      call. = FALSE
    )
  }

  data <- convert_ts_to_decimal_date(gg$data)

  for (p in permitted_panels(gg$layout)) {
    if (is.null(data[[p]])) {
      # Empty plot, so create a null entry
      data[[p]] <- new_panel_data(TRUE, 1)
    }
  }

  # Make sure the list is ordered in panel order
  data <- data[order(names(data))]

  # handle series attributes
  data <- handleattributes(data)

  # split out bardata, as that has to be plotted separately
  data <- lapply(data, extract_bar_data)

  # Units and scales
  yunits <- handleunits(data, gg$yunits, gg$layout)
  xunits <- handlexunits(names(data), gg$xunits)
  yaxislabels <- handleaxislabels(gg$yaxislabels, names(data))
  xaxislabels <- handleaxislabels(gg$xaxislabels, names(data))

  if (length(gg$xlim) == 0 && (gg$log_scale == "x" || gg$log_scale == "xy")) {
    stop("You must manually set x axis limits for log scale plots.",
         call. = FALSE)
  }
  xlim <- xlimconform(gg$xlim, data, gg$layout)
  xlabels <- handlexlabels(data, xlim, gg$xfreq, gg$layout, gg$showallxlabels)
  xlim <- collapse_in_padding(xlim)

  if (length(gg$ylim) == 0 && (gg$log_scale == "y" || gg$log_scale == "xy")) {
    stop("You must manually set y axis limits for log scale plots.",
         call. = FALSE)
  }
  ylim <- ylimconform(gg$ylim, data, gg$layout, gg$stacked, xlim)
  yticks <- handleticks(data, ylim)

  # Handle shading
  shading <- handleshading(gg$shading, data)

  # handle annotations
  labels <- gg$labels
  arrows <- gg$arrows

  # Get number of legend cols (if needed)
  if (gg$legend) {
    legend_cr <- determinelegendcols(data, gg$legend_ncol)
    legend_nrow <- legend_cr$r
    legend_ncol <- legend_cr$c
  } else {
    legend_nrow <- 0
  }

  # Format titles, footnotes and sources
  footnotes <- formatfn(gg$footnotes, gg$plotsize[2] - WIDTHSPACESNOTES)
  sources <- formatsrcs(gg$sources, gg$plotsize[2] - WIDTHSPACESSOURCES)
  if (!is.null(gg$title)) {
    title <- splitoverlines(gg$title,
                            gg$plotsize[2] - MINIMUMSIDEPADDING,
                            28 / 20)
  } else {
    title <- NULL
  }
  if (!is.null(gg$subtitle)) {
    subtitle <- splitoverlines(gg$subtitle,
                               gg$plotsize[2] - MINIMUMSIDEPADDING,
                               1)
  } else {
    subtitle <- NULL
  }

  # Conform panel titles
  paneltitles <- conformpaneltitles(names(data),
                                    gg$paneltitles,
                                    gg$layout,
                                    gg$plotsize[2] - MINIMUMSIDEPADDING,
                                    1)
  panelsubtitles <- conformpaneltitles(names(data),
                                       gg$panelsubtitles,
                                       gg$layout,
                                       gg$plotsize[2] - MINIMUMSIDEPADDING,
                                       18 / 20)

  # Now need to start the canvas
  device <- finddevice(filename)
  margins <-
    figuresetup(
      filename,
      device,
      names(data),
      xlabels,
      yticks,
      xunits,
      yunits,
      title,
      subtitle,
      footnotes,
      sources,
      yaxislabels,
      xaxislabels,
      gg$legend_onpanel,
      legend_nrow,
      gg$plotsize,
      gg$portrait,
      gg$layout,
      gg$srt
    )
  handlelayout(gg$layout)

  # Plot each panel
  for (p in names(data)) {
    drawpanel(
      p,
      data[[p]],
      shading[[p]],
      gg$bgshading,
      margins,
      gg$layout,
      yunits[[p]],
      xunits[[p]],
      yticks[[p]],
      xlabels[[p]],
      c(ylim[[p]]$min, ylim[[p]]$max),
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
  # For easy alignment of legend, footnotes and sources just draw a unit square
  # over the graph
  graphics::par(mfrow = c(1, 1))
  graphics::par(mfg = c(1, 1))
  graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "",
                 xlim = c(0, 1), ylim = c(0, 1))

  drawtitle(title, subtitle)
  drawnotes(footnotes, sources, margins$notesstart)
  if (gg$legend) {
    if (!gg$legend_onpanel) {
      draw_outer_legend(data,
                        legend_ncol,
                        margins$xtickmargin,
                        length(xaxislabels) > 0)
    } else {
      draw_onpanel_legend(data, legend_ncol, gg$legend_x, gg$legend_y)
    }
  }
  handlelayout(gg$layout) # Put the correct layout back

  for (p in names(data)) {
    # Fraw all the annotations
    l <- getlocation(p, gg$layout)
    graphics::par(mfg = l)
    graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "",
                   xlim = xlim[[p]], ylim = c(ylim[[p]]$min, ylim[[p]]$max))

    drawannotationlines(gg$lines, p)
    drawarrows(arrows, p)
    drawlabels(labels, p)
  }


  if (gg$enable_autolabeller) {
    # Finally, if desired, run the autolabeller
    autolabel(gg, data, xlim, ylim, margins, labels, gg$autolabel_quiet,
              gg$arrow_lines, gg$arrow_bars, gg$ignore_existing_labels)
  }

  if (!is.null(device)) {
    grDevices::dev.off() # Close files, but not if we're using default device.
  }
}

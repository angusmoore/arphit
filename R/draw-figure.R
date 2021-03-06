font_family <- function() {
  if (names(grDevices::dev.cur()) == "pdf") {
    "ArialMT"
  } else if (.Platform$OS.type == "windows") {
    "sans"
  } else {
    "Arial"
  }
}

finddevice <- function(filename) {
  if (is.null(filename)) {
    NULL
  } else {
    filetype <- tools::file_ext(filename)
    if (filetype == "" &&
        substr(filename, nchar(filename) - 4, nchar(filename)) == ".emf+") {
      filetype <- "emf+"
    }
    if (filetype == "png" || filetype == "pdf" || filetype == "emf" ||
        filetype == "svg" || filetype == "emf+" || filetype == "xlsx") {
      filetype
    } else {
      stop(paste("Unsupported file type ", filetype, ".", sep = ""),
           call. = FALSE)
    }
  }
}

is.even <- function(x) {
  as.integer(x) %% 2 == 0
}

leftrightpadding <- function(yticks, yunits, panels, layout) {
  r <- 0
  l <- 0
  for (p in panels) {
    if (layout != "1h") {
      yticks_dropped <- yticks_to_draw(yticks[[p]], p, layout)
    } else {
      yticks_dropped <- yticks[[p]]
    }

    nc <- max(sapply(pretty_format_numbers(yticks_dropped), getstrwidth))
    nc <- max(nc, getstrwidth(yunits[[p]]))

    if (is.even(p)) {
      r <- max(r, nc)
    } else {
      l <- max(l, nc)
    }
  }

  list(left = l / CSI, right = r / CSI)
}

countsrclines <- function(sources) {
  if (sources$text == "") {
    0
  } else {
    1.7 + 1.1 * sum(stringr::str_count(sources$text, "\n"))
  }
}

countfnlines <- function(footnotes) {
  nf <- length(footnotes)
  extralines <- sum(stringr::str_count(footnotes, "\n"))
  return(1.3 * nf + 1.1 * extralines)
}

counttitlelines <- function(title, subtitle) {
  slines <- stringr::str_count(subtitle, "\n")
  if (is.null(title)) {
    tlines <- -0.9
  } else {
    tlines <- stringr::str_count(title, "\n")
  }
  top <- 1.9 * tlines

  if (is.null(subtitle)) {
    top <- top + 2.7
  } else {
    top <- top + 3.85 + 1.8 * slines
  }

  top
}

xticksize <- function(xlabels, layout, srt) {
  size <- 0
  for (p in names(xlabels)) {
    if (needxlabels(p, layout)) {
      xticks <- xlabels[[p]]$labels
      if (srt == 0) {
        size <- max(size,
                    max(sapply(xticks, getstrheight, USE.NAMES = FALSE),
                        na.rm = TRUE))
      } else if (srt == 90) {
        size <- max(size,
                    max(sapply(xticks, getstrwidth, USE.NAMES = FALSE),
                        na.rm = TRUE))
      } else {
        h <- sapply(xticks, getstrheight, USE.NAMES = FALSE)
        w <- sapply(xticks, getstrwidth, USE.NAMES = FALSE)
        size <- max(size,
                    max(sin(srt * pi / 180) * w + cos(srt * pi / 180) * h,
                        na.rm = TRUE))
      }
    }
  }

  size / CSI
}

getfigsize <- function(plotsize, top, bottom, left, right) {
  top <- top * CSI
  bottom <- bottom * CSI
  left <- left * CSI
  right <- right * CSI
  # Now apply the minimum left and right padding
  left <- max(left, MINIMUMSIDEPADDING)
  right <- max(right, MINIMUMSIDEPADDING)

  figwidth <- plotsize[2] + left + right
  figheight <- plotsize[1] + top + bottom

  list(
    height = figheight,
    width = figwidth,
    top = top,
    bottom = bottom,
    left = left,
    right = right
  )
}

figuresetup <- function(filename,
                        device,
                        panels,
                        xticks,
                        yticks,
                        xunits,
                        yunits,
                        title,
                        subtitle,
                        footnotes,
                        sources,
                        yaxislabels,
                        xaxislabels,
                        legend_onpanel,
                        legend_nrow,
                        plotsize,
                        portrait,
                        layout,
                        srt) {
  # Figure out margins
  if (layout != "1h") {
    lr_padding <- leftrightpadding(yticks, yunits, panels, layout)
    left <- lr_padding$left + 1
    right <- lr_padding$right + 1 # A bit of extra white spacing
  } else {
    left <- leftrightpadding(lapply(xticks, function(x) x$labels),
                             lapply(yunits, function(x) ""),
                             panels,
                             layout)$left + 2
    right <- max(1,
                 sapply(yunits, getstrwidth, USE.NAMES = FALSE) /
                   (2 * CSI) + 0.5)
  }

  if ((layout != "1h" && length(yaxislabels) > 0) ||
      (layout == "1h" && length(xaxislabels) > 0)) {
    left <- left + 1.4
  }
  if ((layout != "1h" && length(xaxislabels) > 0) ||
      (layout == "1h" && length(yaxislabels) > 0)) {
    notesstart <- 2
  } else {
    notesstart <- 0
  }
  if (legend_nrow > 0 && !legend_onpanel) {
    notesstart <- notesstart + (legend_nrow - 1) * 1.2 + 2.5
  }

  if (layout != "1h") {
    xtickmargin <- 1.8 + xticksize(xticks, layout, srt)
  } else {
    xtickmargin <- 1.8 + xticksize(lapply(yticks, function(x) list(labels = x)),
                                   layout,
                                   srt)
  }

  notesstart <- notesstart + xtickmargin

  bottom <- countfnlines(footnotes) + countsrclines(sources) + notesstart
  top <- counttitlelines(title, subtitle)

  if (portrait) {
    # Override whatever is set as plotsize
    plotsize <- PORTRAITSIZE
  }

  fig <- getfigsize(plotsize, top, bottom, left, right)
  createfigure(filename, device, fig, plotsize)

  list(
    top = top,
    bottom = bottom,
    left = left,
    right = right,
    xtickmargin = xtickmargin,
    notesstart = notesstart
  )
}

handlelayout <- function(layout) {
  if (layout %in% c("1", "1h")) {
    graphics::par(mfrow = c(1, 1))
  } else if (layout == "2h") {
    graphics::par(mfrow = c(2, 1))
  } else if (layout == "2v") {
    graphics::par(mfrow = c(1, 2))
  } else if (layout == "2b2") {
    graphics::par(mfrow = c(2, 2))
  } else if (layout == "3v") {
    graphics::par(mfrow = c(1, 3))
  } else if (layout == "3h") {
    graphics::par(mfrow = c(3, 1))
  } else if (layout == "3b2") {
    graphics::par(mfrow = c(3, 2))
  } else if (layout == "4h") {
    graphics::par(mfrow = c(4, 1))
  } else if (layout == "4b2") {
    graphics::par(mfrow = c(4, 2))
  } else {
    stop(paste("Unknown layout option ", layout,
               ". Options are 1, 1h, 2h, 2v, 2b2, 3v, 3h, 3b2, 4h, 4b2.",
               sep = ""),
         call. = FALSE)
  }
  graphics::par(cex = 1)
}

startdevice <- function(filename, device, figsize) {
  if (is.null(device)) {
    if (.Platform$OS.type == "windows") {
      # I use try catch because R can't always use the windows device even on
      # windows (e.g. when running examples, or during tests)
      tryCatch({
        grDevices::windows(width = figsize$width, height = figsize$height)
      },
      error = function(cond) {
        grDevices::dev.new(width = figsize$width, height = figsize$height)
      })
    } else {
      grDevices::dev.new(width = figsize$width, height = figsize$height)
    }
  } else if (device == "png") {
    grDevices::png(filename = filename,
                   width = figsize$width,
                   height = figsize$height,
                   units = "in", res = PNGDPI)
  } else if (device == "pdf") {
    grDevices::pdf(file = filename,
                   width = figsize$width,
                   height = figsize$height)
  } else if (device == "emf") {
    devEMF::emf(file = filename,
                width = figsize$width,
                height = figsize$height,
                emfPlus = FALSE)
  } else if (device == "emf+") {
    devEMF::emf(file = substr(filename, 1, nchar(filename) - 1),
                width = figsize$width,
                height = figsize$height,
                emfPlus = TRUE, custom.lty = FALSE)
  } else if (device == "svg") {
    grDevices::svg(filename = filename,
                   width = figsize$width,
                   height = figsize$height)
  }
}

createfigure <- function(filename, device, figsize, plotsize) {
  # Set  up the figure and plot
  startdevice(filename, device, figsize)

  # Check if we actually got that, or whether the device can't provide the
  # desired dimensions
  if (abs(grDevices::dev.size()[1] / figsize$width - 1) > 0.01) {
    message(
      paste0(
        "Needed figure width exceeds that which the display device could ",
        "provide. Chart will not display correctly."
      )
    )
  }
  if (abs(grDevices::dev.size()[2] / figsize$height - 1) > 0.01) {
    message(
      paste0(
        "Needed figure height exceeds that which the display device could ",
        "provide. Chart will not display correctly."
      )
    )
  }

  graphics::par(mai = c(figsize$bottom,
                        figsize$left,
                        figsize$top,
                        figsize$right))

  plotcorners <- c((figsize$left) / figsize$height,
                   1 - (figsize$right) / figsize$height,
                   (figsize$bottom) / figsize$height,
                   1 - (figsize$top) / figsize$height)
  graphics::par(pin = plotsize, plt = plotcorners)

  # Misc display things
  graphics::par(family = font_family(), xaxs = "i", yaxs = "i", ps = 20,
                cex.main = (28 / 20), cex.axis = 1, las = 1, lheight = 1)
  graphics::par(omi = c(figsize$bottom,
                        figsize$left,
                        figsize$top,
                        figsize$right))
}

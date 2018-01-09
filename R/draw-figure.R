finddevice <- function(filename) {
  if (is.null(filename)) {
    return(NULL)
  } else {
    filetype <- stringr::str_sub(filename, -3,-1)
    if (filetype == "png" || filetype == "pdf" || filetype == "emf") {
      return(filetype)
    } else {
      stop(paste("Unsupported file type ", filetype, ".", sep = ""))
    }
  }
}

is.even <- function(x) {
  return(as.integer(x) %% 2 == 0)
}

leftrightpadding <- function(ticks, scaleunits, panels) {
  R <- 0
  L <- 0
  for (p in names(panels$panels)) {
    nc <- max(graphics::strwidth(ticks[[p]], units = "inches"))
    nc <- max(nc, graphics::strwidth(scaleunits[[p]], units = "inches"))
    if (is.even(p)) {
      R <- max(R, nc)
    } else {
      L <- max(L, nc)
    }
  }
  return(list(left = L/CSI, right = R/CSI))
}

countsrclines <- function(sources) {
  if (sources$text == "") {
    return(0)
  } else {
    return(1.7 + 1.1*sum(stringr::str_count(sources$text, "\n")))
  }
}

countfnlines <- function(footnotes) {
  nf <- length(footnotes)
  extralines <- sum(stringr::str_count(footnotes, "\n"))
  return(3.5 + 1.1*(nf-1) + 1.1*extralines)
}

counttitlelines <- function(title, subtitle) {
  slines <- stringr::str_count(subtitle, "\n")
  if (is.null(title)) {
    tlines <- -0.9
  } else {
    tlines <- stringr::str_count(title, "\n")
  }
  top <- 1.9*tlines

  if (is.null(subtitle)) {
    top <- top + 2.7
  } else {
    top <- top + 3.85 + 1.8*slines
  }
  return(top)
}

getfigsize <- function(plotsize, top, bottom, left, right) {
  top <- top*CSI
  bottom <- bottom*CSI
  left <- left*CSI
  right <- right*CSI
  # Now apply the minimum left and right padding
  left <- max(left, MINIMUMSIDEPADDING)
  right <- max(right, MINIMUMSIDEPADDING)

  figwidth <- plotsize[2] + left + right
  figheight <- plotsize[1] + top + bottom
  return(list("height" = figheight, "width" = figwidth))
}

figuresetup <- function(filename, device, panels, ticks, scaleunits, title, subtitle, footnotes, sources, portrait) {
  # Figure out margins
  LRpadding <- leftrightpadding(ticks, scaleunits, panels)
  left <- 2 + 1.2*LRpadding$left
  right <- 2 + 1.2*LRpadding$right

  bottom <- countfnlines(footnotes) + countsrclines(sources)
  top <- counttitlelines(title, subtitle)

  if (portrait) {
    plotsize <- PORTRAITSIZE
  } else {
    plotsize <- LANDSCAPESIZE
  }

  fig <- getfigsize(plotsize, top, bottom, left, right)
  createfigure(filename, device, fig, plotsize, top, bottom, left, right)

  return(list("top" = top, "bottom" = bottom, "left" = left, "right" = right))
}

handlelayout <- function(layout) {
  if (layout == "1") {
    graphics::par(mfrow=c(1,1))
  } else if (layout == "2h") {
    graphics::par(mfrow=c(2,1))
    graphics::par(cex = 1)
  } else if (layout == "2v") {
    graphics::par(mfrow=c(1,2))
    graphics::par(cex = 1)
  } else if (layout == "2b2") {
    graphics::par(mfrow=c(2,2))
    graphics::par(cex = 1)
  } else {
    stop(paste("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2.", sep = ""))
  }
}

startdevice <- function(filename, device, figsize) {
  if (is.null(device)) {
    grDevices::dev.new(width = figsize$width, height = figsize$height)
  } else if (device == "png") {
    grDevices::png(filename = filename, width = figsize$width, height = figsize$height, units = "in", res = PNGDPI)
  } else if (device == "pdf") {
    grDevices::pdf(file = filename, width = figsize$width, height = figsize$height)
  } else if (device == "emf") {
    devEMF::emf(file = filename, width = figsize$width, height = figsize$height)
  }
}

createfigure <- function(filename, device, figsize, plotsize, top, bottom, left, right) {
  # Set  up the figure and plot
  startdevice(filename, device, figsize)

    # Check if we actually got that, or whether the device can't provide the desired dimensions
  if (abs(grDevices::dev.size()[1]/figsize$width-1) > 0.01) {
    message("Needed figure width exceeds that which the display device could provide. Chart will not display correctly.")
  }
  if (abs(grDevices::dev.size()[2]/figsize$height-1) > 0.01) {
    message("Needed figure height exceeds that which the display device could provide. Chart will not display correctly.")
  }

  graphics::par(mar= c(bottom, left, top, right))

  plotcorners <- c((left*CSI)/figsize$height, 1-(right*CSI)/figsize$height, (bottom*CSI)/figsize$height, 1-(top*CSI)/figsize$height)
  graphics::par(pin = plotsize, plt = plotcorners)

  # Misc display things
  graphics::par(family = "sans", xaxs = "i", yaxs = "i", ps = 20, cex.main = (28/20), cex.axis = 1, las = 1, lheight = 1)
  graphics::par(oma = c(bottom, left, top, right))
}

## Constructors

#' Add a title or panel title
#'
#' @param text A string for the title.
#' @param panel (optional) Specify a panel identifier to add a panel title instead of an overall graph title.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_title("Graph Title")
#' arphitgg(data) + agg_title("Panel title", panel = "1")
#'
#' @export
agg_title <- function(text, panel = NULL) {
  if (!is.null(panel)) {
    panel <- as.character(panel)
  }
  return(list(type = "title", text = text, panel = panel))
}

#' Add a subtitle or panel subtitle
#'
#' @param text A string for the subtitle.
#' @param panel (optional) Specify a panel identifier to add a panel subtitle instead of an overall graph subtitle.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_subtitle("Graph Subtitle")
#' arphitgg(data) + agg_subtitle("Panel subtitle", panel = "1")
#'
#' @export
agg_subtitle <- function(text, panel = NULL) {
  if (!is.null(panel)) {
    panel <- as.character(panel)
  }
  return(list(type = "subtitle", text = text, panel = panel))
}

#' Add units (for the y axis)
#'
#' @param units A string specifying the units.
#' @param panel (optional) Specify a panel identifier to add to a specific panel. If blank, units will be applied to all panels.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_units("index")
#' arphitgg(data) + agg_units("ppt", panel = "1")
#'
#' @export
agg_units <- function(units, panel = NULL) {
  if (!is.null(panel)) {
    panel <- as.character(panel)
  }
  return(list(type = "units", units = units, panel = panel))
}

#' Add units to the x axis (only works for scatter graphs)
#'
#' @param units A string specifying the units.
#' @param panel (optional) Specify a panel identifier to add to a specific panel. If blank, units will be applied to all panels.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_xunits("index")
#'
#' @export
agg_xunits <- function(units, panel = NULL) {
  if (!is.null(panel)) {
    panel <- as.character(panel)
  }
  return(list(type = "xunits", units = units, panel = panel))
}

#' Add an axis label to the y axis
#'
#' @param axislabel A string specifying the axis label
#' @param panel (optional) Specify a panel identifier to add to a specific panel. If blank, axis label will be applied to all panels.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_yaxislabel("Some y axis label")
#'
#' @export
agg_yaxislabel <- function(axislabel, panel = NULL) {
  if (!is.null(panel)) {
    panel <- as.character(panel)
  }
  return(list(type = "yaxislabel", axislabel = axislabel, panel = panel))
}

#' Add an axis label to the x axis
#'
#' @param axislabel A string specifying the axis label
#' @param panel (optional) Specify a panel identifier to add to a specific panel. If blank, axis label will be applied to all panels.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_xaxislabel("year")
#'
#' @export
agg_xaxislabel <- function(axislabel, panel = NULL) {
  if (!is.null(panel)) {
    panel <- as.character(panel)
  }
  return(list(type = "xaxislabel", axislabel = axislabel, panel = panel))
}

#' Add a source (or many sources)
#'
#' @param source A string, or vector of strings, to be added as sources
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_source("Source 1")
#' arphitgg(data) + agg_source(c("Source 1", "Source 2"))
#'
#' @export
agg_source <- function(source) {
  return(list(type = "source", source = source))
}

#' Add a footnote (or many footnotes)
#'
#' @param footnote A string, or vector of strings, to be added as footnotes.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_footnote("Here is a footnote")
#' arphitgg(data) + agg_footnote(c("Here is a footnote", "And a second one"))
#'
#' @export
agg_footnote <- function(footnote) {
  return(list(type = "footnote", footnote = footnote))
}

#' Add a label
#'
#' @param text The text to display on your plot
#' @param color The color of your text
#' @param x The x coordinate of the center of your label
#' @param y The y coordinate of the center of your label
#' @param panel Which panel should the label be placed on?
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_label("Here is a footnote", RBA["Red3"], 2003, 0.2, "1")
#'
#' @export
agg_label <- function(text, color, x, y, panel) {
  return(list(type = "label", text = text, color = color, x = x, y = y, panel = panel))
}

#' Add an arrow
#'
#' @param tail.x The x coordinate of the arrow tail
#' @param tail.y The y coordinate of the arrow tail
#' @param head.x The x coordinate of the arrow head
#' @param head.y The y coordinate of the arrow head
#' @param color The color of the arrow
#' @param panel Which panel should the arrow be placed on?
#' @param lwd (Optional, default 1) The linewidth of the arrow
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_arrow(tail.x = 2002, tail.y = 0, head.x = 2003, head.y = 1, color = RBA["Blue1"], panel = "1")
#'
#' @export
agg_arrow <- function(tail.x, tail.y, head.x, head.y, color, panel, lwd = 1) {
  return(list(type = "arrow", tail.x = tail.x, tail.y = tail.y, head.x = head.x,
              head.y = head.y, color = color, panel = panel, lwd = lwd))
}

#' Add an AB line to your graph
#'
#' You need specify only one of x, or y, or x1,x2,y1,y2
#'
#' @param x Draw a vertical line at x (omit to draw a specific AB line)
#' @param y Draw a horizontal at y omit to draw a specific AB line)
#' @param x1 For specific AB lines: the first x coordinate
#' @param y1 For specific AB lines: the first y coordinate
#' @param x2 For specific AB lines: the second x coordinate
#' @param y2 For specific AB lines: the second y coordinate
#' @param color (optional) The color of the AB line (default black)
#' @param panel Which panel should the line be placed on?
#' @param lwd (Optional, default 1) The linewidth
#' @param lty (Optional, default 1) The line type
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_abline(x = 2001, color = RBA["Blue1"], panel = "1") +
#'   agg_abline(y = -0.5, color = RBA["Red1"], panel = "1")
#'
#' arphitgg(data) + agg_abline(x1 = 2000, y1 = -0.1, x2 = 2002, y2 = 0.5, color = RBA["Blue1"], panel = "1")
#'
#' @export
agg_abline <- function(x = NULL, y = NULL, x1 = NULL, y1 = NULL, x2 = NULL, y2 = NULL, color = NULL, panel, lwd = 1, lty = 1) {
  return(list(type = "abline", x = x, y = y, x1 = x1, y1 = y1, x2 = x2, y2 = y2,
              color = color, panel = panel, lwd = lwd, lty = lty))
}

#' Add background shading
#'
#' @param x1 The bottom left x coordinate (omit to have the shading automatically snap to the edge of the panel)
#' @param y1 The bottom left y coordinate (omit to have the shading automatically snap to the edge of the panel)
#' @param x2 The top right x coordinate (omit to have the shading automatically snap to the edge of the panel)
#' @param y2 The top right y coordinate (omit to have the shading automatically snap to the edge of the panel)
#' @param color (optional) The color of the AB line (default grey)
#' @param panel Which panel should the background shading be placed on?
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_bgshading(x1 = 2001, x2 = 2002, panel = "1")
#' arphitgg(data) + agg_bgshading(y1 = 0.5, y2 = -0.5, panel = "1")
#'
#' @export
agg_bgshading <- function(x1 = NA, y1 = NA, x2 = NA, y2 = NA, color = NULL, panel) {
  return(list(type = "bgshading", x1 = x1, y1 = y1, x2 = x2, y2 = y2, color = color, panel = panel))
}



#' Add shading between series
#'
#' @param from The series name to shade from (if you have no group aesthetic, it will be the name of the y variable); if you have groups, it will (usually) be the group identifier. This can get more complicated if you have duplicate group names. If so, arphit appends .y to the group names, so try that.)
#' @param to The name of the series to shade to
#' @param color (optional) The color to shade between the series (default grey)
#' @param panel (optional) Which panel are the relevant series in? (arphit will try to find them if you don't specify)
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' data <- data.frame(date = seq.Date(from = as.Date("2000-03-10"), length.out = 12, by = "month"),
#'                    x1 = rnorm(12), x2 = rnorm(12))
#' arphitgg(data, agg_aes(x = date)) + agg_line(agg_aes(y = x1), color = RBA["Blue2"]) +
#'    agg_line(agg_aes(y = x2), color = RBA["Red4"]) +
#'    agg_shading(from = x1, to = x2)
#'
#' @export
agg_shading <- function(from, to, panel = NULL, color = NULL) {
  from <- as.character(deparse(substitute(from)))
  to <- as.character(deparse(substitute(to)))
  return(list(type = "shading", from = from, to = to, panel = panel, color = color))
}

#' Specify the y scale for a graph
#'
#' @param min The minimum for the y-axis
#' @param max The maximum
#' @param nsteps The number of steps (includes the minimum and maximum)
#' @param panel (optional) Which panel to apply the scale to. If left blank, will be applied to all panels.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data, layout = "2b2") + agg_ylim(min = -10, max = 10, nsteps = 5)
#' arphitgg(data, layout = "2b2") + agg_ylim(min = -10, max = 10, nsteps = 5, panel = "1")
#'
#' @export
agg_ylim <- function(min, max, nsteps, panel = NULL) {
  return(list(type = "ylim", min = min, max = max, nsteps = nsteps, panel = panel))
}

#' Specify the x limits for a graph
#'
#' @param min The minimum for the x-axis
#' @param max The maximum
#' @param panel (optional) Which panel to apply the scale to. If left blank, will be applied to all panels. This is recommended.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' arphitgg(data) + agg_xlim(min = -10, max = 10)
#' arphitgg(data) + agg_xlim(min = -10, max = 10, panel = "1")
#'
#' @export
agg_xlim <- function(min, max, panel = NULL) {
  return(list(type = "xlim", min = min, max = max, panel = panel))
}

#' Add a line layer to an arphit plot.
#'
#' @param data The data to be used. Will inherit from parent if missing.
#' @param aes The aesthetic that defines the layer. Will inherit (or parts thereof) if omitted.
#' @param color A colour to be applied to all of the series, or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements.
#' @param pch A point marker to be applied to all series, or or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements. Any value accepted by R for pch can be used.
#' @param lty A line type to be applied to all series, or or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements. Any value accepted by R for lty can be used.
#' @param lwd A line width to be applied to all series, or or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements. Any value accepted by R for lwd can be used.
#' @param panel (default = "1") Which panel of the graph to place this layer on.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' data  <- data.frame(unemployment = rnorm(20), state = c(rep("A", 10), rep("B", 10)),
#'   date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))
#' arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), panel = "1")
#'
#' @export
agg_line <- function(aes = NULL, data = NULL, color = NULL, pch = NULL, lty = NULL, lwd = NULL, panel = "1") {
  return(list(type = "line", data = data, aes = aes, color = color, pch = pch, lty = lty, lwd = lwd, panel = as.character(panel)))
}

#' Add a col layer to an arphit plot.
#'
#' @param data The data to be used. Will inherit from parent if missing.
#' @param aes The aesthetic that defines the layer. Will inherit (or parts thereof) if omitted.
#' @param color A colour to be applied to all of the series, or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements.
#' @param barcol (optional) Outline colours for each bar series
#' @param panel (default = "1") Which panel of the graph to place this layer on.
#' @param stacked (default = TRUE) Stack the bars, or group them?
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' data  <- data.frame(unemployment = rnorm(20), state = c(rep("A", 10), rep("B", 10)),
#'   date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))
#' arphitgg(data) + agg_col(aes = agg_aes(x = date, y = unemployment, group = state), panel = "1")
#'
#' @export
agg_col <- function(aes = NULL, data = NULL, color = NULL, barcol = NULL, panel = "1", stacked = TRUE) {
  return(list(type = "col", data = data, aes = aes, color = color, barcol = barcol, panel = as.character(panel), stacked = stacked))
}

#' Add a scatter layer to an arphit plot.
#'
#' @param data The data to be used. Will inherit from parent if missing.
#' @param aes The aesthetic that defines the layer. Will inherit (or parts thereof) if omitted.
#' @param color A colour to be applied to all of the series, or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements.
#' @param panel (default = "1") Which panel of the graph to place this layer on.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' data  <- data.frame(x = rnorm(10), y = rnorm(10))
#' arphitgg(data) + agg_point(aes = agg_aes(x = x, y = y), panel = "1")
#'
#' @export
agg_point <- function(aes = NULL, data = NULL, color = NULL, panel = "1") {
  return(list(type = "point", data = data, aes = aes, color = color, panel = as.character(panel)))
}

#' Define an aesthetic for a graph, or a graph layer.
#'
#' If specified as part of a agg_line or agg_col, fields left blank will be inherited from the parent.
#'
#' @param x Which series is the x variable.
#' @param y Which series are you plotting on the y axis.
#' @param group If your data are in long form, which variable defines the groups.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @export
agg_aes <- function(x, y, group = NULL) {
  x <- as.character(deparse(substitute(x)))
  y <- as.character(deparse(substitute(y)))
  group <- as.character(deparse(substitute(group)))
  if (x == "NULL" || x == "") {
    x <- NULL
  }
  if (y == "NULL" || y == "") {
    y <- NULL
  }
  if (group == "NULL" || group == "") {
    group <- NULL
  }
  return(list(type = "aes", x = x, y = y, group = group))
}

#' Create an arphit graph to be built using the ggplot-like interface.
#'
#' @param data (Optional) Data to be used for the plot. Can be left blank, but must then be supplied for each layer.
#' @param aes (Optional) The aesthetic that defines your graph. Can be left blank, but must then be supplied for each layer. Layers that don't specify aesthetics will inherit missing parts of aesthetic from here.
#' @param layout (default = "1")
#' @param portrait (default = false) Logical indicating whether the layout should be a landscape size (FALSE, default), or a taller portrait size (TRUE).
#' @param dropxlabel (optional) Logical indicating whether the first xlabel of right hand panels in 2v and 2b2 should be ignored (prevents overlapping of last xlabel on left panel with first on right). FALSE by default.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @export
arphitgg <- function(data = NULL, aes = NULL, layout = "1", portrait = FALSE, dropxlabel = FALSE) {
  gg <- list(data = list(parent = data),
             aes = aes,
             x = list(),
             series = list(),
             layout = as.character(layout),
             bars = list(),
             title = NULL,
             subtitle = NULL,
             paneltitles = list(),
             panelsubtitles = list(),
             yaxislabels = list(),
             xaxislabels = list(),
             footnotes = c(),
             sources = c(),
             yunits = NULL,
             xunits = NULL,
             ylim = list(),
             xlim = list(),
             col = list(),
             pch = list(),
             lty = list(),
             lwd = list(),
             barcol = list(),
             labels = list(),
             arrows = list(),
             lines = list(),
             bgshading = list(),
             shading = list(),
             portrait = portrait,
             dropxlabel = dropxlabel,
             stacked = TRUE)

  class(gg) <- "arphit.gg"
  return(gg)
}

## Combining

convert2wide <- function(data, aes) {
  if (!is.null(aes$group)) {
    return(tidyr::spread_(data, key = aes$group, value = aes$y))
  } else {
    return(data)
  }
}

subsetdata <- function(data, x, y, group) {
  if (!is.null(group)) {
    return(dplyr::select_(data, x, y, group))
  } else {
    return(dplyr::select_(data, x, y))
  }
}

getnewcolumns <- function(old, combined) {
  allnames <- colnames(combined)
  originalnames <- colnames(old)
  return(allnames[!(allnames %in% originalnames)])
}

unrename <- function(data) {
  for (col in colnames(data)) {
    if (substr(col, nchar(col)-1, nchar(col)) == ".x") {
      colnames(data)[colnames(data) == col] <- substr(col, 1, nchar(col)-2)
    }
  }
  return(data)
}

applyattribute <- function(gg, attributename, panel, newseriesnames, attributevalues) {
  i <- 1
  if (is.null(gg[[attributename]][[panel]])) {
    gg[[attributename]][[panel]] <- list()
  }
  for (name in newseriesnames) {
    gg[[attributename]][[panel]][[name]] <- attributevalues[i]
    i <- i %% length(attributevalues) + 1
  }
  return(gg)
}

addnewseries <- function(gg, new, panel) {
  # if aes is new, inherit from parent
  if (is.null(new$aes)) {
    new$aes <- gg$aes
  }
  # Check all the parts
  if (is.null(new$aes$x) && !is.null(gg$aes$x)) {
    new$aes$x <- gg$aes$x
  }
  if (is.null(new$aes$y) && !is.null(gg$aes$y)) {
    new$aes$y <- gg$aes$y
  }
  if (is.null(new$aes$group) && !is.null(gg$aes$group)) {
    new$aes$group <- gg$aes$group
  }

  # if data is null, inherit from parent
  if (is.null(new$data)) {
    new$data <- gg$data[["parent"]]
    if (is.null(new$data)) {
      stop("You have not supplied data for series")
    }
  }

  # Figure out the x variable
  if (!is.null(gg$x[[panel]])) {
    # Have previously set an x variable. Check is the same.
    if (gg$x[[panel]] != new$aes$x) {
      stop(paste("You cannot add a series to panel ", panel, " with x variable ", new$aes$x, " because you have already added a series to that panel with x variable ", gg$x[[panel]], sep = ""))
    }
  } else {
    gg$x[[panel]] <- new$aes$x
  }

  # handle data
  newdata <- subsetdata(new$data, new$aes$x, new$aes$y, new$aes$group)
  newdata <- convert2wide(newdata, new$aes)
  if (!is.null(gg$data[[panel]])) {
    # We have already added a series for this panel, so we need to merge the new data on to the old
    mergeddata <- dplyr::full_join(gg$data[[panel]], newdata, by = gg$x[[panel]])
    mergeddata <- unrename(mergeddata)
    newseriesnames <- getnewcolumns(gg$data[[panel]], mergeddata)
    gg$data[[panel]] <- mergeddata
  } else {
    gg$data[[panel]] <- newdata
    newseriesnames <- colnames(newdata)[colnames(newdata) != gg$x[[panel]]]
  }

  if (!is.null(new$color)) {
    gg <- applyattribute(gg, "col", panel, newseriesnames, new$color)
  }


  return(list(gg = gg, newseriesnames = newseriesnames))
}

addlineseries <- function(gg, newline) {
  panel <- newline$panel
  out <- addnewseries(gg, newline, panel)
  gg <- out$gg
  newseriesnames <- out$newseriesnames
  if (!is.null(newline$pch)) {
    gg <- applyattribute(gg, "pch", panel, newseriesnames, newline$pch)
  }
  if (!is.null(newline$lty)) {
    gg <- applyattribute(gg, "lty", panel, newseriesnames, newline$lty)
  }
  if (!is.null(newline$lwd)) {
    gg <- applyattribute(gg, "lwd", panel, newseriesnames, newline$lwd)
  }
  return(gg)
}

addpointseries <- function(gg, newpoint) {
  # Just create a line series with PCH and LTY set
  newpoint$pch <- 16
  newpoint$lty <- 0
  return(addlineseries(gg, newpoint))
}

addcolseries <- function(gg, newcol) {
  panel <- newcol$panel
  out <- addnewseries(gg, newcol, panel)
  gg <- out$gg
  newcolnames <- out$newseriesnames

  if (!is.null(gg$bars[[panel]])) {
    gg$bars[[panel]] <- append(gg$bars, newcolnames)
  } else {
    gg$bars[[panel]] <- newcolnames
  }

  if (!is.null(newcol$stacked)) {
    gg$stacked <- newcol$stacked
  }
  if (!is.null(newcol$barcol)) {
    gg <- applyattribute(gg, "barcol", panel, newcolnames, newcol$barcol)
  }

  return(gg)
}

addtitle <- function(gg, title) {
  if (is.null(title$panel)) {
    gg$title <- title$text
  } else {
    gg$paneltitles[[title$panel]] <- title$text
  }
  return(gg)
}

addsubtitle <- function(gg, subtitle) {
  if (is.null(subtitle$panel)) {
    gg$subtitle <- subtitle$text
  } else {
    gg$panelsubtitles[[subtitle$panel]] <- subtitle$text
  }
  return(gg)
}

addunits <- function(gg, units) {
  if (is.null(units$panel)) {
    gg$yunits <- units$units
  } else {
    if (is.null(gg$yunits)) {
      gg$yunits <- list()
    }
    gg$yunits[[units$panel]] <- units$units
  }
  return(gg)
}

addxunits <- function(gg, units) {
  if (is.null(units$panel)) {
    gg$xunits <- units$units
  } else {
    if (is.null(gg$xunits)) {
      gg$xunits <- list()
    }
    gg$xunits[[units$panel]] <- units$units
  }
  return(gg)
}

addsource <- function(gg, source) {
  gg$sources <- append(gg$sources, source$source)
  return(gg)
}

addfootnote <- function(gg, footnote) {
  gg$footnotes <- append(gg$footnotes, footnote$footnote)
  return(gg)
}

addannotation <- function(gg, annotation, element) {
  type <- annotation$type
  annotation$type <- NULL
  gg[[element]] <- append(gg[[element]], list(annotation))
  return(gg)
}

addshading <- function(gg, shading) {
  shading$type <- NULL
  if (is.null(shading$panel)) {
    shading$panel <- NULL
  }
  if (is.null(shading$color)) {
    shading$color <- NULL
  }
  gg$shading <- append(gg$shading, list(shading))
  return(gg)
}

addylim <- function(gg, ylim) {
  ylim$type <- NULL
  if (is.null(ylim$panel)) {
    ylim$panel <- NULL
    gg$ylim <- ylim
  } else {
    p <- ylim$panel
    ylim$panel <- NULL
    gg$ylim[[p]] <- ylim
  }
  return(gg)
}

addxlim <- function(gg, xlim) {
  xlim$type <- NULL
  if (is.null(xlim$panel)) {
    xlim$panel <- NULL
    gg$xlim <- c(xlim$min, xlim$max)
  } else {
    p <- xlim$panel
    xlim$panel <- NULL
    gg$xlim[[p]] <- c(xlim$min, xlim$max)
  }
  return(gg)
}

addaxislabel <- function(gg, axislabel, axis) {
  index <- paste0(axis, "axislabels")
  if (is.null(axislabel$panel)) {
    gg[[index]] <- axislabel$axislabel
  } else {
    gg[[index]][[axislabel$panel]] <- axislabel$axislabel
  }
  return(gg)
}

## Wrap up

#' Draw a defined graph
#'
#' @param gg An arphitgg built graph.
#' @param filename (optional) If specified, save image to filename instead of displaying in R. Supports pdf, emf and png extensions.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @export
agg_draw <- function(gg, filename = NULL) {
  # Here we call the arphit drawing function
  gg$data[["parent"]] <- NULL
  arphit(data = gg$data,
         x = gg$x,
         layout = gg$layout,
         bars = gg$bars,
         title = gg$title,
         subtitle = gg$subtitle,
         paneltitles = gg$paneltitles,
         panelsubtitles = gg$panelsubtitles,
         yaxislabels = gg$yaxislabels,
         xaxislabels = gg$xaxislabels,
         footnotes = gg$footnotes,
         sources = gg$sources,
         yunits = gg$yunits,
         ylim = gg$ylim,
         xlim = gg$xlim,
         col = gg$col,
         pch = gg$pch,
         lty = gg$lty,
         lwd = gg$lwd,
         barcol = gg$barcol,
         labels = gg$labels,
         arrows = gg$arrows,
         lines = gg$lines,
         bgshading = gg$bgshading,
         shading = gg$shading,
         portrait = gg$portrait,
         dropxlabel = gg$dropxlabel,
         bar.stacked = gg$stacked,
         filename = filename)
}

#' Draw a defined graph
#'
#' @param x An arphitgg built graph.
#' @param ... Further arguments passed to or from other methods.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @export
print.arphit.gg <- function(x, ...) {
  agg_draw(x)
}

#' Add a layer or element to an arphitgg graph.
#'
#' @param gg An arphitgg built graph.
#' @param element The element to add to the graph.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @export
"+.arphit.gg" <- function(gg, element) {
  gg = switch(element$type,
         "line" = addlineseries(gg, element),
         "col" = addcolseries(gg, element),
         "point" = addpointseries(gg, element),
         "title" = addtitle(gg, element),
         "subtitle" = addsubtitle(gg, element),
         "units" = addunits(gg, element),
         "xunits" = addxunits(gg, element),
         "source" = addsource(gg, element),
         "footnote" = addfootnote(gg, element),
         "label" = addannotation(gg, element, "labels"),
         "arrow" = addannotation(gg, element, "arrows"),
         "abline" = addannotation(gg, element, "lines"),
         "bgshading" = addannotation(gg, element, "bgshading"),
         "shading" = addshading(gg, element),
         "ylim" = addylim(gg, element),
         "xlim" = addxlim(gg, element),
         "yaxislabel" = addaxislabel(gg, element, "y"),
         "xaxislabel" = addaxislabel(gg, element, "x"),
         stop("Unknown element type for arphit.gg"))
  return(gg)
}

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

#' Add a legend to the graph
#'
#' @param ncol (optional) Specify the number of columns in the legend (if left blank, arphit will guess)
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' data <- data.frame(x = 1:10, y = 1:10)
#' arphitgg(data) + agg_line(agg_aes(x = x, y = y)) + agg_legend()
#'
#' @export
agg_legend <- function(ncol = NULL) {
  return(list(type = "legend", ncol = ncol))
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
#' @param facet If you data are in long form, which variable defines the facets (facets split data across panels, see the gg-interface vignette for example)
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @export
agg_aes <- function(x, y, group = NULL, facet = NULL) {
  x <- as.character(deparse(substitute(x)))
  y <- as.character(deparse(substitute(y)))
  group <- as.character(deparse(substitute(group)))
  facet <- as.character(deparse(substitute(facet)))
  if (x == "NULL" || x == "") {
    x <- NULL
  }
  if (y == "NULL" || y == "") {
    y <- NULL
  }
  if (group == "NULL" || group == "") {
    group <- NULL
  }
  if (facet == "NULL" || facet == "") {
    facet <- NULL
  }
  return(list(type = "aes", x = x, y = y, group = group, facet = facet))
}

#' Create an arphit graph to be built using the ggplot-like interface.
#'
#' @param data (Optional) Data to be used for the plot. Can be left blank, but must then be supplied for each layer.
#' @param aes (Optional) The aesthetic that defines your graph. Can be left blank, but must then be supplied for each layer. Layers that don't specify aesthetics will inherit missing parts of aesthetic from here.
#' @param layout (default = "1") The layout of the graph. Valid options are "1", "2v", "2h", "2b2", "3v", "3h", "3b2", "4h", "4b2".
#' @param portrait (default = false) Logical indicating whether the layout should be a landscape size (FALSE, default), or a taller portrait size (TRUE).
#' @param dropxlabel (optional) Logical indicating whether the first xlabel of right hand panels in 2v and 2b2 should be ignored (prevents overlapping of last xlabel on left panel with first on right). FALSE by default.
#' @param srt (default 0) Orientation adjustment for xlabels. In degrees; 0 is horizontal.
#' @param showallxlabels (optional) (Only for categorical graphs) Force all x labels to show? By default, this is false for numeric categorical and true for non-numeric categorical.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @export
arphitgg <- function(data = NULL, aes = NULL, layout = "1", portrait = FALSE, dropxlabel = FALSE, srt = 0, showallxlabels = NULL) {
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
             legend = FALSE,
             legend.ncol = NA,
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
             stacked = TRUE,
             srt = srt,
             showallxlabels = showallxlabels)

  class(gg) <- "arphit.gg"
  return(gg)
}


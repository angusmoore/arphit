check_panel <- function(panel) {
  if (!all(panel %in% as.character(1:8))) {
    stop(paste0("Panel identifier '", panel, "' is invalid. Panels must be between 1 and 8."),
         call. = FALSE)
  }
}

#' Add a title or panel title
#'
#' @param text A string for the title.
#' @param panel (optional) Specify a panel identifier to add a panel title instead of an overall graph title.  You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the title to multiple panels at once.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
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
  if (length(text) > 1) {
    stop("text for title should be a single character, not a vector",
         call. = FALSE)
  }
  if (!is.null(panel)) check_panel(panel)
  if (is.null(text) || is.na(text)) text <- NULL
  return(list(type = "title", text = text, panel = panel))
}

#' Add a subtitle or panel subtitle
#'
#' @param text A string for the subtitle.
#' @param panel (optional) Specify a panel identifier to add a panel subtitle instead of an overall graph subtitle. You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the subtitle to multiple panels at once.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
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
  if (length(text) > 1) {
    stop("text for subtitle should be a single character, not a vector",
         call. = FALSE)
  }
  if (!is.null(panel)) check_panel(panel)
  if (is.null(text) || is.na(text)) text <- NULL
  return(list(type = "subtitle", text = text, panel = panel))
}

#' Add units (for the y axis)
#'
#' @param units A string specifying the units.
#' @param panel (optional) Specify a panel identifier to add to a specific panel. If blank, units will be applied to all panels.  You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the units to multiple panels at once.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
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
  if (!is.null(panel)) check_panel(panel)
  return(list(type = "units", units = units, panel = panel))
}

#' Add units to the x axis (only works for scatter graphs)
#'
#' @param units A string specifying the units.
#' @param panel (optional) Specify a panel identifier to add to a specific panel. If blank, units will be applied to all panels.  You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the units to multiple panels at once.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' arphitgg(data) + agg_xunits("index")
#'
#' @export
agg_xunits <- function(units, panel = NULL) {
  if (!is.null(panel)) {
    panel <- as.character(panel)
  }
  if (!is.null(panel)) check_panel(panel)
  return(list(type = "xunits", units = units, panel = panel))
}

#' Add an axis label to the y axis
#'
#' @param axislabel A string specifying the axis label
#' @param panel (optional) Specify a panel identifier to add to a specific panel. If blank, axis label will be applied to all panels. You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the axis label to multiple panels at once.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' arphitgg(data) + agg_yaxislabel("Some y axis label")
#'
#' @export
agg_yaxislabel <- function(axislabel, panel = NULL) {
  if (!is.null(panel)) {
    panel <- as.character(panel)
  }
  if (!is.null(panel)) check_panel(panel)
  return(list(type = "yaxislabel", axislabel = axislabel, panel = panel))
}

#' Add an axis label to the x axis
#'
#' @param axislabel A string specifying the axis label
#' @param panel (optional) Specify a panel identifier to add to a specific panel. If blank, axis label will be applied to all panels. You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the axis to multiple panels at once.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' arphitgg(data) + agg_xaxislabel("year")
#'
#' @export
agg_xaxislabel <- function(axislabel, panel = NULL) {
  if (!is.null(panel)) {
    panel <- as.character(panel)
  }
  if (!is.null(panel)) check_panel(panel)
  return(list(type = "xaxislabel", axislabel = axislabel, panel = panel))
}

#' Add a source (or many sources)
#'
#' @param source A string, or vector of strings, to be added as sources
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' arphitgg(data) + agg_source("Source 1")
#' arphitgg(data) + agg_source(c("Source 1", "Source 2"))
#'
#' @export
agg_source <- function(source) {
  if (!is.null(source)) {
    source <- source[!is.na(source)]
  }
  return(list(type = "source", source = source))
}

#' Add a footnote (or many footnotes)
#'
#' @param footnote A string, or vector of strings, to be added as footnotes.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' arphitgg(data) + agg_footnote("Here is a footnote")
#' arphitgg(data) + agg_footnote(c("Here is a footnote", "And a second one"))
#'
#' @export
agg_footnote <- function(footnote) {
  if (!is.null(footnote)) {
    footnote <- footnote[!is.na(footnote)]
  }
  return(list(type = "footnote", footnote = footnote))
}

#' Add a label
#'
#' @param text The text to display on your plot
#' @param x The x coordinate of the center of your label
#' @param y The y coordinate of the center of your label
#' @param panel Which panel should the label be placed on? You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the label to multiple panels at once.
#' @param colour The colour of your text
#' @param size Font size (default 20)
#' @param color (Deprecated; use colour instead) The colour of your text
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' arphitgg() + agg_label("Here is a label", 2003, 0.2, "1", RBA["Red3"])
#'
#' @export
agg_label <- function(text, x, y, panel, colour = "black", size = 20, color) {
  check_panel(panel)
  if (!missing(color)) {
    warning("color is deprecated; use colour instead")
    colour <- color
  }
  return(list(type = "label", text = text, colour = colour, x = x, y = y, panel = panel, cex = size / 20))
}

#' Add automatically placed label
#'
#' @param quiet (logical, default FALSE) Do not output progress of autolabeller
#' @param arrow_lines (logical, default TRUE) Add arrows between labels and line series
#' if there isn't line of sight between the label and the series.
#' @param arrow_bars (logical, default FALSE) Add arrows between labels and bar series
#' if there isn't line of sight between the label and the series.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' data <- data.frame(x=1:10,y1=rnorm(10),y2=rnorm(10))
#' arphitgg(data,agg_aes(x=x)) +
#'   agg_line(agg_aes(y=y1)) +
#'   agg_line(agg_aes(y=y2)) +
#'   agg_autolabel()
#'
#' @export
agg_autolabel <- function(quiet = FALSE, arrow_lines = TRUE, arrow_bars = FALSE) {
  return(list(type = "autolabel", quiet = quiet, arrow_lines = arrow_lines, arrow_bars = arrow_bars))
}

#' Add an arrow
#'
#' @param tail.x The x coordinate of the arrow tail
#' @param tail.y The y coordinate of the arrow tail
#' @param head.x The x coordinate of the arrow head
#' @param head.y The y coordinate of the arrow head
#' @param colour The colour of the arrow
#' @param panel Which panel should the arrow be placed on? You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the arrow to multiple panels at once.
#' @param lwd (Optional, default 1) The linewidth of the arrow
#' @param color (Deprecated; use colour instead) The colour of the arrow
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' arphitgg(data) +
#'   agg_arrow(tail.x = 2002, tail.y = 0, head.x = 2003, head.y = 1,
#'             color = RBA["Blue1"], panel = "1")
#'
#' @export
agg_arrow <- function(tail.x, tail.y, head.x, head.y, colour = "black", panel, lwd = 1, color) {
  check_panel(panel)
  if (!missing(color)) {
    warning("color is deprecated; use colour instead")
    colour <- color
  }
  return(list(type = "arrow", tail.x = tail.x, tail.y = tail.y, head.x = head.x,
              head.y = head.y, colour = colour, panel = panel, lwd = lwd))
}

#' Add a line between two points on your graph
#'
#' @param x (Deprecated; use agg_vline instead) Draw a vertical line at x (omit to draw a specific AB line)
#' @param y (Deprecated; use agg_hline instead) Draw a horizontal at y omit to draw a specific AB line)
#' @param x1 The first x coordinate
#' @param y1 The first y coordinate
#' @param x2 The second x coordinate
#' @param y2 The second y coordinate
#' @param colour The colour of the AB line (default black)
#' @param panel Which panel should the line be placed on? You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the line to multiple panels at once.
#' @param lwd (Optional, default 1) The line width
#' @param lty (Optional, default 1) The line type  (uses R line types)
#' @param color (Deprecated; use colour instead) The colour of the AB line (default black)
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' arphitgg(data) + agg_abline(x = 2001, colour = RBA["Blue1"], panel = "1") +
#'   agg_abline(y = -0.5, colour = RBA["Red1"], panel = "1")
#'
#' arphitgg(data) +
#'   agg_abline(x1 = 2000, y1 = -0.1, x2 = 2002, y2 = 0.5,
#'              colour = RBA["Blue1"], panel = "1")
#'
#' @export
agg_abline <- function(x = NULL, y = NULL, x1, y1, x2, y2, colour = "black", panel, lwd = 1, lty = 1, color) {
  if (!missing(color)) {
    warning("color is deprecated; use colour instead")
    colour <- color
  }
  if (!is.null(x)) {
    warning("`agg_abline` with only x to draw a vertical line is deprecated. Use `agg_vline` instead.")
    return(agg_vline(x = x, colour = colour, panel = panel, lwd = lwd, lty = lty))
  }
  if (!is.null(y)) {
    warning("`agg_abline` with only y to draw a horizontal line is deprecated. Use `agg_hline` instead.")
    return(agg_hline(y = y, colour = colour, panel = panel, lwd = lwd, lty = lty))
  }
  if (missing(x1)) stop("Line is missing x1")
  if (missing(y1)) stop("Line is missing y1")
  if (missing(x2)) stop("Line is missing x2")
  if (missing(y2)) stop("Line is missing y2")
  line <- list(x1 = x1, y1 = y1, x2 = x2, y2 = y2, colour = colour, panel = panel, lwd = lwd, lty = lty)
  check_panel(panel)
  return(append(line, list(type = "abline")))
}

#' Add a vertical line to your graph
#'
#' @param x The x coordinate to draw the vertical line at
#' @param colour The colour of the line (default black)
#' @param panel Which panel should the line be placed on? You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the line to multiple panels at once.
#' @param lwd (Optional, default 1) The line width
#' @param lty (Optional, default 1) The line type (uses R line types)
#'
#' @export
#'
#' @examples
#' arphitgg() + agg_vline(x=2003,panel="1")
agg_vline <- function(x, colour = "black", panel, lwd = 1, lty = 1) {
  check_panel(panel)
  return(list(x1 = x, x2 = x, y1 = NA, y2 = NA, panel = panel, lwd = lwd, lty = lty, colour = colour, type = "abline"))
}

#' Add a horizontal line to your graph
#'
#' @param y The y coordinate to draw the vertical line at
#' @param colour The colour of the line (default black)
#' @param panel Which panel should the line be placed on? You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the line to multiple panels at once.
#' @param lwd (Optional, default 1) The line width
#' @param lty (Optional, default 1) The line type (uses R line types)
#'
#' @export
#'
#' @examples
#' arphitgg() + agg_hline(y=0.7,panel="1")
agg_hline <- function(y, colour = "black", panel, lwd = 1, lty = 1) {
  check_panel(panel)
  return(list(y1 = y, y2 = y, x1 = NA, x2 = NA, panel = panel, lwd = lwd, lty = lty, colour = colour, type = "abline"))
}

#' Add background shading
#'
#' @param x1 The bottom left x coordinate (omit to have the shading automatically snap to the edge of the panel)
#' @param y1 The bottom left y coordinate (omit to have the shading automatically snap to the edge of the panel)
#' @param x2 The top right x coordinate (omit to have the shading automatically snap to the edge of the panel)
#' @param y2 The top right y coordinate (omit to have the shading automatically snap to the edge of the panel)
#' @param colour (optional) The colour of the background shading (default grey)
#' @param panel Which panel should the background shading be placed on? You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the shading to multiple panels at once.
#' @param color (Deprecated; use colour instead) The colour of the background shading (default grey)
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' arphitgg(data) + agg_bgshading(x1 = 2001, x2 = 2002, panel = "1")
#' arphitgg(data) + agg_bgshading(y1 = 0.5, y2 = -0.5, panel = "1")
#'
#' @export
agg_bgshading <- function(x1 = NA, y1 = NA, x2 = NA, y2 = NA, colour = RBA["Grey2"], panel, color) {
  check_panel(panel)
  if (!missing(color)) {
    warning("color is deprecated; use colour instead")
    colour <- color
  }
  return(list(type = "bgshading", x1 = x1, y1 = y1, x2 = x2, y2 = y2, colour = colour, panel = panel))
}

#' Add shading between series
#'
#' @param from The series name to shade from (if you have no group aesthetic, it will be the name of the y variable); if you have groups, it will (usually) be the group identifier. This can get more complicated if you have duplicate group names. If so, arphit appends .y to the group names, so try that.)
#' @param to The name of the series to shade to
#' @param panel (optional) Which panel are the relevant series in? (arphit will try to find them if you don't specify)
#' @param colour (optional) The colour to shade between the series (default grey)
#' @param color (Deprecated; use colour instead) The colour to shade between the series (default grey)
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' data <- data.frame(date = seq.Date(from = as.Date("2000-03-10"), length.out = 12, by = "month"),
#'                    x1 = rnorm(12), x2 = rnorm(12))
#' arphitgg(data, agg_aes(x = date)) + agg_line(agg_aes(y = x1), colour = RBA["Blue2"]) +
#'    agg_line(agg_aes(y = x2), colour = RBA["Red4"]) +
#'    agg_shading(from = x1, to = x2)
#'
#' @export
agg_shading <- function(from, to, panel = NULL, colour = RBA["Grey2"], color) {
  from <- as.character(deparse(substitute(from)))
  to <- as.character(deparse(substitute(to)))
  if (!is.null(panel)) check_panel(panel)
  if (!missing(color)) {
    warning("color is deprecated; use colour instead")
    colour <- color
  }
  return(list(type = "shading", from = from, to = to, panel = panel, colour = colour))
}

#' Specify the y scale for a graph
#'
#' @param min The minimum for the y-axis
#' @param max The maximum
#' @param nsteps The number of steps (includes the minimum and maximum)
#' @param panel (optional) Which panel to apply the scale to. If left blank, will be applied to all panels. You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the scale to multiple panels at once.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' arphitgg(data, layout = "2b2") + agg_ylim(min = -10, max = 10, nsteps = 5)
#' arphitgg(data, layout = "2b2") + agg_ylim(min = -10, max = 10, nsteps = 5, panel = "1")
#'
#' @export
agg_ylim <- function(min, max, nsteps, panel = NULL) {
  if (nsteps < 2) {
    stop("The y-limit you supplied has fewer than 2 points.", call. = FALSE)
  }
  if (!is.null(panel)) check_panel(panel)
  return(list(type = "ylim", min = min, max = max, nsteps = nsteps, panel = panel))
}

#' Specify the x limits for a graph
#'
#' @param min The minimum for the x-axis (can be NA to have arphit guess, based on your data)
#' @param max The maximum (can also be NA, as above)
#' @param panel (optional) Which panel to apply the scale to. If left blank, will be applied to all panels. This is recommended. You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the limits to multiple panels at once.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' arphitgg(data) + agg_xlim(min = -10, max = 10)
#' arphitgg(data) + agg_xlim(min = -10, max = 10, panel = "1")
#'
#' @export
agg_xlim <- function(min, max, panel = NULL) {
  if (!is.null(panel)) check_panel(panel)
  return(list(type = "xlim", min = min, max = max, panel = panel))
}

#' Specify the frequency of the x ticks for time series graphs
#'
#' @param freq The frequency, either "decade", "year", "quarter", "month"
#' @param panel (optional) Which panel to apply the frequency scale to. If left blank, will be applied to all panels. You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the frequency to multiple panels at once.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @export
agg_xaxisfreq <- function(freq, panel = NULL) {
  if (!is.null(panel)) check_panel(panel)
  if (!freq %in% c("decade","year","quarter","month")) {
    stop(paste0(freq, " is not a valid frequency"), call. = FALSE)
  }
  return(list(type = "xfreq", freq = freq, panel = panel))
}

#' Add a legend to the graph
#'
#' @param ncol (optional) Specify the number of columns in the legend (if left blank, arphit will guess)
#' @param x (optional) Specify a location _on_ the plot for the legend. If omitted, the legend
#' is added beneath the graph. x can be one of "bottomright", "bottom", "bottomleft", "left",
#' "topleft", "top", "topright", "right" and "center" to have the legend automatically
#' placed in that location. Alternatively, you can supply a value between 0 and 1
#' (and a y coordinate) to place a legend in a specific place on the graph. (0,0)
#' corresponds to the bottom left corner, (1,1) top right. Multipanels are ignored
#' for the purposes of on-panel legends - graphs are treated as a whole and panels
#' are ignored.
#' @param y (optional) Only required if x is a numeric. y-location for on-panel legend.
#' Must be between 0 and 1. See above for more detail.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' data <- data.frame(x = 1:10, y = 1:10)
#' arphitgg(data) + agg_line(agg_aes(x = x, y = y)) + agg_legend()
#'
#' @export
agg_legend <- function(ncol = NULL, x = NULL, y = NULL) {
  if (!is.null(x)) {
    if (is.numeric(x) && (is.null(y))) stop("You must specify a y coordinate if you specify an x coordinate for on panel legends", call. = FALSE)
    if (is.character(x)) {
      y <- NULL
      if (!x %in% c("bottomright", "bottom", "bottomleft", "left", "topleft",
                    "top", "topright", "right", "center")) {
        stop("Valid options for automatic placement of on panel legend are bottomright, bottom, bottomleft, left, topleft, top, topright, right and center", call. = FALSE)
      }
    }
  }
  return(list(type = "legend", ncol = ncol, onpanel = !is.null(x), x = x, y = y))
}

#' Add a line layer to an arphit plot.
#'
#' @param data The data to be used. Will inherit from parent if missing.
#' @param aes The aesthetic that defines the layer. Will inherit (or parts thereof) if omitted.
#' @param colour A colour to be applied to all of the series, or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements.
#' @param pch A point marker to be applied to all series, or or (if your aesthetic has a group), a vector of pch values that will be cycled through to consecutive group elements. Any value accepted by R for pch can be used.
#' @param lty A line type to be applied to all series, or or (if your aesthetic has a group), a vector of lty values that will be cycled through to consecutive group elements. Any value accepted by R for lty can be used.
#' @param lwd A line width to be applied to all series, or or (if your aesthetic has a group), a vector of lwd values that will be cycled through to consecutive group elements. Any value accepted by R for lwd can be used.
#' @param pointsize Scale the size of the points? (default 1)
#' @param panel (default = "1") Which panel of the graph to place this layer on. You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the layer to multiple panels at once.
#' @param color (Deprecated; use colour instead) A colour to be applied to all of the series, or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' data  <- data.frame(unemployment = rnorm(20), state = c(rep("A", 10), rep("B", 10)),
#'   date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))
#' arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), panel = "1")
#'
#' @export
agg_line <- function(aes = NULL, data = NULL, colour = NULL, pch = NULL, lty = NULL, lwd = NULL, pointsize = 1, panel = "1", color) {
  check_panel(panel)
  if (!missing(color)) {
    warning("color is deprecated; use colour instead")
    colour <- color
  }
  return(
    list(
      type = "line",
      data = data,
      aes = aes,
      colour = colour,
      pch = pch,
      lty = lty,
      lwd = lwd,
      pointsize = pointsize,
      panel = as.character(panel)
    )
  )
}

#' Add a step line layer to an arphit plot.
#'
#' @param data The data to be used. Will inherit from parent if missing.
#' @param aes The aesthetic that defines the layer. Will inherit (or parts thereof) if omitted.
#' @param colour A colour to be applied to all of the series, or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements.
#' @param pch A point marker to be applied to all series, or or (if your aesthetic has a group), a vector of pch values that will be cycled through to consecutive group elements. Any value accepted by R for pch can be used.
#' @param lty A line type to be applied to all series, or or (if your aesthetic has a group), a vector of lty values that will be cycled through to consecutive group elements. Any value accepted by R for lty can be used.
#' @param lwd A line width to be applied to all series, or or (if your aesthetic has a group), a vector of lwd values that will be cycled through to consecutive group elements. Any value accepted by R for lwd can be used.
#' @param pointsize Scale the size of the points? (default 1)
#' @param panel (default = "1") Which panel of the graph to place this layer on. You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the layer to multiple panels at once.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' data  <- data.frame(unemployment = rnorm(20), state = c(rep("A", 10), rep("B", 10)),
#'   date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))
#' arphitgg(data) + agg_step(aes = agg_aes(x = date, y = unemployment, group = state), panel = "1")
#'
#' @export
agg_step <- function(aes = NULL, data = NULL, colour = NULL, pch = NULL, lty = NULL, lwd = NULL, pointsize = 1, panel = "1") {
  check_panel(panel)
  return(
    list(
      type = "step",
      data = data,
      aes = aes,
      colour = colour,
      pch = pch,
      lty = lty,
      lwd = lwd,
      pointsize = pointsize,
      panel = as.character(panel)
    )
  )
}

#' Add a col layer to an arphit plot.
#'
#' @param data The data to be used. Will inherit from parent if missing.
#' @param aes The aesthetic that defines the layer. Will inherit (or parts thereof) if omitted.
#' @param colour A colour to be applied to all of the series, or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements.
#' @param barcol (optional) Outline colours for each bar series
#' @param panel (default = "1") Which panel of the graph to place this layer on. You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the layer to multiple panels at once.
#' @param stacked (default = TRUE) Stack the bars, or group them?
#' @param color (Deprecated; use colour instead) A colour to be applied to all of the series, or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' data  <- data.frame(unemployment = rnorm(20), state = c(rep("A", 10), rep("B", 10)),
#'   date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))
#' arphitgg(data) + agg_col(aes = agg_aes(x = date, y = unemployment, group = state), panel = "1")
#'
#' @export
agg_col <- function(aes = NULL, data = NULL, colour = NULL, barcol = NULL, panel = "1", stacked = TRUE, color) {
  check_panel(panel)
  if (!missing(color)) {
    warning("color is deprecated; use colour instead")
    colour <- color
  }
  return(
    list(
      type = "col",
      data = data,
      aes = aes,
      colour = colour,
      barcol = barcol,
      panel = as.character(panel),
      stacked = stacked
    )
  )
}

#' Add a scatter layer to an arphit plot.
#'
#' @param data The data to be used. Will inherit from parent if missing.
#' @param aes The aesthetic that defines the layer. Will inherit (or parts thereof) if omitted.
#' @param colour A colour to be applied to all of the series, or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements.
#' @param pointsize Scale the size of the points? (default 1)
#' @param panel (default = "1") Which panel of the graph to place this layer on. You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the layer to multiple panels at once.
#' @param color (Deprecated; use colour instead) A colour to be applied to all of the series, or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' data  <- data.frame(x = rnorm(10), y = rnorm(10))
#' arphitgg(data) + agg_point(aes = agg_aes(x = x, y = y), panel = "1")
#'
#' @export
agg_point <- function(aes = NULL, data = NULL, colour = NULL, pointsize = 1, panel = "1", color) {
  check_panel(panel)
  if (!missing(color)) {
    warning("color is deprecated; use colour instead")
    colour <- color
  }
  return(
    list(
      type = "line",
      data = data,
      aes = aes,
      colour = colour,
      pointsize = pointsize,
      pch = 16,
      lty = 0,
      panel = as.character(panel)
    )
  )
}

#' Add a waterfall layer to an arphit plot.
#'
#' Creates a bar chart, showing how the changes from left-most observation to the
#' right-most observation.
#'
#' You should ensure that your data are ordered so that your first observation is
#' the observation you want the waterfall to 'start' from, and your last
#' observation the one you want the waterfall to 'end' at. Do this by specifying
#' an `order` in your aesthetic.
#'
#' Setting groups (and colours) is a good idea: create a group to differentiate
#' your start and end observations and your positive and negative observations (
#' separately colouring positive and negative makes the graph easier to read).
#'
#' This layer type should be considered experimental and may undergo breaking
#' changes.
#'
#' @param aes The aesthetic that defines the layer. Will inherit (or parts thereof) if omitted.
#' @param data The data to be used. Will inherit from parent if missing.
#' @param colour A colour to be applied to all of the series, or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements.
#' @param barcol (optional) Outline colours for each bar series
#' @param panel (default = "1") Which panel of the graph to place this layer on. You can specify a vector of panels (e.g. `panel = c("1","3")`) to apply the layer to multiple panels at once.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @examples
#' # Simple waterfall graph
#' data  <- data.frame(x = letters[1:6], y = c(2,1,-0.5,-0.2,0.4,2.7))
#' arphitgg(data) + agg_waterfall(agg_aes(x=x,y=y,order=x))
#'
#' # Waterfall graph separately colouring positive and negative changes
#' data <- data.frame(x = letters[1:6], y = c(2,1,-0.5,-0.2,0.4,2.7))
#' arphitgg(data) + agg_waterfall(agg_aes(x=x,y=y,group=y<0,order=x))
#'
#' # Waterfall graphs with multiple groups per x observation
#' data <- data.frame(x = c('start','a','a','b','b','end'),
#'                    y = c(1, 0.5, -0.4, 0.2, 0.1, 1.4),
#'                    group = c(1, 2, 3, 2, 3, 4),
#'                    order = c(1,2,2,3,3,4))
#' arphitgg(data) +
#'   agg_waterfall(agg_aes(x=x,y=y,group=group,order=order))
#'
#' @export
agg_waterfall <- function(aes = NULL, data = NULL, colour = NULL, barcol = NULL, panel = "1") {
  check_panel(panel)
  return(
    list(
      type = "waterfall",
      data = data,
      aes = aes,
      colour = colour,
      barcol = barcol,
      panel = as.character(panel)
    )
  )
}

#' Define an aesthetic for a graph, or a graph layer.
#'
#' If specified as part of a layer, fields left blank will be inherited from the parent.
#'
#' @param x The x variable
#' @param y The y variable
#' @param group If your data are in long form, which variable defines the groups
#' @param facet If you data are in long form, which variable defines the facets (facets split data across panels)
#' @param order Which variable to order the x-ticks by
#'
#' @section Tidy evaluation:
#'
#' Aesthetics use tidy evaluation. This means any of can be expressions composed
#' of variables in the data, rather than just variable names. For instance, you
#' could do `order = desc(some_variable)`, or `y = my_variable^2`.
#'
#' The plotting options vignette provides some examples of aesthetics
#' using tidy evaluation.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @export
agg_aes <- function(x, y, group = NULL, facet = NULL, order = NULL) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)
  facet <- rlang::enquo(facet)
  if (length(deparse(substitute(order))) > 0 && as.character(deparse(substitute(order))) == "NULL") {
    order <- x
  } else {
    order <- rlang::enquo(order)
  }

  return(list(type = "aes", x = x, y = y, group = group, facet = facet, order = order))
}

#' Create an new arphit graph
#'
#' @param data (Optional) Data to be used for the plot. Can be left blank, but must then be supplied for each layer.
#' @param aes (Optional) The aesthetic that defines your graph. Can be left blank, but must then be supplied for each layer. Layers that don't specify aesthetics will inherit missing parts of aesthetic from here.
#' @param layout (default = "1") The layout of the graph. Valid options are "1", "2v", "2h", "2b2", "3v", "3h", "3b2", "4h", "4b2".
#' @param portrait (default = FALSE) Logical indicating whether the layout should be a landscape size (FALSE, default), or a taller portrait size (TRUE).
#' @param dropxlabel (optional) Logical indicating whether the first xlabel of right hand panels in 2v and 2b2 should be ignored (prevents overlapping of last xlabel on left panel with first on right). FALSE by default.
#' @param srt (default 0) Orientation adjustment for xlabels. In degrees; 0 is horizontal.
#' @param showallxlabels (optional) (Only for categorical graphs) Force all x labels to show? By default, this is TRUE.
#' @param joined (default TRUE) Logical indicating whether you want to join between missing observations (TRUE()), or break the series (FALSE).
#' @param plotsize (optional) A vector of two variables specifying the height and width of your graph, respectively. (Default 5.53, 7.5). Ignored if portrait is set to TRUE
#' @param log_scale (optional) Set one or both of y and x axes as log scales? Use "y" for just y axis, "x" for just x and "xy" for both
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @export
arphitgg <- function(data = NULL, aes = NULL, layout = "1", portrait = FALSE, dropxlabel = NA, srt = 0, showallxlabels = TRUE, joined = TRUE, plotsize = LANDSCAPESIZE, log_scale = "") {
  gg <- list(data = list(parent = data),
             aes = aes,
             x = list(),
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
             xfreq = list(),
             legend = FALSE,
             legend.ncol = NA,
             col = list(),
             pch = list(),
             lty = list(),
             lwd = list(),
             pointsize = list(),
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
             showallxlabels = showallxlabels,
             joined = joined,
             plotsize = plotsize,
             enable_autolabeller = FALSE,
             autolabel_quiet = FALSE,
             log_scale = log_scale)

  class(gg) <- "arphit.gg"
  return(gg)
}

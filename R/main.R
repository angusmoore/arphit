#' RBA-style time series graphs in R
#'
#' Creates a (potentially multipanel) graph. Supports bar and line (and combinations of).
#'
#' @param data Object containing the series you want to plot. Can be a data.frame, tibble or ts. Can also be a list of the above, with separate for each panel.
#' @param series (optional) A list of vectors of string names of the series from data you wish to plot. The keys in the list must be "1", "2", etc to indicate which panel the series should be plotted on.
#'   Panel numbers are sequential left-to-right, top-to-bottom. For one-panel and top-and-bottom two panel, the right axis is counted as a separate panel. Thus, for a top-and-bottom, panels "1" and "2" are the top panel, left and right respectively; likewise "3" and "4" for the bottom panel.
#'   You do not need to supply all panels. For instance, in a one-panel, there is no need to supply series for panel "2" if you want all series on the left hand side axis.
#'   Alternatively if just a vector of string names is supplied, it will be assumed all series are being plotted in panel 1. Similarly, if series is not supplied at all, all series will be plotted in panel 1.
#' @param x (optional) A list specifying x variables for each of your panels. ts objects passed in as data will automatically use dates.
#' @param layout (optional) A string indicating the layout of the chart. Valid options are "1" (single panel), "2v" (side-by-side two panel), "2h" (top and bottom two panel), and "2b2" two-by-two four panel chart. Defaults to single panel if not supplied.
#' @param bars (optional) Vector of string names indicating which series should be bars, rather than lines. Alternatively, if you set `bars = TRUE` all series will plot as bars.
#' @param filename (optional) If specified, save image to filename instead of displaying in R. Supports pdf, emf and png extensions.
#' @param shading (optional) List of shading between series. See the plotting options vignette for information on how to use.
#' @param title (optional) A string indicating the title for the entire chart. Passing NULL (or omitting the argument) will suppress printing of title.
#' @param subtitle (optional) A string indicating the subtitle for the entire chart. Passing NULL (or omitting the argument) will suppress printing of subtitle.
#' @param paneltitles (optional) A list of string -> string pairs indicating panel titles. Keys must be "1", "2", etc to indicate which panel the title is for.
#' @param panelsubtitles (optional) A list string -> string pairs indicating panel titles. See paneltitles.
#' @param footnotes (optional) A vector strings, corresponding to the footnotes, in order.
#' @param sources (optional) A vector of strings, one entry for each source.
#' @param scaleunits (optional) A list of string -> string pairs indicating the units to be used on each panel (/axes, see notes to series for explanation on how right-hand-side axes are treated). Alternatively, providing just a string will assign that to all panels. If not supplied, a per cent sign will be used.
#' @param labels (optional) A list of lists specifying series labels to add to the plot. Each sub-list specifies a single text label. These must be of the form: list(x = 2008, y = -1, text = "Label text", panel = 1, color = "black"). Where x and y specify where (in the units on the plot) the centre of the label should be. panel specifies which panel to place the label on.
#' @param arrows (optional) A list of lists specifying arrows to add to the plot. Each sub-list specifies a single arrow. These must be of the form: list(tail.x = 2000, tail.y = -1, head.x = 2001, head.y = 0, panel = 1, lwd = 1, color = "black"). tail.x and tail.y specify the coordinates (in the units on the plot) of where to start the arrow at, head.x and head.y where to finish it. panel specifies which panel to place the arrow on. lwd is optional and specifies the linewidth of the arrow (default = 1).
#' @param bgshading (optional) A list of lists specifying background shading to add to the plot. Each of the sublists corresponds to a rectangle of background shading to draw. Each must be specified list(x1 = NA, y1 = -1, x2 = NA, y2 = 1, panel = 1, color = "lightgrey"). x1 and x2 correspond to the bottom left corner; x2, y2 the top right. Passing NA will set that coordinate to the axis limit - this is useful for creating shading that stretches across a whole panel. color and panel are self-explanatory (color is optional and defaults to light grey).
#' @param lines (optional) A list of lists specifying line segments to add to the plot. Each of the sublists specifies a single line segment to add. These can be specified in three ways. If you want to specify a vertical line that stretches across the panel use list(x = 2001, panel = 1, color = "green", lty = 1). x is where, on the x-axis, the line will be drawn. panel specifies which panel to draw on. color is self-explanatory and is optional (default black). lty specifies what line type to use (see R lty options) - it is optional and defaults to 1 (solid). To draw a horizontal line use list(y = 1, panel = 1). To draw a specific line between two points use list(x1 = 2000, y1 = -1, x2 = 2001, y2 = 0, panel = 1) instead of x and y. All other options are the same.
#' @param col (optional) A list of string -> misc pairs. The keys should be series names, and the values colours for each series (any colour accepted by R is fine.) You need not supply colours for all series. Default colours will be assigned  (cycling through) to series without assigned colours. Alternatively, you can supply a single value to apply to all series.
#' @param pch (optional) A list of string -> int pairs. The keys should be series names, and the values markers (pch values) for each series. Defaults to none (NA). Can be supplied as list, or a single value (which will be applied to all series).
#' @param lty (optional) A list of string -> int pairs. The keys should be series names, and the values line types (lty values) for each series. Defaults to solid (1). Can be supplied as list, or a single value (which will be applied to all series).
#' @param lwd (optional) A list of string -> numeric pairs. The keys should be series names, and the values line width, relative to default, for each series. Defaults to 1. Can be supplied as list, or a single value (which will be applied to all series).
#' @param barcol (optional) A list of string -> misc pairs.  The keys should be series names, and the values outline colours for each bar series (any colour accepted by R is fine.) You need not supply colours for all series. Bar series without assigned colours are given no outline by default. Alternatively, you can supply a single value to apply to all series.
#' @param xlim (optional) c(numeric, numeric) Gives the x limits (in years) for the graph. Alternatively, you can supply a list to provide different x limits for each panel (not recommended). If unsupplied, a suitable default is chosen (recommended).
#' @param ylim (optional) A list of string -> list(min = numeric, max = numeric, nsteps int) pairs. Keys are panel names (e.g. "1", "2", etc). Values are the scale, provided as a list with the keys min, max and nsteps. If unsupplied, a suitable default is chosen (recommended, but will not work well for multipanels).
#' @param portrait (optional) Logical indicating whether the layout should be a landscape size (FALSE, default), or a taller portrait size (TRUE).
#' @param bar.stacked (optional) Logical indicating whether the bar series should be stacked (TRUE, default) or side-by-side (FALSE).
#' @param dropxlabel (optional) Logical indicating whether the first xlabel of right hand panels in 2v and 2b2 should be ignored (prevents overlapping of last xlabel on left panel with first on right). TRUE by default.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options and how they affect the output.
#'
#' @examples
#' T <- 24
#' randomdata <- ts(data.frame(x1 = rnorm(T), x2 = rnorm(T), x3 = rnorm(T, sd = 10),
#'   x4 = rnorm(T, sd = 5)), start = c(2000,1), frequency = 4)
#' arphit(randomdata, series = list("1" = c("x1", "x2"), "2" = c("x3", "x4")),
#'   bars = c("x3","x4"), layout = "2v", title = "A Title", subtitle = "A subtitle",
#'   footnotes = c("a","B"), sources = c("A Source", "Another source"), scaleunits = "index")
#'
#' @export
arphit <- function(data, series = NULL, x = NULL, layout = "1", bars = NULL, filename = NULL, shading = NULL, title = NULL, subtitle = NULL, paneltitles = NULL, panelsubtitles = NULL, footnotes = NULL, sources = NULL, scaleunits = NULL, labels = NULL, arrows = NULL, bgshading = NULL, lines = NULL, col = NULL, pch = NULL, lty = 1, lwd = 2, barcol = NA, xlim = NULL, ylim = NULL, portrait = FALSE, bar.stacked = TRUE, dropxlabel = TRUE) {

  out <- handledata(series, data, x)
  data <- out$data
  series <- out$series

  # Determine the x values for each panel
  xvars <- handlex(data, x)

  # Handle panels
  panels <- handlepanels(series, layout)
  bars <- handlebars(panels, bars)

  # handle series attributes
  attributes <- handleattributes(panels, col, pch, lty, lwd, barcol)

  # Units and scales
  scaleunits <- handleunits(panels, scaleunits, layout)
  ylim <- ylimconform(panels, ylim, data, layout)
  ticks <- handleticks(data, panels, ylim)
  xlim <- xlimconform(panels, xlim, xvars, data)
  xlabels <- handlexlabels(panels, xlim, xvars, data)

  # Handle shading
  shading <- handleshading(shading, panels)

  # handle annotations
  labels <- sanitychecklabels(labels)
  arrows <- sanitycheckarrows(arrows)
  bgshading <- sanitycheckbgshading(bgshading)
  lines <- sanitychecklines(lines)

  # Format footnotes and sources
  footnotes <- formatfn(footnotes)
  sources <- formatsrcs(sources)
  # TODO split titles if they're too long

  # Conform panel titles
  paneltitles <- conformpaneltitles(panels, paneltitles)
  panelsubtitles <- conformpaneltitles(panels, panelsubtitles)

  # Now need to start the canvas
  device <- finddevice(filename)
  margins <- figuresetup(filename, device, panels, ticks, scaleunits, title, subtitle, footnotes, sources, portrait)
  handlelayout(layout)

  # Plot each panel
  for (p in names(panels)) {
    drawpanel(p, panels[[p]], bars[[p]], data[[p]], xvars[[p]], !is.null(xvars[[paste(p,"ts",sep="")]]), shading, bgshading, margins, layout, portrait, attributes[[p]], scaleunits, ticks, xlabels, ylim[[p]], xlim[[p]], paneltitles[[p]], panelsubtitles[[p]], bar.stacked, dropxlabel)
  }

  # Draw outer material
  drawtitle(title, subtitle)
  drawnotes(footnotes, sources)

  for (p in names(panels)) {
    # Finally, draw all the annotations draw all
    l <- getlocation(p ,layout)
    graphics::par(mfg = l)
    graphics::plot(0, lwd = 0, pch = NA, axes = FALSE, xlab = "", ylab = "", xlim = xlim[[p]], ylim = c(ylim[[p]]$min, ylim[[p]]$max))
    drawannotationlines(lines, p)
    drawarrows(arrows, p)
    drawlabels(labels, p)
  }

  if (!is.null(device)) {
    grDevices::dev.off() # Close files, but not if we're using default device.
  }
}

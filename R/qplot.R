conformdata <- function(data, layout, series) {
  if (is.acceptable.data(data)) {
    # Only a single data set, not a list of data
    tmpdata <- data
    data <- list()

    if (stats::is.ts(tmpdata)) {
      agg_time <- as.Date(lubridate::date_decimal(as.numeric(stats::time(tmpdata))))
      tmpdata <- tibble::as_tibble(tmpdata)
      tmpdata$agg_time <- agg_time
    } else if (zoo::is.zoo(tmpdata) || xts::is.xts(tmpdata)) {
      agg_time <- stats::time(tmpdata)
      tmpdata <- tibble::as_tibble(tmpdata)
      tmpdata$agg_time <- agg_time
    }

    if (!is.null(series)) {
      for (p in names(series)) {
        data[[as.character(p)]] <- tmpdata
      }
    } else {
      data[["1"]] <- tmpdata
    }

  } else if (!is.list(data)) {
    stop("Data is of unknown form.")
  }
  return(data)
}

conformxvariable <- function(x, data, layout) {
  if (!is.list(x) && !is.null(x)) {
    tmpx <- x
    x <- list()
    for (p in 1:maxpanels(layout)) {
      x[[as.character(p)]] <- tmpx
    }
  } else if (is.null(x)) {
    # Check for if we gave a time series, and need to make agg_time
    x <- list()
    for (p in names(data)) {
      if ("agg_time" %in% colnames(data[[p]])) {
        x[[p]] <- "agg_time"
      } else {
        stop(paste("You did not specify an x variable for panel", p))
      }
    }
  }
  return(x)
}

#' RBA-style graphs in R
#'
#' Quickly creates a (potentially multipanel) graph. Supports bar and line (and combinations of).
#'
#' @param data Object containing the series you want to plot. Can be a data.frame, tibble or ts. Can also be a list of the above, with separate for each panel.
#' @param series (optional) A list of vectors of string names of the series from data you wish to plot. The keys in the list must be "1", "2", etc to indicate which panel the series should be plotted on.
#'   Panel numbers are sequential left-to-right, top-to-bottom. For one-panel and top-and-bottom two panel, the right axis is counted as a separate panel. Thus, for a top-and-bottom, panels "1" and "2" are the top panel, left and right respectively; likewise "3" and "4" for the bottom panel.
#'   You do not need to supply all panels. For instance, in a one-panel, there is no need to supply series for panel "2" if you want all series on the left hand side axis.
#'   Alternatively if just a vector of string names is supplied, it will be assumed all series are being plotted in panel 1. Similarly, if series is not supplied at all, all series will be plotted in panel 1.
#' @param x (optional) A list specifying x variables for each of your panels. ts objects passed in as data will automatically use dates.
#' @param layout (optional) A string indicating the layout of the chart. Valid options are "1" (single panel), "2v" (side-by-side two panel), "2h" (top and bottom two panel), "2b2" (two-by-two four panel chart), "3v" (side-by-side three panel), "3h" (horizonal three panel), "3b2" (three-by-two six panel chart), "4h" (horizonal four panel) and "4b2" (four-by-two eight panel). Defaults to single panel if not supplied.
#' @param bars (optional) Vector of string names indicating which series should be bars, rather than lines. Alternatively, if you set `bars = TRUE` all series will plot as bars.
#' @param filename (optional) If specified, save image to filename instead of displaying in R. Supports pdf, emf and png extensions.
#' @param title (optional) A string indicating the title for the entire chart. Passing NULL (or omitting the argument) will suppress printing of title.
#' @param subtitle (optional) A string indicating the subtitle for the entire chart. Passing NULL (or omitting the argument) will suppress printing of subtitle.
#' @param paneltitles (optional) A list of string -> string pairs indicating panel titles. Keys must be "1", "2", etc to indicate which panel the title is for.
#' @param panelsubtitles (optional) A list string -> string pairs indicating panel titles. See paneltitles.
#' @param footnotes (optional) A vector strings, corresponding to the footnotes, in order.
#' @param sources (optional) A vector of strings, one entry for each source.
#' @param yunits (optional) A list of string -> string pairs indicating the units to be used on each panel (/axes, see notes to series for explanation on how right-hand-side axes are treated). Alternatively, providing just a string will assign that to all panels. If not supplied, a per cent sign will be used.
#' @param col (optional) A list of string -> misc pairs. The keys should be series names, and the values colours for each series (any colour accepted by R is fine.) You need not supply colours for all series. Default colours will be assigned  (cycling through) to series without assigned colours. Alternatively, you can supply a single value to apply to all series.
#' @param pch (optional) A list of string -> int pairs. The keys should be series names, and the values markers (pch values) for each series. Defaults to none (NA). Can be supplied as list, or a single value (which will be applied to all series).
#' @param lty (optional) A list of string -> int pairs. The keys should be series names, and the values line types (lty values) for each series. Defaults to solid (1). Can be supplied as list, or a single value (which will be applied to all series).
#' @param lwd (optional) A list of string -> numeric pairs. The keys should be series names, and the values line width, relative to default, for each series. Defaults to 1. Can be supplied as list, or a single value (which will be applied to all series).
#' @param barcol (optional) A list of string -> misc pairs.  The keys should be series names, and the values outline colours for each bar series (any colour accepted by R is fine.) You need not supply colours for all series. Bar series without assigned colours are given no outline by default. Alternatively, you can supply a single value to apply to all series.
#' @param xlim (optional) c(numeric, numeric) Gives the x limits (in years) for the graph. Alternatively, you can supply a list to provide different x limits for each panel (not recommended). If unsupplied, a suitable default is chosen (recommended).
#' @param ylim (optional) A list of string -> list(min = numeric, max = numeric, nsteps int) pairs. Keys are panel names (e.g. "1", "2", etc). Values are the scale, provided as a list with the keys min, max and nsteps. If unsupplied, a suitable default is chosen (recommended, but will not work well for multipanels).
#' @param legend A logical indicating whether to add a legend to the graph (default FALSE).
#' @param legend.ncol How many columns do you want the legend to have (if NA, which is the default, arphit will guess for you).
#' @param portrait (optional) Logical indicating whether the layout should be a landscape size (FALSE, default), or a taller portrait size (TRUE).
#' @param bar.stacked (optional) Logical indicating whether the bar series should be stacked (TRUE, default) or side-by-side (FALSE).
#' @param dropxlabel (optional) Logical indicating whether the first xlabel of right hand panels in 2v , 3v, 2b2, 3b2, and 4b2 should be ignored (prevents overlapping of last xlabel on left panel with first on right). FALSE by default.
#' @param joined (optiona) Logical indicating whether you want to join between missing observations (TRUE()), or break the series (FALSE). TRUE by default.
#' @param srt (default 0) Orientation adjustment for xlabels. In degrees; 0 is horizontal.
#' @param showallxlabels (optional) (Only for categorical graphs) Force all x labels to show? By default, this is false for numeric categorical and true for non-numeric categorical.
#'
#' @seealso \code{vignette("qplot-options", package = "arphit")} for a detailed description of
#' all the plotting options and how they affect the output.
#'
#' @examples
#' T <- 24
#' randomdata <- ts(data.frame(x1 = rnorm(T), x2 = rnorm(T), x3 = rnorm(T, sd = 10),
#'   x4 = rnorm(T, sd = 5)), start = c(2000,1), frequency = 4)
#' agg_qplot(randomdata, series = list("1" = c("x1", "x2"), "2" = c("x3", "x4")),
#'   bars = c("x3","x4"), layout = "2v", title = "A Title", subtitle = "A subtitle",
#'   footnotes = c("a","B"), sources = c("A Source", "Another source"), yunits = "index")
#'
#' @export
agg_qplot <- function(data, series = NULL, x = NULL, layout = "1", bars = NULL, filename = NULL, title = NULL, subtitle = NULL, paneltitles = NULL, panelsubtitles = NULL, footnotes = NULL, sources = NULL, yunits = NULL, col = NULL, pch = NULL, lty = NULL, lwd = NULL, barcol = NULL, xlim = NULL, ylim = NULL, legend = FALSE, legend.ncol = NA, portrait = FALSE, bar.stacked = TRUE, dropxlabel = FALSE, joined = TRUE, srt = 0, showallxlabels = NULL) {

  data <- conformdata(data, layout, series)
  x <- conformxvariable(x, data, layout)
  if (is.null(series)) {
    series <- list()
    for (p in names(data)) {
      series[[p]] <- colnames(data)
    }
  }

  agg_draw_internal(data = data,
                    series = series,
                    x = x,
                    layout = layout,
                    bars = bars,
                    filename = filename,
                    title = title,
                    subtitle = subtitle,
                    paneltitles = paneltitles,
                    panelsubtitles = panelsubtitles,
                    footnotes = footnotes,
                    sources = sources,
                    yunits = yunits,
                    col = col,
                    pch = pch,
                    lty = lty,
                    lwd = lwd,
                    barcol = barcol,
                    xlim = xlim,
                    ylim = ylim,
                    legend = legend,
                    legend.ncol = legend.ncol,
                    portrait = portrait,
                    bar.stacked = bar.stacked,
                    dropxlabel = dropxlabel,
                    joined = joined,
                    srt = srt,
                    showallxlabels = showallxlabels)
}

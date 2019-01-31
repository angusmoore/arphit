conformdata <- function(data, series, x) {
  if (is.acceptable.data(data)) {
    # Only a single data set, not a list of data is accepted for qplot
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
      data[["1"]] <- tmpdata[c(x, series)]
    } else {
      data[["1"]] <- tmpdata
    }

  } else {
    stop(paste0("Data is of unsupported type (you passed in ", class(data),")"))
  }
  return(data)
}

conformxvariable <- function(x, data) {
  if (!is.null(x)) {
    x <- list("1" = x)
  } else if (is.null(x)) {
    # Check for if we gave a time series, and need to make agg_time
    if (stats::is.ts(data) || zoo::is.zoo(data) || xts::is.xts(data)) {
      x <- list("1" = "agg_time")
    } else {
      stop("You did not specify an x variable and cannot guess it because your data is not a time series.`")
    }
  }
  return(x)
}

sanity_check_ylim <- function(ylim) {
  if (is.null(ylim$nsteps) || ylim$nsteps < 2) {
    stop("The y-limit you supplied has fewer than 2 points (or you forgot to supply nsteps).")
  }
  if (is.null(ylim$max)) {
    stop("You did not supply a max ylimit.")
  }
  if (is.null(ylim$min)) {
    stop("You did not supply a min ylimit.")
  }
}

handlebars <- function(data, bars) {
  if (is.logical(bars)) {
    # Everything is a bar!
    bars <- list()
    for (p in names(data)) {
      bars[[p]] <- colnames(data[[p]])
    }
    return(bars)
  }

  if (!is.list(bars)) {
    # Haven't supplied bars as a per panel thing. This is fine, but may be an issue
    oldbar <- bars
    bars <- list()
    for (p in names(data)) {
      bars[[p]] <- oldbar
    }
  }

  newbars <- list()
  for (p in names(data)) {
    newbars[[p]] <- c()
    for (s in colnames(data[[p]])) {
      if (s %in% bars[[p]]) {
        newbars[[p]] <- append(newbars[[p]], s)
      }
    }
  }
  return(newbars)
}


#' RBA-style graphs in R
#'
#' Quickly creates a (potentially multipanel) graph. Supports bar and line (and combinations of).
#'
#' @param data Object containing the series you want to plot. Can be a data.frame, tibble or ts. Can also be a list of the above, with separate for each panel.
#' @param series A vector of series names specifying which subset of series
#' you want to plot.
#' @param x The x variable for your plot. `ts`, `xts` and `zoo` data use the dates in the time series.
#' @param bars (optional) Vector of string names indicating which series should be bars, rather than lines. Alternatively, if you set `bars = TRUE` all series will plot as bars.
#' @param filename (optional) If specified, save image to filename instead of displaying in R. Supports pdf, emf and png extensions.
#' @param title (optional) A string indicating the title for the entire chart. Passing NULL (or omitting the argument) will suppress printing of title.
#' @param subtitle (optional) A string indicating the subtitle for the entire chart. Passing NULL (or omitting the argument) will suppress printing of subtitle.
#' @param footnotes (optional) A vector strings, corresponding to the footnotes, in order.
#' @param sources (optional) A vector of strings, one entry for each source.
#' @param yunits (optional) A list of string -> string pairs indicating the units to be used on each panel (/axes, see notes to series for explanation on how right-hand-side axes are treated). Alternatively, providing just a string will assign that to all panels. If not supplied, a per cent sign will be used.
#' @param col (optional) A list of string -> misc pairs. The keys should be series names, and the values colours for each series (any colour accepted by R is fine.) You need not supply colours for all series. Default colours will be assigned  (cycling through) to series without assigned colours. Alternatively, you can supply a single value to apply to all series.
#' @param pch (optional) A list of string -> int pairs. The keys should be series names, and the values markers (pch values) for each series. Defaults to none (NA). Can be supplied as list, or a single value (which will be applied to all series).
#' @param lty (optional) A list of string -> int pairs. The keys should be series names, and the values line types (lty values) for each series. Defaults to solid (1). Can be supplied as list, or a single value (which will be applied to all series).
#' @param lwd (optional) A list of string -> numeric pairs. The keys should be series names, and the values line width, relative to default, for each series. Defaults to 1. Can be supplied as list, or a single value (which will be applied to all series).
#' @param xlim (optional) c(numeric, numeric) Gives the x limits (in years) for the graph. Alternatively, you can supply a list to provide different x limits for each panel (not recommended). If unsupplied, a suitable default is chosen (recommended).
#' @param ylim (optional) A list of string -> list(min = numeric, max = numeric, nsteps int) pairs. Keys are panel names (e.g. "1", "2", etc). Values are the scale, provided as a list with the keys min, max and nsteps. If unsupplied, a suitable default is chosen (recommended, but will not work well for multipanels).
#' @param legend A logical indicating whether to add a legend to the graph (default FALSE).
#' @param legend.ncol How many columns do you want the legend to have (if NA, which is the default, arphit will guess for you).
#' @param bar.stacked (optional) Logical indicating whether the bar series should be stacked (TRUE, default) or side-by-side (FALSE).
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
agg_qplot <- function(data, series = NULL, x = NULL, bars = NULL, filename = NULL, title = NULL, subtitle = NULL, footnotes = c(), sources = c(), yunits = NULL, col = list(), pch = list(), lty = list(), lwd = list(), xlim = list(), ylim = list(), legend = FALSE, legend.ncol = NA, bar.stacked = TRUE) {

  x <- conformxvariable(x, data)
  data <- conformdata(data, series, x[["1"]])
  bars <- handlebars(data, bars)

  if (!x[["1"]] %in% names(data[["1"]])) {
    stop(paste0("The x variable you specified (", x[["1"]], ") is not in your data."))
  }

  agg_draw_internal(
    list(
      data = data,
      x = x,
      layout = "1",
      bars = bars,
      title = title,
      subtitle = subtitle,
      paneltitles = list(),
      panelsubtitles = list(),
      yaxislabels = list(),
      xaxislabels = list(),
      footnotes = footnotes,
      sources = sources,
      yunits = NULL,
      xunits = NULL,
      ylim = apply_ylim_to_panels(ylim),
      xlim = xlim,
      legend = legend,
      legend.ncol = legend.ncol,
      col = col,
      pch = pch,
      lty = lty,
      lwd = lwd,
      pointsize = list(),
      labels = list(),
      arrows = list(),
      lines = list(),
      bgshading = list(),
      shading = list(),
      portrait = FALSE,
      dropxlabel = FALSE,
      stacked = bar.stacked,
      srt = 0,
      showallxlabels = NULL,
      joined = TRUE,
      plotsize = LANDSCAPESIZE,
      enable_autolabeller = FALSE,
      log_scale = ""
    ),
    filename = filename)
}

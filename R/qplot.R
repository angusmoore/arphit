sanity_check_ylim <- function(ylim) {
  if (!is.list(ylim)) stop("ylim should be a list", call. = FALSE)
  if (is.null(ylim$nsteps) || ylim$nsteps < 2) {
    stop("The y-limit you supplied has fewer than 2 points (or you forgot to supply nsteps).", #nolint
         call. = FALSE)
  }
  if (is.null(ylim$max)) {
    stop("You did not supply a max ylimit.", call. = FALSE)
  }
  if (is.null(ylim$min)) {
    stop("You did not supply a min ylimit.", call. = FALSE)
  }
}

check_attribute_series_names <- function(attr, series_names) {
  if (any(!names(attr) %in% series_names)) {
    name <- names(attr)[!names(attr) %in% series_names]
    stop(paste0("You have tried to set attributes for ", name,
                " but it is not a series in your data."),
         call. = FALSE)
  }
}

qplot_get_attribute <- function(att, y) {
  if (!is.null(att)) {
    if (!is.list(att)) {
      return(att)
    } else {
      return(att[[y]])
    }
  } else {
    return(NULL)
  }
}

#' Quick plot - for quickly creates a single-panel graph. Supports bar and line
#' (and combinations of).
#'
#' @param data Object containing the series you want to plot. Can be a
#' `data.frame`, `tibble`, `zoo`, `xts` or `ts`.
#' @param series A vector of series names specifying which subset of series
#' you want to plot.
#' @param x The x variable for your plot. Not required for `ts`, `xts` and `zoo`
#' data because they use the dates in the time series.
#' @param bars (optional) Vector of string names indicating which series should
#' be bars, rather than lines. Alternatively, if you set `bars = TRUE` all
#' series will plot as bars.
#' @param filename (optional) If specified, save image to filename instead of
#' displaying in R. Supports svg, pdf, emf, emf+ and png extensions.
#' @param title (optional) A string indicating the title for the graph. Passing
#' `NULL` (or omitting the argument) will suppress printing of title.
#' @param subtitle (optional) A string indicating the subtitle for the graph.
#' Passing `NULL` (or omitting the argument) will suppress printing of subtitle.
#' @param footnotes (optional) A vector strings, corresponding to the footnotes,
#' in order.
#' @param sources (optional) A vector of strings, one entry for each source.
#' @param yunits (optional) A string indicating the units to be used. If not
#' supplied, a \% sign will be used.
#' @param col (optional) A list of string -> misc pairs. The keys should be
#' series names, and the values colours for each series (any colour accepted by
#' R is fine.) You need not supply colours for all series. Default colours will
#' be assigned  (cycling through) to series without assigned colours.
#' Alternatively, you can supply a single value to apply to all series.
#' @param pch (optional) Markers for your series. Passed as with `col`.
#' Defaults to none (NA).
#' @param lty (optional) Line types for each series.  Passed as with `col`.
#' Defaults to solid (1).
#' @param lwd (optional) Line width, relative to default, for each series.
#' Passed as with `col`.
#' @param xlim (optional) c(numeric, numeric) Gives the x limits (in years) for
#' the graph.
#' @param ylim (optional) A list(min = numeric, max = numeric, nsteps int).
#' If unsupplied, a suitable default is chosen.
#' @param legend A logical indicating whether to add a legend to the graph
#' (default FALSE).
#' @param legend_ncol (optional) How many columns do you want the legend to have
#' (if NA, which is the default, arphit will guess for you).
#' @param stacked (optional) Logical indicating whether the bar series
#' should be stacked (TRUE, default) or side-by-side (FALSE).
#' @param legend.ncol (DEPRECATED) Alias for legend_ncol
#' @param bar.stacked (DEPRECATED) Alias for stacked
#'
#' @seealso \code{vignette("qplot-options", package = "arphit")} for a detailed
#' description of all the plotting options and how they affect the output.
#'
#' @examples
#' T <- 24
#' randomdata <- ts(data.frame(x1 = rnorm(T),
#'                             x2 = rnorm(T),
#'                             x3 = rnorm(T, sd = 10),
#'                             x4 = rnorm(T, sd = 5)),
#'                  start = c(2000, 1),
#'                  frequency = 4)
#' agg_qplot(
#'   randomdata,
#'   title = "A Title",
#'   subtitle = "A subtitle",
#'   footnotes = c("a", "B"),
#'   sources = c("A Source", "Another source"),
#'   yunits = "index"
#' )
#'
#' @export
agg_qplot <- function(data,
                      series = NULL,
                      x = NULL,
                      bars = FALSE,
                      filename = NULL,
                      title = NULL,
                      subtitle = NULL,
                      footnotes = c(),
                      sources = c(),
                      yunits = NULL,
                      col = list(),
                      pch = list(),
                      lty = list(),
                      lwd = list(),
                      xlim = NULL,
                      ylim = NULL,
                      legend = FALSE,
                      legend_ncol = NA,
                      stacked = TRUE,
                      bar.stacked, #nolint
                      legend.ncol) { #nolint

  if (!missing(bar.stacked)) {
    warning("`bar.stacked` is deprecated; use `stacked` instead")
    stacked <- bar.stacked
  }

  if (!missing(legend.ncol)) {
    warning("`legend.ncol` is deprecated; use `legend_ncol` instead")
    legend_ncol <- legend.ncol
  }

  if (!is.acceptable.data(data)) {
    stop(paste0("Data is of unsupported type (you passed in ",
                class(data), ")"),
         call. = FALSE)
  }

  # Create the basics
  p <- arphitgg(data) + agg_title(title) + agg_subtitle(subtitle) +
    agg_footnote(footnotes) + agg_source(sources) +
    agg_units(yunits)

  if (!is.null(ylim)) {
    sanity_check_ylim(ylim)
    p <- p + agg_ylim(ylim$min, ylim$max, ylim$nsteps)
  }

  if (!is.null(xlim)) {
    p <- p + agg_xlim(xlim[1], xlim[2])
  }

  if (legend) {
    p <- p + agg_legend(ncol = legend_ncol)
  }

  # Now add each series as a layer
  if (is.null(series)) {
    series <- colnames(data)
    if (!is.null(x)) {
      series <- series[series != x]
    }
  }

  check_attribute_series_names(col, series)
  check_attribute_series_names(pch, series)
  check_attribute_series_names(lty, series)
  check_attribute_series_names(lwd, series)

  for (y in series) {
    y_sym <- rlang::sym(y)
    if (!is.null(x)) {
      x_sym <- rlang::sym(x)
      aes <- list(type = "aes",
                  x = rlang::enquo(x_sym),
                  y = rlang::enquo(y_sym),
                  order = rlang::enquo(x_sym))
    } else {
      aes <- list(type = "aes", y = rlang::enquo(y_sym))
    }

    if ((is.logical(bars) && bars) || (y %in% bars)) {
      p <- p + agg_col(aes = aes, colour = qplot_get_attribute(col, y))
    } else {
      p <- p + agg_line(aes = aes,
                        colour = qplot_get_attribute(col, y),
                        pch = qplot_get_attribute(pch, y),
                        lty = qplot_get_attribute(pch, y),
                        lwd = qplot_get_attribute(lwd, y))
    }
  }

  agg_draw(p, filename = filename)
}

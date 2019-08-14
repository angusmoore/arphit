#' Draw a defined graph
#'
#' @param gg An arphitgg built graph.
#' @param filename (optional) If specified, save image to filename instead of
#' displaying in R. Supports png, pdf, emf, emf+, svg and xlsx extensions.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a
#' detailed description of all the plotting options
#'
#' @export
agg_draw <- function(gg, filename = NULL) {
  # Here we call the arphit drawing function
  gg$data[["parent"]] <- NULL
  if (!is.null(finddevice(filename)) && finddevice(filename) == "xlsx") {
    write_to_excel(gg, filename)
  } else {
    agg_draw_internal(gg, filename)
  }
}

#' Draw a defined graph
#'
#' @param x An arphitgg built graph.
#' @param ... Further arguments passed to or from other methods.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a
#' detailed description of all the plotting options
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
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a
#' detailed description of all the plotting options
#'
#' @export
"+.arphit.gg" <- function(gg, element) {
  switch(
    element$type,
    "line" = addseries(gg, element, "line"),
    "col" = addseries(gg, element, "bar"),
    "step" = addseries(gg, element, "step"),
    "waterfall" = addseries(gg, element, "waterfall"),
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
    "legend" = addlegend(gg, element),
    "autolabel" = enableautolabel(gg,
                                  element$quiet,
                                  element$arrow_lines,
                                  element$arrow_bars,
                                  element$ignore_existing_labels),
    "xfreq" = addxfreq(gg, element$freq, element$panel),
    "rename_series" = renameseries(gg, element$mapping, element$panel),
    stop("Unknown element type for arphitgg", call. = FALSE)
  )
}

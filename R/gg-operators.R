#' Draw a defined graph
#'
#' @param gg An arphitgg built graph.
#' @param filename (optional) If specified, save image to filename instead of displaying in R. Supports pdf, emf and png extensions.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @export
agg_draw <- function(gg, filename = NULL) {
  # Here we call the arphit drawing function
  gg$data[["parent"]] <- NULL
  agg_draw_internal(gg, filename)
}

#' Draw a defined graph
#'
#' @param x An arphitgg built graph.
#' @param ... Further arguments passed to or from other methods.
#'
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
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
#' @seealso \code{vignette("plotting-options", package = "arphit")} for a detailed description of
#' all the plotting options
#'
#' @export
"+.arphit.gg" <- function(gg, element) {
  gg = switch(element$type,
         "line" = addlineseries(gg, element),
         "col" = addcolseries(gg, element),
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
         "autolabel" = enableautolabel(gg, element$quiet),
         stop("Unknown element type for arphit.gg"))
  return(gg)
}

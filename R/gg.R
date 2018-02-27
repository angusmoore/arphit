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
#' data  <- data.frame(unemployment = rnorm(20), state = c(rep("A", 10), rep("B", 10)), date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))
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
#' data  <- data.frame(unemployment = rnorm(20), state = c(rep("A", 10), rep("B", 10)), date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))
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

#' Add units
#'
#' @param units A string specifying the units.
#' @param panel (optional) Specify a panel identifier to add to a specific panel. If blank, units will be applied to all panels.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' data  <- data.frame(unemployment = rnorm(20), state = c(rep("A", 10), rep("B", 10)), date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))
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

#' Add a source (or many sources)
#'
#' @param source A string, or vector of strings, to be added as sources
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' data  <- data.frame(unemployment = rnorm(20), state = c(rep("A", 10), rep("B", 10)), date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))
#' arphitgg(data) + agg_footnote("Source 1")
#' arphitgg(data) + agg_footnote(c("Source 1", "Source 2"))
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
#' data  <- data.frame(unemployment = rnorm(20), state = c(rep("A", 10), rep("B", 10)), date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))
#' arphitgg(data) + agg_footnote("Here is a footnote")
#' arphitgg(data) + agg_footnote(c("Here is a footnote", "And a second one"))
#'
#' @export
agg_footnote <- function(footnote) {
  return(list(type = "footnote", footnote = footnote))
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
#' data  <- data.frame(unemployment = rnorm(20), state = c(rep("A", 10), rep("B", 10)), date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))
#' arphitgg(data) + agg_line(aes = agg_aes(x = date, y = unemployment, group = state), panel = "1")
#'
#' @export
agg_line <- function(data = NULL, aes = NULL, color = NULL, pch = NULL, lty = NULL, lwd = NULL, panel = "1") {
  if (is.list(data) && !is.null(data$type) && data$type == "aes") {
    stop("You passed aes as the first argument to agg_line rather than data. Did you forget to name the aes argument? (aes = agg_aes(...))")
  }
  return(list(type = "line", data = data, aes = aes, color = color, pch = pch, lty = lty, lwd = lwd, panel = as.character(panel)))
}

#' Add a col layer to an arphit plot.
#'
#' @param data The data to be used. Will inherit from parent if missing.
#' @param aes The aesthetic that defines the layer. Will inherit (or parts thereof) if omitted.
#' @param color A colour to be applied to all of the series, or (if your aesthetic has a group), a vector of colours that will be cycled through to consecutive group elements.
#' @param panel (default = "1") Which panel of the graph to place this layer on.
#' @param stacked (default = TRUE) Stack the bars, or group them?
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @examples
#' data  <- data.frame(unemployment = rnorm(20), state = c(rep("A", 10), rep("B", 10)), date = seq.Date(from = as.Date("2017-01-10"), length.out = 10, by = "quarter"))
#' arphitgg(data) + agg_col(aes = agg_aes(x = date, y = unemployment, group = state), panel = "1")
#'
#' @export
agg_col <- function(data = NULL, aes = NULL, color = NULL, panel = "1", stacked = TRUE) {
  if (is.list(data) && !is.null(data$type) && data$type == "aes") {
    stop("You passed aes as the first argument to agg_col rather than data. Did you forget to name the aes argument? (aes = agg_aes(...))")
  }
  return(list(type = "col", data = data, aes = aes, color = color, panel = as.character(panel), stacked = stacked))
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
#' @param dropxlabel (optional) Logical indicating whether the first xlabel of right hand panels in 2v and 2b2 should be ignored (prevents overlapping of last xlabel on left panel with first on right). TRUE by default.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @export
arphitgg <- function(data = NULL, aes = NULL, layout = "1", portrait = FALSE, dropxlabel = TRUE) {
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
             footnotes = c(),
             sources = c(),
             scaleunits = NULL,
             col = list(),
             pch = list(),
             lty = list(),
             lwd = list(),
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
    gg$scaleunits <- units$units
  } else {
    if (is.null(gg$scaleunits)) {
      gg$scaleunits <- list()
    }
    gg$scaleunits[[units$panel]] <- units$units
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
         footnotes = gg$footnotes,
         sources = gg$sources,
         scaleunits = gg$scaleunits,
         col = gg$col,
         pch = gg$pch,
         lty = gg$lty,
         lwd = gg$lwd,
         portrait = gg$portrait,
         dropxlabel = gg$dropxlabel,
         bar.stacked = gg$stacked,
         filename = filename)
}

#' Draw a defined graph
#'
#' @param gg An arphitgg built graph.
#'
#' @seealso \code{vignette("gg-interface", package = "arphit")} for a detailed description of
#' how to use the ggplot-like interface.
#'
#' @export
print.arphit.gg <- function(gg, ...) {
  agg_draw(gg)
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
         "title" = addtitle(gg, element),
         "subtitle" = addsubtitle(gg, element),
         "units" = addunits(gg, element),
         "source" = addsource(gg, element),
         "footnote" = addfootnote(gg, element),
         stop("Unknown element type for arphit.gg"))
  return(gg)
}

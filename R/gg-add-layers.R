convert2wide <- function(data, aes) {
  if (!is.null(aes$group)) {
    groups <- unique(data[[aes$group]])
    data <- tidyr::spread_(data, key = aes$group, value = aes$y)
  }
  if (!is.null(aes$order)) {
    data <- dplyr::arrange_(data, aes$order)
    # now check whether the order variable is one we want to plot (in wihch case, keep it, or a variable included purely to order the data)
    if (aes$order != aes$x && aes$order != aes$y && (is.null(aes$group) || !(aes$order %in% groups))) {
      data <- dplyr::select_(data, paste0("-",aes$order))
    }
  }
  return(data)
}

addbackticks <- function(x) {
  paste0("`", x, "`")
}

subsetdata <- function(data, x, y, group, order) {
  x <- addbackticks(x)
  y <- addbackticks(y)
  if (!is.null(group)) {
    group <- addbackticks(group)
    if (!is.null(order) && order %in% colnames(data)) {
      order <- addbackticks(order)
      return(dplyr::select_(data, x, y, group, order))
    } else {
      return(dplyr::select_(data, x, y, group))
    }
  } else {
    if (!is.null(order)) {
      order <- addbackticks(order)
      return(dplyr::select_(data, x, y, order))
    } else {
      return(dplyr::select_(data, x, y))
    }
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

autolayout <- function(n) {
  layout <- switch(as.character(n),
                   "1" = "1",
                   "2" = "2h",
                   "3" = "3h",
                   "4" = "2b2",
                   "5" = "3b2",
                   "6" = "3b2",
                   "7" = "4b2",
                   "8" = "4b2",
                   stop(paste0("Cannot layout ", n, " facets.")))

  panels <- switch(as.character(n),
                   "1" = c("1"),
                   "2" = c("1","3"),
                   "3" = c("1","3","5"),
                   "4" = c("1","2","3","4"),
                   "5" = c("1","2","3","4","5"),
                   "6" = c("1","2","3","4","5","6"),
                   "7" = c("1","2","3","4","5","6","7"),
                   "8" = c("1","2","3","4","5","6","7","8"))
  return(list(layout=layout,panels=panels))
}

facetlayout <- function(data, facet, layout) {
  n <- length(unique(data[[facet]]))
  maxnp <- maxpanels(layout)
  if (layout != "1" && maxnp >= n) {
    # have specified a layout and it is fine (assume that 1 is just the default and should be ignored)
    if (layout == "2v" || layout == "3v" || layout == "2b2" || layout == "3b2" || layout == "4b2") {
      # No left and right axes to worry about
      return(list(layout=layout,panels=as.character(1:n)))
    } else {
      # Try to keep to odd, if that doesn't work, use all
      if (max(seq(1, length.out = n, by = 2)) <= maxnp) {
        return(list(layout=layout,panels=as.character(seq(1, length.out = n, by = 2))))
      } else {
        # nope, doesn't fit on left axes
        return(list(layout=layout,panels=as.character(1:n)))
      }
    }
  } else {
    return(autolayout(n))
  }
}

addlayertopanel <- function(gg, new, panel) {
  if (tibble::is_tibble(new$data) || is.data.frame(new$data)) {
    # Make sure data isn't grouped (causes errors sometimes if it is)
    new$data <- dplyr::ungroup(new$data)
  }
  # Special case ts data
  if (is.null(new$aes$x) && (stats::is.ts(new$data))) {
    agg_time <- as.Date(lubridate::date_decimal(as.numeric(stats::time(new$data))))
    new$aes$x <- "agg_time"
    new$data <- tibble::as_tibble(new$data)
    new$data$agg_time <- agg_time
  } else if (is.null(new$aes$x) && (zoo::is.zoo(new$data) || xts::is.xts(new$data))) {
    agg_time <- stats::time(new$data)
    new$aes$x <- "agg_time"
    new$data <- tibble::as_tibble(new$data)
    new$data$agg_time <- agg_time
  }

  # Check for bare minimum aes
  if (is.null(new$aes$x)) {
    stop("Cannot add layer. You have not specified an x aesthetic (and there was not one to inherit).")
  }
  if (is.null(new$aes$y)) {
    stop("Cannot add layer. You have not specified a y aesthetic for at least one of your layers (and there was not one to inherit).")
  }

  # Assign the x variable
  if (!is.null(gg$x[[panel]])) {
    # Have previously set an x variable. Check is the same.
    if (gg$x[[panel]] != new$aes$x) {
      stop(paste("You cannot add a series to panel ", panel, " with x variable ", new$aes$x, " because you have already added a series to that panel with x variable ", gg$x[[panel]], sep = ""))
    }
  } else {
    gg$x[[panel]] <- new$aes$x
  }

  # handle data
  reorder <- !is.unsorted(new$data[[new$aes$x]]) && !is.unsorted(gg$data[[panel]][[new$aes$x]])
  newdata <- subsetdata(new$data, new$aes$x, new$aes$y, new$aes$group, new$aes$order)
  newdata <- convert2wide(newdata, new$aes)
  if (!is.null(gg$data[[panel]])) {
    # We have already added a series for this panel, so we need to merge the new data on to the old
    mergeddata <- dplyr::full_join(gg$data[[panel]], newdata, by = gg$x[[panel]])
    mergeddata <- unrename(mergeddata)
    if (reorder) {
      mergeddata <- dplyr::arrange_(mergeddata, addbackticks(gg$x[[panel]]))
    }
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

addlayer <- function(gg, new, panel) {
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
  if (is.null(new$aes$facet) && !is.null(gg$aes$facet)) {
    new$aes$facet <- gg$aes$facet
  }

  # if data is null, inherit from parent
  if (is.null(new$data)) {
    new$data <- gg$data[["parent"]]
    if (is.null(new$data)) {
      stop("You have not supplied data for series")
    }
  }

  if (is.null(new$aes$facet)) {
    out <- addlayertopanel(gg, new, panel)
    gg <- out$gg
    newseriesnames <- out$newseriesnames
    returnpanels <- panel
  } else {
    layoutoverride <- facetlayout(new$data, new$aes$facet, gg$layout)
    gg$layout <- layoutoverride$layout
    facets <- sort(unique(new$data[[new$aes$facet]]))
    newseriesnames <- list()
    for (i in 1:length(facets)) {
      panel <- layoutoverride$panels[[i]]
      subset_data <- new
      subset_data$data <- subset_data$data[new$data[[new$aes$facet]] == facets[i],]
      out <- addlayertopanel(gg, subset_data, panel)
      gg <- out$gg
      gg$paneltitles[[panel]] <- as.character(facets[i])
      newseriesnames[[panel]] <- out$newseriesnames
    }
  }
  return(list(gg = gg, newseriesnames = newseriesnames))
}

applylineattributes <- function(gg, newline, panel, newseriesnames) {
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

addlineseries <- function(gg, newline) {
  panel <- newline$panel
  out <- addlayer(gg, newline, panel)
  gg <- out$gg
  newseriesnames <- out$newseriesnames
  if (!is.list(newseriesnames)) {
    gg <- applylineattributes(gg, newline, panel, newseriesnames)
  } else {
    for (panel in names(newseriesnames)) {
      gg <- applylineattributes(gg, newline, panel, newseriesnames[[panel]])
    }
  }
  return(gg)
}

addpointseries <- function(gg, newpoint) {
  # Just create a line series with PCH and LTY set
  newpoint$pch <- 16
  newpoint$lty <- 0
  return(addlineseries(gg, newpoint))
}

applycolattributes <- function(gg, panel, newcol, newcolnames) {
  if (!is.null(gg$bars[[panel]])) {
    gg$bars[[panel]] <- append(gg$bars, newcolnames)
  } else {
    gg$bars[[panel]] <- newcolnames
  }
  if (!is.null(newcol$barcol)) {
    gg <- applyattribute(gg, "barcol", panel, newcolnames, newcol$barcol)
  }
  return(gg)
}

addcolseries <- function(gg, newcol) {
  panel <- newcol$panel
  out <- addlayer(gg, newcol, panel)
  gg <- out$gg
  newcolnames <- out$newseriesnames

  if (!is.list(newcolnames)) {
    gg <- applycolattributes(gg, panel, newcol, newcolnames)
  } else {
    for (panel in names(newcolnames)) {
      gg <- applycolattributes(gg, panel, newcol, newcolnames[[panel]])
    }
  }

  if (!is.null(newcol$stacked)) {
    gg$stacked <- newcol$stacked
  }

  return(gg)
}

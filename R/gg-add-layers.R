
applyattribute <- function(gg, attributename, panel, newseries, attributevalues) {
  i <- 1
  for (s in newseries) {
    gg$data[[panel]]$series[[s]]$attributes[[attributename]] <- attributevalues[i]
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
  n <- length(unique(rlang::eval_tidy(facet, data)))
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

sanity_check_aesthetic <- function(aes) {
  if (is_null_quo(aes$x)) {
    stop("Cannot add layer. You have not specified an x aesthetic (and there was not one to inherit).")
  }
  if (is_null_quo(aes$y)) {
    stop("Cannot add layer. You have not specified a y aesthetic for at least one of your layers (and there was not one to inherit).")
  }
}

try_tidy <- function(eval, data) {
  tryCatch({
    ignore <- rlang::eval_tidy(eval, data)
    return(TRUE)
  },
  error = function(e)
    return(FALSE))
}

is_null_quo <- function(x) {
  is.null(x) || rlang::quo_is_null(x) || rlang::quo_is_missing(x)
}

check_aes_in_data <- function(data, aes, panel) {
  if (!try_tidy(aes$x, data)) stop(paste0(rlang::quo_name(aes$x)," is not in your data for panel ", panel))
  if (!try_tidy(aes$y, data)) stop(paste0(rlang::quo_name(aes$y)," is not in your data for panel ", panel))
  if (!is_null_quo(aes$group) && !try_tidy(aes$group, data)) stop(paste0(rlang::quo_name(aes$group)," is not in your data for panel ", panel))
}

convert_data <- function(data, aes) {
  # Special case ts data
  if (is_null_quo(aes$x) && (stats::is.ts(data) || zoo::is.zoo(data) || xts::is.xts(data))) {
    if (stats::is.ts(data)) {
      agg_time <- as.Date(lubridate::date_decimal(as.numeric(stats::time(data))))
    } else {
      agg_time <- stats::time(data)
    }
    x_sym <- rlang::sym("agg_time")
    aes$x <- rlang::enquo(x_sym)
    aes$order <- rlang::enquo(x_sym)
    data <- as.data.frame(data)
    row.names(data) <- c()
    data$agg_time <- agg_time
  }
  return(list(data = data, aes = aes))
}

widen_x <- function(gg, data, aes, panel) {
  # check compatible
  new_x <- rlang::eval_tidy(aes$x, data)
  old_x <- gg$data[[panel]]$x
  if (is.factor(new_x)) {
    new_x <- as.character(new_x)
  }
  if (!is.null(old_x) && class(new_x) != class(old_x) &&
      !((class(new_x)=="integer" && class(old_x)=="numeric") ||
        (class(old_x)=="integer" && class(new_x)=="numeric"))) {
    stop(paste0("Do not know how to join together x values ", class(new_x), " and ", class(old_x), " (panel ", panel, ")"))
  }
  gg$data[[panel]]$x <- unique(c(new_x,old_x))
  return(gg)
}

assign_series <- function(gg, data, aes, panel, bar) {
  if (is_null_quo(aes$group)) {
    new_series <- create_series(rlang::quo_name(aes$y), rlang::eval_tidy(aes$x, data), rlang::eval_tidy(aes$y, data), bar)
    gg$data[[panel]]$series <- append(gg$data[[panel]]$series, list(new_series))
  } else {
    # Special case NAs in the data
    groups <- factor(rlang::eval_tidy(aes$group, data), ordered = FALSE, exclude = NULL)
    data <- split(data, groups)
    names(data)[is.na(names(data))] <- "<NA>"

    for (name in names(data)) {
      new_series <- create_series(name, rlang::eval_tidy(aes$x, data[[name]]), rlang::eval_tidy(aes$y, data[[name]]), bar)
      gg$data[[panel]]$series <- append(gg$data[[panel]]$series, list(new_series))
    }
  }
  return(gg)
}

reorder_series <- function(gg, data, aes, panel) {

  if (!is_null_quo(aes$group) && rlang::quo_name(aes$order) %in% series_names(gg$data[[panel]])) {
    # ordering by the value of one of the series (the group subset of aes$y)
    order_index <- which(rlang::quo_name(aes$order) == series_names(gg$data[[panel]]))
    new_order_mapping <- data.frame(x = gg$data[[panel]]$series[[order_index]]$x,
                                    order = series_values(gg$data[[panel]], order_index))
  } else {
    x <- rlang::eval_tidy(aes$x, data)
    order <- rlang::eval_tidy(aes$order, data)
    if (is.factor(x)) x <- as.character(x)
    if (is.factor(order)) order <- as.character(order)

    new_order_mapping <- unique(data.frame(x = x, order = order, stringsAsFactors = FALSE))
  }

  # combine the existing order mapping with the new one and get unique entries
  if (!is.null(gg$data[[panel]]$order_mapping$order) &&
      class(new_order_mapping$order) != class(gg$data[[panel]]$order_mapping$order) &&
      !((class(new_order_mapping$order)=="integer" && class(gg$data[[panel]]$order_mapping$order)=="numeric") ||
        (class(gg$data[[panel]]$order_mapping$order)=="integer" && class(new_order_mapping$order)=="numeric"))) {
    stop(paste0("Do not know how to join together ordering variables with classes ", class(new_order_mapping$order), " and ", class(gg$data[[panel]]$order_mapping$order), " (panel ", panel, "). Perhaps you added layers to the same panel with different ordering variables (or didn't specify an ordering variable for one of the layers)?"))
  }
  gg$data[[panel]]$order_mapping <- unique(rbind(new_order_mapping, gg$data[[panel]]$order_mapping))

  # Check for ambiguity
  if (anyDuplicated(gg$data[[panel]]$order_mapping$x) ||
      nrow(gg$data[[panel]]$order_mapping) != length(gg$data[[panel]]$x)) {
    stop("Ordering is ambiguous - some x values associate with multiple values of the ordering variable, or there are no observations of the ordering variable for some x values.")
  }

  # sort the order mapping
  reorder <- order(gg$data[[panel]]$order_mapping$order)
  gg$data[[panel]]$order_mapping <- gg$data[[panel]]$order_mapping[reorder, ]

  # get how the order mapping should remap x values
  reorder <- match(gg$data[[panel]]$order_mapping$x, gg$data[[panel]]$x)
  gg$data[[panel]]$x <- gg$data[[panel]]$x[reorder]

  # for each series join on the order mapping and then order accordingly
  for (i in 1:length(gg$data[[panel]]$series)) {
    tmp <- data.frame(x=series_x_values(gg$data[[panel]], i), stringsAsFactors = FALSE)
    tmp <- dplyr::left_join(tmp, gg$data[[panel]]$order_mapping, by = "x")
    reorder <- order(tmp$order)

    gg$data[[panel]]$series[[i]]$x <- series_x_values(gg$data[[panel]], i)[reorder]
    gg$data[[panel]]$series[[i]]$y <- series_values(gg$data[[panel]], i)[reorder]
  }

  return(gg)
}

addlayertopanel <- function(gg, data, aes, panel, bar) {
  existing_series <- length(gg$data[[panel]]$series)
  out <- convert_data(data, aes) # convert TS/ZOO/XTS to dataframe
  data <- out$data
  aes <- out$aes
  sanity_check_aesthetic(aes) # Check for bare minimum aes
  check_aes_in_data(data, aes, panel)

  ## initialise the panel if it doesn't exist
  if (is.null(gg$data[[panel]])) {
    gg$data[[panel]] <- new_panel_data()
  }

  ## assign the x variable
  gg <- widen_x(gg, data, aes, panel)

  ## assign series
  gg <- assign_series(gg, data, aes, panel, bar)

  ## now reorder
  gg <- reorder_series(gg, data, aes, panel)

  return(list(gg = gg, new_series_indices = (existing_series+1):length(gg$data[[panel]]$series)))
}

inherit_aes <- function(gg, aes) {
  # if aes is null, inherit from parent
  if (is.null(aes)) {
    aes <- gg$aes
  }
  # Check all the parts
  if (is_null_quo(aes$x) && !is_null_quo(gg$aes$x)) {
    aes$x <- gg$aes$x
  }
  if (is_null_quo(aes$y) && !is_null_quo(gg$aes$y)) {
    aes$y <- gg$aes$y
  }
  if (is_null_quo(aes$group) && !is_null_quo(gg$aes$group)) {
    aes$group <- gg$aes$group
  }
  if (is_null_quo(aes$facet) && !is_null_quo(gg$aes$facet)) {
    aes$facet <- gg$aes$facet
  }
  if (is_null_quo(aes$order) && !is_null_quo(gg$aes$order)) {
    aes$order <- gg$aes$order
  }

  return(aes)
}

inherit_data <- function(gg, data) {
  # if data is null, inherit from parent
  if (is.null(data)) {
    data <- gg$data[["parent"]]
    if (is.null(data)) {
      stop("You have not supplied data for series")
    }
  }
  return(data)
}

addlayer <- function(gg, new, panel, bar) {
  aes <- inherit_aes(gg, new$aes)
  data <- inherit_data(gg, new$data)
  # Error if data is weird
  if (!is.acceptable.data(data)) {
    stop(paste0("Data is of unsupported type (you passed in ", class(data),")"))
  }

  if (is_null_quo(aes$facet)) {
    out <- addlayertopanel(gg, data, aes, panel, bar)
    gg <- out$gg
    newseries <- out$new_series_indices
  } else {
    layoutoverride <- facetlayout(data, aes$facet, gg$layout)
    gg$layout <- layoutoverride$layout
    facets <- sort(unique(rlang::eval_tidy(aes$facet, data)), na.last = TRUE)
    newseries <- list()
    for (i in 1:length(facets)) {
      panel <- layoutoverride$panels[[i]]
      if (!is.na(facets[i])) {
        keep_rows <- !is.na(rlang::eval_tidy(aes$facet, data)) & rlang::eval_tidy(aes$facet, data) == facets[i]
      } else {
        subset_data <- is.na(rlang::eval_tidy(aes$facet, data))
      }
      subset_data <- data[keep_rows,]
      out <- addlayertopanel(gg, subset_data, aes, panel, bar)
      gg <- out$gg
      gg$paneltitles[[panel]] <- ifelse(is.na(facets[i]), "", as.character(facets[i]))
      newseries[[panel]] <- out$new_series_indices
    }
  }
  return(list(gg = gg, newseries = newseries))
}

applylineattributes <- function(gg, newline, panel, newseries) {
  gg <- applyattribute(gg, "col", panel, newseries, newline$colour)
  gg <- applyattribute(gg, "pch", panel, newseries, newline$pch)
  gg <- applyattribute(gg, "lty", panel, newseries, newline$lty)
  gg <- applyattribute(gg, "lwd", panel, newseries, newline$lwd)
  gg <- applyattribute(gg, "pointsize", panel, newseries, newline$pointsize)
  return(gg)
}

addlineseries_ <- function(gg, newline) {
  panel <- newline$panel
  out <- addlayer(gg, newline, panel, FALSE)
  gg <- out$gg
  newseries <- out$newseries
  if (!is.list(newseries)) {
    gg <- applylineattributes(gg, newline, panel, newseries)
  } else {
    for (panel in names(newseries)) {
      gg <- applylineattributes(gg, newline, panel, newseries[[panel]])
    }
  }
  return(gg)
}

addlineseries <- function(gg, newline)  {
  for (p in newline$panel) {
    tmp <- newline
    tmp$panel <- p
    gg <- addlineseries_(gg, tmp)
  }
  return(gg)
}

applycolattributes <- function(gg, panel, newcol, newcols) {
  if (!is.null(newcol$barcol)) {
    gg <- applyattribute(gg, "barcol", panel, newcols, newcol$barcol)
  }
  if (!is.null(newcol$col)) {
    gg <- applyattribute(gg, "col", panel, newcols, newcol$col)
  }
  return(gg)
}

addcolseries_ <- function(gg, newcol) {
  panel <- newcol$panel
  out <- addlayer(gg, newcol, panel, TRUE)
  gg <- out$gg
  newcols <- out$newseries

  if (!is.list(newcols)) {
    gg <- applycolattributes(gg, panel, newcol, newcols)
  } else {
    for (panel in names(newcols)) {
      gg <- applycolattributes(gg, panel, newcol, newcols[[panel]])
    }
  }

  if (!is.null(newcol$stacked)) {
    gg$stacked <- newcol$stacked
  }

  return(gg)
}


addcolseries <- function(gg, newcol) {
  for (p in newcol$panel) {
    tmp <- newcol
    tmp$panel <- p
    gg <- addcolseries_(gg, tmp)
  }
  return(gg)
}

applyattribute <- function(gg,
                           attributename,
                           panel,
                           newseries,
                           attributevalues) {
  i <- 1
  for (s in newseries) {
    gg$data[[panel]]$series[[s]]$attributes[[attributename]] <-
      attributevalues[i]
    i <- i %% length(attributevalues) + 1
  }

  gg
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
                   stop(paste0("Cannot layout ", n, " facets.")),
                   call. = FALSE)

  panels <- switch(as.character(n),
                   "1" = c("1"),
                   "2" = c("1", "3"),
                   "3" = c("1", "3", "5"),
                   "4" = c("1", "2", "3", "4"),
                   "5" = c("1", "2", "3", "4", "5"),
                   "6" = c("1", "2", "3", "4", "5", "6"),
                   "7" = c("1", "2", "3", "4", "5", "6", "7"),
                   "8" = c("1", "2", "3", "4", "5", "6", "7", "8"))

  list(layout = layout, panels = panels)
}

facetlayout <- function(data, facet, layout) {
  n <- length(unique(rlang::eval_tidy(facet, data)))
  maxnp <- maxpanels(layout)
  if (layout != "1" && maxnp >= n) {
    # have specified a layout and it is fine (assume that 1 is just the default
    # and should be ignored)
    if (layout == "2v" || layout == "3v" || layout == "2b2" ||
        layout == "3b2" || layout == "4b2") {
      # No left and right axes to worry about
      list(layout = layout, panels = as.character(1:n))
    } else {
      # Try to keep to odd, if that doesn't work, use all
      if (max(seq(1, length.out = n, by = 2)) <= maxnp) {
        list(layout = layout,
             panels = as.character(seq(1, length.out = n, by = 2)))
      } else {
        # nope, doesn't fit on left axes
        list(layout = layout, panels = as.character(1:n))
      }
    }
  } else {
    autolayout(n)
  }
}

sanity_check_aesthetic <- function(aes) {
  if (is_null_quo(aes$x)) {
    stop("Cannot add layer. You have not specified an x aesthetic (and there was not one to inherit).",#nolint
         call. = FALSE)
  }
  if (is_null_quo(aes$y)) {
    stop("Cannot add layer. You have not specified a y aesthetic for at least one of your layers (and there was not one to inherit).",#nolint
         call. = FALSE)
  }
}

try_tidy <- function(eval, data) {
  tryCatch({
    rlang::eval_tidy(eval, data)
    TRUE
  },
  error = function(e)
    FALSE)
}

is_null_quo <- function(x) {
  is.null(x) || rlang::quo_is_null(x) || rlang::quo_is_missing(x)
}

check_aes_in_data <- function(data, aes, panel) {
  if (!try_tidy(aes$x, data)) {
    stop(paste0(rlang::quo_name(aes$x),
                " is not in your data for panel ", panel),
         call. = FALSE)
  }
  if (!try_tidy(aes$y, data)) {
    stop(paste0(rlang::quo_name(aes$y),
                " is not in your data for panel ", panel),
         call. = FALSE)
  }
  if (!is_null_quo(aes$group) && !try_tidy(aes$group, data)) {
    stop(paste0(rlang::quo_name(aes$group),
                " is not in your data for panel ", panel),
         call. = FALSE)
  }
}

convert_data <- function(data, aes) {
  # Special case ts data
  if (is_null_quo(aes$x) &&
      (stats::is.ts(data) || zoo::is.zoo(data) || xts::is.xts(data))) {
    if (stats::is.ts(data)) {
      agg_time <- as.Date(
        lubridate::date_decimal(as.numeric(stats::time(data)))
      )
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

  list(data = data, aes = aes)
}

widen_x <- function(gg, data, aes, panel) {
  # check compatible
  new_x <- rlang::eval_tidy(aes$x, data)
  old_x <- gg$data[[panel]]$x
  if (is.factor(new_x)) {
    new_x <- as.character(new_x)
  }
  if (!is.null(old_x) && class(new_x) != class(old_x) &&
      !((class(new_x) == "integer" && class(old_x) == "numeric") ||
        (class(old_x) == "integer" && class(new_x) == "numeric"))) {
    stop(paste0("Do not know how to join together x values ",
                class(new_x), " and ", class(old_x), " (panel ", panel, ")"),
         call. = FALSE)
  }
  gg$data[[panel]]$x <- unique(c(new_x, old_x))

  gg
}

assign_series <- function(gg, data, aes, panel, geomtype) {
  if (is_null_quo(aes$group)) {
    new_series <- create_series(rlang::quo_name(aes$y),
                                rlang::eval_tidy(aes$x, data),
                                rlang::eval_tidy(aes$y, data), geomtype)
    gg$data[[panel]]$series <- append(gg$data[[panel]]$series, list(new_series))
  } else {
    # Special case NAs in the data
    groups <- factor(rlang::eval_tidy(aes$group, data),
                     ordered = FALSE,
                     exclude = NULL)
    data <- split(data, groups)
    names(data)[is.na(names(data))] <- "<NA>"

    for (name in names(data)) {
      new_series <- create_series(name,
                                  rlang::eval_tidy(aes$x, data[[name]]),
                                  rlang::eval_tidy(aes$y, data[[name]]),
                                  geomtype)
      gg$data[[panel]]$series <- append(gg$data[[panel]]$series,
                                        list(new_series))
    }
  }

  gg
}

class_differs <- function(order_mapping, new_order_mapping) {
  if (!is.null(order_mapping$order)) {
    if (class(new_order_mapping$order) == class(order_mapping$order)) {
      FALSE
    } else {
      !((class(new_order_mapping$order) == "integer" &&
          class(order_mapping$order) == "numeric") ||
        (class(order_mapping$order) == "integer" &&
            class(new_order_mapping$order) == "numeric"))
    }
  } else {
    FALSE
  }
}

reorder_series <- function(gg, data, aes, panel) {
  if (!is_null_quo(aes$group) &&
      rlang::quo_name(aes$order) %in% series_names(gg$data[[panel]])) {
    # ordering by the value of one of the series (the group subset of aes$y)
    order_index <- which(rlang::quo_name(aes$order) ==
                           series_names(gg$data[[panel]]))
    new_order_mapping <- data.frame(
      x = gg$data[[panel]]$series[[order_index]]$x,
      order = series_values(gg$data[[panel]], order_index)
    )
  } else {
    x <- rlang::eval_tidy(aes$x, data)
    order <- rlang::eval_tidy(aes$order, data)
    if (is.factor(x)) x <- as.character(x)
    if (is.factor(order)) order <- as.character(order)

    new_order_mapping <- unique(data.frame(x = x,
                                           order = order,
                                           stringsAsFactors = FALSE))
  }

  # combine the existing order mapping with the new one and get unique entries
  if (class_differs(gg$data[[panel]]$order_mapping, new_order_mapping)) {
    stop(
      paste0(
        "Do not know how to join together ordering variables with classes ",
        class(new_order_mapping$order),
        " and ",
        class(gg$data[[panel]]$order_mapping$order),
        " (panel ",
        panel,
        ").\n> Perhaps you added layers to the same panel with different ordering variables (or didn't specify an ordering variable for one of the layers)?" #nolint
      ),
      call. = FALSE
    )
  }
  gg$data[[panel]]$order_mapping <- unique(
    rbind(new_order_mapping,
          gg$data[[panel]]$order_mapping)
  )

  # Check for ambiguity
  if (anyDuplicated(gg$data[[panel]]$order_mapping$x) ||
      nrow(gg$data[[panel]]$order_mapping) != length(gg$data[[panel]]$x)) {
    stop("Ordering is ambiguous.
Some x values associate with multiple values of the ordering variable, or there are no observations of the ordering variable for some x values.", #nolint
         call. = FALSE)
  }

  # sort the order mapping
  reorder <- order(gg$data[[panel]]$order_mapping$order)
  gg$data[[panel]]$order_mapping <- gg$data[[panel]]$order_mapping[reorder, ]

  # get how the order mapping should remap x values
  reorder <- match(gg$data[[panel]]$order_mapping$x, gg$data[[panel]]$x)
  gg$data[[panel]]$x <- gg$data[[panel]]$x[reorder]

  # for each series join on the order mapping and then order accordingly
  for (i in 1:length(gg$data[[panel]]$series)) {
    tmp <- data.frame(x = series_x_values(gg$data[[panel]], i),
                      stringsAsFactors = FALSE)
    tmp <- dplyr::left_join(tmp, gg$data[[panel]]$order_mapping, by = "x")
    reorder <- order(tmp$order)

    gg$data[[panel]]$series[[i]]$x <-
      series_x_values(gg$data[[panel]], i)[reorder]

    gg$data[[panel]]$series[[i]]$y <-
      series_values(gg$data[[panel]], i)[reorder]
  }

  gg
}

addlayertopanel <- function(gg, data, aes, panel, geomtype) {
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
  gg <- assign_series(gg, data, aes, panel, geomtype)

  ## now reorder (x values)
  gg <- reorder_series(gg, data, aes, panel)

  list(
    gg = gg,
    new_series_indices = (existing_series + 1):length(gg$data[[panel]]$series)
  )
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

  aes
}

inherit_data <- function(gg, data) {
  # if data is null, inherit from parent
  if (is.null(data)) {
    data <- gg$data[["parent"]]
    if (is.null(data)) {
      stop("You have not supplied data for series", call. = FALSE)
    }
  }
  return(data)
}

addlayer <- function(gg, new, panel, geomtype) {
  aes <- inherit_aes(gg, new$aes)
  data <- inherit_data(gg, new$data)
  # Error if data is weird
  if (!is.acceptable.data(data)) {
    stop(paste0("Data is of unsupported type (you passed in ",
                class(data), ")"),
         call. = FALSE)
  }

  if (is_null_quo(aes$facet)) {
    out <- addlayertopanel(gg, data, aes, panel, geomtype)
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
        keep_rows <- !is.na(rlang::eval_tidy(aes$facet, data)) &
          rlang::eval_tidy(aes$facet, data) == facets[i]
      } else {
        subset_data <- is.na(rlang::eval_tidy(aes$facet, data))
      }
      subset_data <- data[keep_rows, ]
      out <- addlayertopanel(gg, subset_data, aes, panel, geomtype)
      gg <- out$gg
      gg$paneltitles[[panel]] <- ifelse(is.na(facets[i]),
                                        "",
                                        as.character(facets[i]))
      newseries[[panel]] <- out$new_series_indices
    }
  }

  list(gg = gg, newseries = newseries)
}

applyattributes <- function(gg, this, panel, allnewseries) {
  gg <- applyattribute(gg, "col", panel, allnewseries, this$colour)
  gg <- applyattribute(gg, "pch", panel, allnewseries, this$pch)
  gg <- applyattribute(gg, "lty", panel, allnewseries, this$lty)
  gg <- applyattribute(gg, "lwd", panel, allnewseries, this$lwd)
  gg <- applyattribute(gg, "pointsize", panel, allnewseries, this$pointsize)
  gg <- applyattribute(gg, "barcol", panel, allnewseries, this$barcol)
  gg <- applyattribute(gg, "col", panel, allnewseries, this$col)

  gg
}

change_bar_order <- function(gg, reorder_bars, panel) {
  snames <- series_names(gg$data[[panel]])
  # special case NAs
  reorder_bars[is.na(reorder_bars)] <- "<NA>"

  if (any(!reorder_bars %in% snames)) {
    undefined_series <- reorder_bars[!reorder_bars %in% snames]
    warning(
      paste0(
        "Cannot reorder bar series `",
        paste(undefined_series, collapse = "`, `"),
        "` as it does not exist; ignoring."
      )
    )
    reorder_bars <- reorder_bars[reorder_bars %in% snames]
  }
  if (any(!snames %in% reorder_bars)) {
    undefined_series <- snames[!snames %in% reorder_bars]
    warning(
      paste0(
        "You did not manually specify an order for `",
        paste(undefined_series, collapse = "`, `"),
        "`; these will be order alphabetically _after_ the series you manually ordered" #nolint
      )
    )
  }

  existing_indices <- data.frame(
    series = snames,
    existing_index = 1:length(snames),
    stringsAsFactors = FALSE
  )
  new_order <- data.frame(
    new_index = 1:length(reorder_bars),
    series = reorder_bars,
    stringsAsFactors = FALSE
  )
  new_order <- dplyr::left_join(existing_indices, new_order, by = "series")
  new_order <- order(new_order$new_index, new_order$series, na.last = TRUE)

  gg$data[[panel]]$series <- gg$data[[panel]]$series[new_order]

  gg
}

.addseries <- function(gg, newseries, type) {
  panel <- newseries$panel
  out <- addlayer(gg, newseries, panel, type)
  gg <- out$gg
  allnewseries <- out$newseries

  if (!is.list(allnewseries)) {
    gg <- applyattributes(gg, newseries, panel, allnewseries)
  } else {
    for (panel in names(allnewseries)) {
      gg <- applyattributes(gg, newseries, panel, allnewseries[[panel]])
    }
  }

  if (type == "bar" && !is.null(newseries$stacked)) {
    gg$stacked <- newseries$stacked
  }

  if (!is.null(newseries$reorder_bars)) {
    gg <- change_bar_order(gg, newseries$reorder_bars, panel)
  }

  gg
}

addseries <- function(gg, newseries, type) {
  for (p in newseries$panel) {
    tmp <- newseries
    tmp$panel <- p
    gg <- .addseries(gg, tmp, type)
  }

  gg
}

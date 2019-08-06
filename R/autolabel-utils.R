panel_has_multiple_series <- function(p, data, layout) {
  other_p <- other_axes(p, layout)
  n_series <- length(data[[p]]$series)
  if (!is.null(other_p)) {
    n_series <- n_series + length(data[[other_p]]$series)
  }
  return(n_series > 1)
}

convert_series_to_label <- function(series_entry, ylim, layout) {
  p <- series_entry$panels[1]
  other_p <- other_axes(p, layout)
  if (!is.null(other_p) && !identical(ylim[[p]], ylim[[other_p]])) {
    series_entry$label <- paste0(series_entry$name,"\n",ifelse(is.even(p),"(RHS)","(LHS)"))
  } else {
    series_entry$label <- series_entry$name
  }
  return(series_entry)
}

series_appears_in_panels <- function(series_entry, data, layout, labels, ignore_existing_labels) {
  panels <- NULL
  for (p in names(data)) {
    if (panel_has_multiple_series(p, data, layout) && notalreadylabelled(p, labels, ignore_existing_labels)) {
      for (series in data[[p]]$series) {
        if (identical(legend_entry(series), series_entry)) {
          if (is.null(panels)) {
            panels <- c(p)
          } else {
            panels <- append(panels, p)
          }
        }
      }
    }
  }
  return(panels)
}

get_series_objects <- function(series_entry, data, layout, labels, ignore_existing_labels) {
  for (p in names(data)) {
    if (panel_has_multiple_series(p, data, layout) && notalreadylabelled(p, labels, ignore_existing_labels)) {
      for (series in data[[p]]$series) {
        if (identical(legend_entry(series), series_entry)) {
          return(series)
        }
      }
    }
  }
}

simplify_label <- function(label) {
  list(name = label$name, col = ifelse(is.null(label$fill), label$col, label$fill))
}

strip_layer_duplicates <- function(unique_labels) {
  # here we collapse attributes down to just colour and strip any duplicates
  # e.g. a bar and a line with the same name and colour
  simplified <- lapply(unique_labels, simplify_label)
  unique_labels[!duplicated(simplified)]
}

createlabels <- function(data, layout, labels, ylim, ignore_existing_labels) {
  unique_labels <- getlegendentries(data) # This gets us all unique series
  unique_labels <- strip_layer_duplicates(unique_labels)

  # Now we match each unique series to the panels it appears in
  panels <-
    lapply(
      unique_labels,
      series_appears_in_panels,
      data = data,
      layout = layout,
      labels = labels,
      ignore_existing_labels = ignore_existing_labels
    )

  matching_series <-
    lapply(
      unique_labels,
      get_series_objects,
      data = data,
      layout = layout,
      labels = labels,
      ignore_existing_labels = ignore_existing_labels
    )

  # Merge that panel data in (we do it this way, so it doesn't ruin the comparison)
  for (i in seq_along(unique_labels)) {
    unique_labels[[i]]$panels <- panels[[i]]
    unique_labels[[i]]$series <- matching_series[[i]]
  }
  unique_labels <- unique_labels[!sapply(panels, is.null)]

  # go through and add annotations - if necessary and (LHS) and (RHS)
  unique_labels <- lapply(unique_labels, convert_series_to_label, ylim = ylim, layout = layout)

  return(unique_labels)
}

other_axes <- function(p, layout) {
  if ((layout == "1" || layout == "2h" || layout == "3h" || layout == "4h")) {
    otherp <- switch(p,
                     "1" = "2",
                     "2" = "1",
                     "3" = "4",
                     "4" = "3",
                     "5" = "6",
                     "6" = "5",
                     "7" = "8",
                     "8" = "7")
    return(otherp)
  } else {
    return(NULL)
  }
}

notalreadylabelled <- function(p, labels, ignore_existing_labels) {
  if (ignore_existing_labels) return(TRUE)
  for (label in labels) {
    if (label$panel == p) {
      return(FALSE)
    }
  }
  return(TRUE)
}

segment_intersection <- function(x1, y1, x2, y2, a1, b1, a2, b2) {
  if (is.na(x1) || is.na(y1) || is.na(x2) || is.na(y2) || is.na(a1) ||
      is.na(b1) || is.na(a2) || is.na(b2)) {
    return(NULL)
  }

  t_a <- ((b1 - b2)*(x1 - a1) + (a2 - a1)*(y1 - b1)) / ((a2 - a1)*(y1 - y2) - (x1 - x2)*(b2 - b1))
  t_b <- ((y1 - y2)*(x1 - a1) + (x2 - x1)*(y1 - b1)) / ((a2 - a1)*(y1 - y2) - (x1 - x2)*(b2 - b1))

  if (t_a >= 0 && t_a <= 1 && t_b >= 0 && t_b <= 1) {
    return(list(x = x1 + t_a*(x2-x1), y = y1 + t_b*(y2-y1)))
  } else {
    return(NULL)
  }
}

bounding_box_intersection <- function(x,y,a,b,h,w) {
  # Construct the bounding box segments
  x1 <- c(x - w/2, x - w/2, x + w/2, x + w/2)
  x2 <- c(x - w/2, x + w/2, x + w/2, x - w/2)
  y1 <- c(y - h/2, y + h/2, y + h/2, y - h/2)
  y2 <- c(y + h/2, y + h/2, y - h/2, y - h/2)

  intersections <- mapply(x1, y1, x2, y2, FUN = function(a0, b0, a1, b1) segment_intersection(a0, b0, a1, b1, a, b, x, y))
  return(intersections[!sapply(intersections, is.null)][[1]])
}

add_arrow <- function(found_location, text, colour, p, inches_conversion) {
  if ((!found_location$los && found_location$distance > 0.1) || found_location$distance > 1.5) {
    # adjust the tail to the edge of the bounding box
    adjusted <-
      bounding_box_intersection(
        found_location$x,
        found_location$y,
        found_location$xx,
        found_location$yy,
        getstrheight(text, units = "user"),
        getstrwidth(text, units = "user")
      )
    # Check the distance adjusted for bounding box is still long enough to warrant arrow
    if (sqrt(((adjusted$x-found_location$xx)*inches_conversion$x)^2 + ((adjusted$y-found_location$yy)*inches_conversion$y)^2) > 0.1) {
      newarrow <- list(tail.x = adjusted$x,
                       tail.y = adjusted$y,
                       head.x = found_location$xx,
                       head.y = found_location$yy,
                       colour = colour,
                       panel = p)
      drawarrow(newarrow)
    } else {
      newarrow <- NULL
    }
  } else {
    newarrow <- NULL
  }
  return(newarrow)
}

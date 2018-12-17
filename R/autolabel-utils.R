createlabels <- function(panels, bars, attributes, layout, labels) {
  legend <- getlegendentries(panels, bars, attributes)

  # match each entry to which panels it appears in
  series_to_panels <- list()
  for (entry in legend) {
    for (p in names(panels)) {
      if (notalreadylabelled(p, labels) && (entry$name %in% panels[[p]]) &&
          any(entry$col == attributes[[p]]$col[[entry$name]], entry$fill == attributes[[p]]$col[[entry$name]])) {
        series_to_panels[[entry$name]] <- append(series_to_panels[[entry$name]], p)
      }
    }
  }

  # Remove any single-panel series if that panel has only one series (doesn't need a label!)
  for (series in names(series_to_panels)) {
    if (length(series_to_panels[[series]]) == 1 && length(panels[[series_to_panels[[series]]]]) == 1) {
      series_to_panels[series] <- NULL
    }
  }

  # if is a RHS axes, add annotations
  series_to_labels <- list()
  for (series in names(series_to_panels)) {
    if (layout == "1" || layout == "2h" || layout == "3h" || layout == "4h") {
      if (length(panels[[other_axes(series_to_panels[[series]][1], layout)]]) > 0) { # This isn't handling all possible corner cases, because it could be that there aren't RHS on this panel but are on subsequent ones where the series also appears. Seems like a very unlikely to occur corner case.
        if (any(c("1","3","5","7") %in% series_to_panels[[series]])) {
          series_to_labels[[series]] <- paste0(series,"\n(LHS)")
        } else {
          series_to_labels[[series]] <- paste0(series,"\n(RHS)")
        }
      } else {
        series_to_labels[[series]] <- series
      }
    } else {
      series_to_labels[[series]] <- series
    }
  }

  list(series_to_labels = series_to_labels, series_to_panels = series_to_panels)
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

notalreadylabelled <- function(p, labels) {
  for (label in labels) {
    if (label$panel == p) {
      return(FALSE)
    }
  }
  return(TRUE)
}


get_series_type <- function(series, bars, lty) {
  if (series %in% bars) {
    return("bar")
  } else if (lty[[series]] == 0) {
    return("point")
  } else {
    return("line")
  }
}

get_series_types <- function(series_list, attributes, bars) {
  series_types <- lapply(series_list, function(x) get_series_type(x, bars, attributes$lty))
  names(series_types) <- series_list
  return(series_types)
}

segment_intersection <- function(x1, y1, x2, y2, a1, b1, a2, b2) {
  if (is.na(x1) || is.na(y1) || is.na(x2) || is.na(y2) || is.na(a1) || is.na(b1) || is.na(a2) || is.na(b2)) {
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

add_arrow <- function(found_location, text, color, p, inches_conversion) {
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
                       color = color,
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

createlabels <- function(panels, bars, attributes, layout, labels) {
  legend <- getlegendentries(panels, bars, attributes)

  # match each entry to which panels it appears in
  series_to_panels <- list()
  for (entry in legend) {
    for (p in names(panels)) {
      if (notalreadylabelled(p, labels) && (entry$name %in% panels[[p]]) &&
          (entry$col == attributes[[p]]$col[[entry$name]] || entry$fill == attributes[[p]]$col[[entry$name]])) {
        series_to_panels[[entry$name]] <- append(series_to_panels[[entry$name]], p)
      }
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

add_arrow <- function(found_location, color, p) {
  if (!found_location$los || found_location$distance > 1.5) {
    newarrow <- list(tail.x = found_location$x,
                     tail.y = found_location$y,
                     head.x = found_location$xx,
                     head.y = found_location$yy,
                     color = color,
                     panel = p)
    drawarrow(newarrow)
  } else {
    newarrow <- NULL
  }
  return(newarrow)
}

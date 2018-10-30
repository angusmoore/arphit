createlabels <- function(panels, bars, attributes, layout) {
  legend <- getlegendentries(panels, bars, attributes)

  # match each entry to which panels it appears in
  series_to_panels <- list()
  for (entry in legend) {
    for (p in names(panels)) {
      if ((entry$name %in% panels[[p]]) &&
          (entry$col == attributes[[p]]$col[[entry$name]])) {
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

notRHS <- function(p, layout) {
  if (layout == "1" || layout == "2h" || layout == "3h" || layout == "4h") {
    if (p == "2" || p == "4" || p == "6" || p == "8") {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(TRUE)
  }
}

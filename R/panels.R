checklayout <- function(series, layout) {
  # Switch through the layout options
  if (layout == "1") {
    maxnp <- 2
  } else if (layout == "2h") {
    maxnp <- 4
  } else if (layout == "2v") {
    maxnp <- 2
  } else if (layout == "2b2") {
    maxnp <- 4
  } else {
    stop(paste("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2.", sep = ""))
  }

  permittedpanels <- as.character(1:maxnp)
  for (i in names(series)) {
    if (!(i %in% permittedpanels)) {
      stop(paste("Your chosen layout (", layout, ") does not have a panel ", i, ".", sep = ""))
    }
  }
  return(permittedpanels)
}

handlepanels <- function(series, layout) {
  if (!is.list(series)) {
    # Haven't supplied panel names. That's ok. Assume just all on one panel)
    series <- list("1" = series)
  }
  # Sense check layout
  permittedpanels <- checklayout(series, layout)

  panels <- list()
  for (p in permittedpanels) {
    if (!is.null(series[[p]])) {
      panels[p] <- series[p]
  	  if (any(duplicated(series[[p]]))) {
  		  stop(paste("Duplicate series identifiers in panel ", p, " (", paste(duplicated(series[[p]]), sep = "", collapse = ", "), ")", sep = ""))
  	  }
    } else {
      panels[p] <- list(NULL)
    }
  }

  return(panels)
}

handlebars <- function(panels, bars) {
  if (is.logical(bars)) {
  	# Everything is a bar!
  	bars <- list()
  	for (p in names(panels)) {
  		bars[[p]] <- panels[[p]]
  	}
  	return(bars)
  }
  if (!is.list(bars)) {
  	# Haven't supplied bars as a per panel thing. This is fine, but may be an issue
  	oldbar <- bars
  	bars <- list()
  	for (p in names(panels)) {
  	  bars[[p]] <- oldbar
  	}
  }

  newbars <- list()
  for (p in names(panels)) {
    newbars[[p]] <- c()
  	for (s in panels[[p]]) {
  	  if (s %in% bars[[p]]) {
  	    newbars[[p]] <- append(newbars[[p]], s)
  	  }
  	}
  }
  return(newbars)
}

maxpanels <- function(layout) {
  # Switch through the layout options
  if (layout == "1" || layout == "2v") {
    maxnp <- 2
  } else if (layout == "2h" || layout == "2b2") {
    maxnp <- 4
  } else if (layout == "3v") {
    maxnp <- 3
  } else if (layout == "3h" || layout == "3b2") {
    maxnp <- 6
  } else if (layout == "4h" || layout == "4b2") {
    maxnp <- 8
  } else {
    stop(paste0("Unknown layout option ", layout, ". Options are 1, 2h, 2v, 2b2, 3v, 3h, 3b2, 4b2."))
  }
  return(maxnp)
}

checklayout <- function(series, layout) {
  maxnp <- maxpanels(layout)
  permittedpanels <- as.character(1:maxnp)
  for (i in names(series)) {
    if (!(i %in% permittedpanels)) {
      stop(paste("Your chosen layout (", layout, ") does not have a panel ", i, ".", sep = ""))
    }
  }
  return(permittedpanels)
}

handlepanels <- function(series, layout) {
  # Sense check layout
  permittedpanels <- checklayout(series, layout)

  panels <- list()
  for (p in permittedpanels) {
    if (!is.null(series[[p]])) {
      panels[[p]] <- series[[p]]
    } else {
      panels[p] <- list(NULL)
    }
  }

  return(panels)
}

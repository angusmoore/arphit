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
      stop(paste("Your chose layout (", layout, ") does not have a panel ", i, ".", sep = ""))
    }
  }
  return(permittedpanels)
}

handlepanels <- function(series, bars, layout) {
  if (!is.list(series)) {
    # Haven't supplied label names. That's ok. Assume just all on one panel)
    series <- list("1" = series)
  }

  # Sense check layout
  permittedpanels <- checklayout(series, layout)

  panels <- list()
  serieslist <- list()
  duplicates <- list()
  barlist <- list()

  for (p in permittedpanels) {
    if (!is.null(series[[p]])) {
      panels[p] <- series[p]
      for (s in series[[p]]) {
        if (s %in% names(serieslist)) {
          # Already exists. First check if just a duplicate in this panel, if so ignore
          if (serieslist[s] == p) {
            message(paste("You duplicated series ", s, " in panel ", p, ". Ignoring the duplicate.", sep = ""))
          } else {
            # from another panel. Need to give each an identifier
            # First replace the existing series and add it to the duplicate list
            message("I need to remove and replace duplicates from the panel list too.")
            serieslist[paste(s, serieslist[s], sep="")] <- serieslist[s]
            if (s %in% bars || paste(s, serieslist[s], sep="") %in% bars) {
              barlist[paste(s, serieslist[s], sep="")] <- TRUE
            }
            duplicates[paste(s, serieslist[s], sep="")] <- s

            # Now add the current one
            serieslist[paste(s, p, sep = "")] <- p
            if (s %in% bars || paste(s, p, sep = "") %in% bars) {
              barlist[paste(s, p, sep = "")] <- TRUE
            }
            duplicates[paste(s, p, sep = "")] <- s
          }
        } else {
          serieslist[s] <- p
          if (s %in% bars) {
            barlist[s] <- TRUE
          }
        }
      }
    } else {
      panels[p] <- list(NULL)
    }
  }

  # Now remove duplicates
  for (d in unlist(duplicates,use.names=FALSE)) {
    serieslist[d] <- NULL
    barlist[d] <- NULL
  }

  # And lastly, order the names
  serieslist <- serieslist[order(names(serieslist))]

  return(list("panels" = panels, "serieslist" = serieslist, "duplicates" = duplicates, "bars" = barlist))
}

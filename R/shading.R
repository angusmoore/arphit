shadingsanity <- function(to, from, panels, p) {
  if (!(to %in% panels[[p]])) {
    stop(paste("Series ", to, " from your shading options is not a recognised series in panel ", p, ".", sep = ""))
  }
  if (!(from %in% panels[[p]])) {
    stop(paste("Series ", from, " from your shading options is not a recognised series in panel ", p, ".", sep = ""))
  }
}

findpanel <- function(panels, name) {
  match <- NULL
  for (p in c("1","2","3","4")) {
  	if (name %in% panels[[p]]) {
  	  if (is.null(match)) {
          match <- p
  	  } else {
  	    stop(paste("Cannot construct shading from series ", name, " because its name is in more than one panel. Supply a panel identifier for the shading.", sep = ""))
  	  }
    }
  }
  if (is.null(match)) {
    stop(paste("Cannot shade with series ", name, " because it does not exist in any panel.", sep = ""))
  }
  return(match)
}

handleshading <- function(shading, panels) {

  out <- list()
  for (p in names(panels)) {
    out[[p]] <- list()
  }

  for (s in shading) {
  	if (is.null(s$panel)) {
  	  s$panel <- findpanel(panels, s$to)
  	  message(paste("No panel identifier supplied for shading. Assuming for panel ", s$panel, " where a matching series can be found.", sep = ""))
  	}
    shadingsanity(s$to, s$from, panels, s$panel)

    i <- length(out[[s$panel]]) + 1
    out[[s$panel]][[i]] <- s
    # Don't need the panel identifier (since it is in the list)
    out[[s$panel]][[i]]$panel <- NULL
  }

  return(out)
}

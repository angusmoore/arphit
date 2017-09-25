shadingsanity <- function(to, from, panels) {
  if (!(to %in% names(panels$serieslist))) {
    stop(paste("Series ", to, " from your shading options is not a recognised series.", sep = ""))
  }
  if (!(from %in% names(panels$serieslist))) {
    stop(paste("Series ", from, " from your shading options is not a recognised series.", sep = ""))
  }
  if (to %in% panels$duplicates && from %in% panels$duplicates) {
    stop(paste("Both ", to, " and ", from, " are non-uniquely identified across panels. Cannot shade between them.", sep = ""))
  }
}

handleshading_duplicates <- function(shading, panels) {
  out <- list()
  i <- 0
  for (s in shading) {
    # Sanity check
    shadingsanity(s$to, s$from, panels)
    i <- i + 1
    if (s$to %in% panels$duplicates) {
      # Check for which panel other series is in
      p <- panels$serieslist[[s$from]]
      out[[i]] <- list(to = paste(s$to, p, sep = ""), from = s$from, color = s$color)
    } else {
      out[[i]] <- s
    }
  }
  shading <- out

  # Repeat, with from
  out <- list()
  i <- 0
  for (s in shading) {
    i <- i + 1
    if (s$from %in% panels$duplicates) {
      # Check for which panel other series is in
      p <- panels$serieslist[[s$to]]
      out[[i]] <- list(to = s$to, from = paste(s$from, p, sep = ""), color = s$color)
    } else {
      out[[i]] <- s
    }
  }
  return(out)
}

handleshading <- function(shading, panels) {

  shading <- handleshading_duplicates(shading, panels)

  out <- list()
  for (p in names(panels$panels)) {
    out[[p]] <- list()
  }

  for (s in shading) {
    p <- panels$serieslist[[s$to]]
    i <- length(out[[p]]) + 1
    out[[p]][[i]] <- s
  }

  return(out)
}

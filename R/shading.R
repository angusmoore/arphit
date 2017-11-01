shadingsanity <- function(to, from, panels) {
  if (!(to %in% names(panels$serieslist))) {
    stop(paste("Series ", to, " from your shading options is not a recognised series. Perhaps it was a duplicate (which get renamed by arphit internally)?", sep = ""))
  }
  if (!(from %in% names(panels$serieslist))) {
    stop(paste("Series ", from, " from your shading options is not a recognised series. Perhaps it was a duplicate (which get renamed by arphit internally)?", sep = ""))
  }
  if (serieslist[[from]] != serieslist[[to]]) {
    stop(paste("Series are not in the same panel, cannot shade between them. ", from, " is in panel ", serieslist[[from]], ", ", to, " is in panel ", serieslist[[to]], sep = ""))
}

handleshading <- function(shading, panels) {

  out <- list()
  for (p in names(panels$panels)) {
    out[[p]] <- list()
  }

  for (s in shading) {
    shadingsanity(s$to, s$from, panels)
    p <- panels$serieslist[[s$to]]
    i <- length(out[[p]]) + 1
    if (is.null(s$color)) {
      s$color <- DEFAULTSHADINGCOLOR
    }
    out[[p]][[i]] <- s
  }

  return(out)
}

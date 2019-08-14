shadingsanity <- function(to, from, data, p) {
  if (!(to %in% series_names(data[[p]]))) {
    stop(paste0("Series `", to,
                "` (the `to` series in shading) is not a recognised series in panel ", #nolint
                p),
         call. = FALSE)
  }
  if (!(from %in% series_names(data[[p]]))) {
    stop(paste0("Series `", from,
                "` (the `from` series in shading) is not a recognised series in panel ", #nolint
                p),
         call. = FALSE)
  }
}

findpanel <- function(data, name) {
  match <- NULL
  for (p in names(data)) {
    if (name %in% series_names(data[[p]])) {
      if (is.null(match)) {
          match <- p
      } else {
        stop(paste0("Cannot construct shading from series `", name,
  	               "` because its name is in more than one panel. Supply a panel identifier for the shading."), #nolint
             call. = FALSE)
      }
    }
  }
  if (is.null(match)) {
    stop(paste0("Cannot shade with series `", name,
                "` because it does not exist in any panel."),
         call. = FALSE)
  }

  match
}

handleshading <- function(shading, data) {

  out <- list()
  for (p in names(data)) {
    out[[p]] <- list()
  }

  for (s in shading) {
    if (is.null(s$panel)) {
      s$panel <- findpanel(data, s$to)
      message(
        paste0(
          "No panel identifier supplied for shading. Assuming for panel ",
          s$panel,
          " where a matching series can be found."
        ),
        call. = FALSE
      )
    }
    shadingsanity(s$to, s$from, data, s$panel)

    i <- length(out[[s$panel]]) + 1
    out[[s$panel]][[i]] <- s
    # Don't need the panel identifier (since it is in the list)
    out[[s$panel]][[i]]$panel <- NULL
  }

  out
}

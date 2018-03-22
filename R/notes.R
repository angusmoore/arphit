conformpaneltitles <- function(panels, paneltitles, layout, maxchars) {
  if (layout == "2v" || layout == "2b2") {
    maxchars <- floor(1/2 * maxchars)
  } else {
    maxchars <- maxchars
  }

  out <- list()
  if (!is.list(paneltitles)) {
    for (p in names(panels)) {
      if (!is.null(paneltitles)) {
        out[[p]] <- splitoverlines(paneltitles, maxchars)
      } else {
        out[p] <- list(NULL)
      }
    }
  } else {
    for (p in names(panels)) {
      if (p %in% names(paneltitles)) {
        out[[p]] <- splitoverlines(paneltitles[[p]], maxchars)
      } else {
        out[p] <- list(NULL)
      }
    }
  }
  return(out)
}

formatsrcs <- function(sources) {
  if (!is.null(sources)) {
    for (i in 1:length(sources)) {
      if (i == 1) {
        out <- sources[i]
      } else {
        out <- paste(out, "; ", sources[i], sep = "")
      }
    }
    if (length(sources) > 1) {
      srcplural <- TRUE
    } else {
      srcplural <- FALSE
    }

    return(list(text = splitoverlines(out, LINELENGTHNOTES), plural = srcplural))
  } else {
    return(list(text = "", plural = FALSE))
  }
}

formatfn <- function(footnotes) {
  if (length(footnotes) > 0) {
    for (i in 1:length(footnotes)) {
      footnotes[i] <- splitoverlines(footnotes[[i]], LINELENGTHNOTES)
    }
  }
  return(footnotes)
}

splitoverlines <- function(s, maxsize) {
  breakpoints <- c()
  if (nchar(s) > 0) {
    lastspace <- NULL
    cumul <- 0
    for (i in 1:nchar(s)) {
      cumul <- cumul + 1
      if (substr(s, i, i) == " ") {
        lastspace <- i
      } else if (substr(s, i, i) == "\n") {
        cumul <- 0
        lastspace <- NULL
      }
      if (cumul > maxsize && !is.null(lastspace)) {
        breakpoints <- append(breakpoints, lastspace)
        cumul <- 0
        lastspace <- NULL
      }
    }
  }
  # Now replace the breakpoints
  if (length(breakpoints) > 0) {
    for (i in breakpoints) {
      substr(s, i, i) <- "\n"
    }
  }
  return(s)
}

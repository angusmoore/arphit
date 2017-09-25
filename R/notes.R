conformpaneltitles <- function(panels, paneltitles) {
  out <- list()
  if (!is.list(paneltitles)) {
    for (p in names(panels$panels)) {
      if (!is.null(paneltitles)) {
        out[[p]] <- paneltitles
      } else {
        out[p] <- list(NULL)
      }
    }
  } else {
    for (p in names(panels$panels)) {
      if (p %in% names(paneltitles)) {
        out[[p]] <- paneltitles[[p]]
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

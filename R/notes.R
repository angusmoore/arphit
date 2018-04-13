conformpaneltitles <- function(panels, paneltitles, layout, width) {
  if (layout == "2v" || layout == "2b2" || layout == "3b2" || layout == "4b2") {
    width <- floor(1/2 * width)
  } else if (layout == "3v") {
    width <- floor(1/3 * width)
  } else {
    width <- width
  }

  out <- list()
  if (!is.list(paneltitles)) {
    for (p in names(panels)) {
      if (!is.null(paneltitles)) {
        out[[p]] <- splitoverlines(paneltitles, width, 1)
      } else {
        out[p] <- list(NULL)
      }
    }
  } else {
    for (p in names(panels)) {
      if (p %in% names(paneltitles)) {
        out[[p]] <- splitoverlines(paneltitles[[p]], width, 18/20)
      } else {
        out[p] <- list(NULL)
      }
    }
  }
  return(out)
}

formatsrcs <- function(sources, width) {
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

    return(list(text = splitoverlines(out, width, 14/20), plural = srcplural))
  } else {
    return(list(text = "", plural = FALSE))
  }
}

formatfn <- function(footnotes, width) {
  if (length(footnotes) > 0) {
    for (i in 1:length(footnotes)) {
      footnotes[i] <- splitoverlines(footnotes[[i]], width, 14/20)
    }
  }
  return(footnotes)
}

splitoverlines <- function(s, maxsize, cex) {
  # Set values needed to determine str widths
  graphics::par(family = "sans", xaxs = "i", yaxs = "i", ps = 20, las = 1, lheight = 1)

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
      string <- substr(s, i-cumul, i)
      if (graphics::strwidth(string, units = "inches", cex = cex) > maxsize && !is.null(lastspace)) {
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

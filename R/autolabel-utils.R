createlabels <- function(data, panels, p, layout) {
  series <- list()
  if (layout == "1" || layout == "2h") {
    # also need to handle RHS series
    primary <- panels[[p]]
    if (p == "1") {
      secondary <- panels[["2"]]
    } else if (p == "3") {
      secondary <- panels[["4"]]
    } else {
      secondary <- list() # Shouldn't be called
    }

    for (s in primary) {
      if (length(secondary) > 0) {
        series[[s]] <- paste(s, "\n(LHS)", sep = "")
      } else {
        series[[s]] <- s
      }
    }
    for (s in secondary) {
      if (length(secondary) > 0) {
        series[[s]] <- paste(s, "\n(RHS)", sep = "")
      } else {
        series[[s]] <- s
      }
    }
  } else {
    for (s in panels[[p]]) {
      series[[s]] <- s
    }
  }
  return(series)
}

rhs_axes <- function(p, layout) {
  if ((layout == "1" || layout == "2h" || layout == "3h" || layout == "4h")) {
    if (p == "1") {
      otherp <- "2"
    } else if (p == "3") {
      otherp <- "4"
    } else if (p == "5") {
      otherp <- "6"
    } else if (p == "7") {
      otherp <- "8"
    } else {
      return(NULL)
    }
    return(otherp)
  } else {
    return(NULL)
  }
}

notalreadylabelled <- function(p, labels) {
  for (label in labels) {
    if (label$panel == p) {
      return(FALSE)
    }
  }
  return(TRUE)
}

notRHS <- function(p, layout) {
  if (layout == "1" || layout == "2h" || layout == "3h" || layout == "4h") {
    if (p == "2" || p == "4" || p == "6" || p == "8") {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(TRUE)
  }
}

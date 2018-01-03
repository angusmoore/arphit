
createlabels <- function(data, panels, p, layout) {
  series <- list()
  if (layout == "1" || layout == "2h") {
    # also need to handle RHS series
    primary <- panels$panels[[p]]
    if (p == "1") {
      secondary <- panels$panels[["2"]]
    } else if (p == "3") {
      secondary <- panels$panels[["4"]]
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
    for (s in panels$panels[[p]]) {
      series[[s]] <- s
    }
  }
  return(series)
}

convertaxis <- function(x, fromaxis, toaxis) {
  fromspan <- fromaxis$max - fromaxis$min
  tospan <- toaxis$max - toaxis$min
  y <- (x - fromaxis$min) / fromspan * tospan  + toaxis$min
  return(y)
}

convertdata.axes <- function(data, panels, p, layout, ylim_list) {
  if ((layout == "1" || layout == "2h")) {
    # need to convert the RHS to LHS so the labeller can correctly do the collisions etc
    toaxis <- ylim_list[[p]]
    if (p == "1") {
      otherp <- "2"
    } else if (p == "3") {
      otherp <- "3"
    } else {
      stop("Invalid panel for converting axes.")
    }
    fromaxis <- ylim_list[[otherp]]
    for (s in panels$panels[[otherp]]) {
      data[, s] <- convertaxis(data[, s], fromaxis, toaxis)
    }
  }
  return(data)
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
  if (layout == "1" || layout == "2h") {
    if (p == "2" || p == "4") {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(TRUE)
  }
}

formatlabels <- function(locations, labelsmap, col, p) {
  formatted <- list()
  for (label in names(locations)) {
    x <- locations[[label]][1]
    y <- locations[[label]][2]
    thislabel <- list(x = x, y = y, text = labelsmap[[label]], panel = p, color = col[[label]])
    formatted <- append(formatted, list(thislabel))
  }
  return(formatted)
}

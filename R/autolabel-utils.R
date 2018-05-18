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

convertaxis <- function(x, fromaxis, toaxis) {
  fromspan <- fromaxis$max - fromaxis$min
  tospan <- toaxis$max - toaxis$min
  y <- (x - fromaxis$min) / fromspan * tospan  + toaxis$min
  return(y)
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

convertdata.axes <- function(data, panels, p, layout, ylim_list) {
  otherp <- rhs_axes(p, layout)
  if (!is.null(otherp)) {
    # need to convert the RHS to LHS so the labeller can correctly do the collisions etc
    toaxis <- ylim_list[[p]]
    fromaxis <- ylim_list[[otherp]]
    otherdata <- data[[otherp]]
    for (s in panels[[otherp]]) {
      otherdata[, s] <- convertaxis(data[[otherp]][, s], fromaxis, toaxis)
    }
    if (!is.null(otherdata)) {
      return(dplyr::full_join(data[[p]], otherdata))
    } else {
      return(data[[p]])
    }
  } else {
    return(data[[p]])
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

merge_colours <- function(attributes, p, layout) {
  otherp <- rhs_axes(p, layout)
  if (!is.null(otherp)) {
    return(append(attributes[[p]]$col, attributes[[otherp]]$col))
  } else {
    return(attributes[[p]]$col)
  }
}

formatlabels <- function(locations, labelsmap, attributes, p, layout) {
  col <- merge_colours(attributes, p, layout)
  formatted <- list()
  for (label in names(locations)) {
    x <- locations[[label]][1]
    y <- locations[[label]][2]
    thislabel <- list(x = x, y = y, text = labelsmap[[label]], panel = p, color = col[[label]])
    formatted <- append(formatted, list(thislabel))
  }
  return(formatted)
}

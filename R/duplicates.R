finddups <- function(series) {
  slist <- c()
  for (p in series) {
    slist <- c(slist, p)
  }
  dups <- c()
  for (s in unique(slist)) {
    if (sum(s == slist) > 1) {
      dups <- c(dups, s)
    }
  }
  return(dups)
}

findduppanels <- function(dups, series) {
  dupmap <- list()
  for (d in dups) {
    dupmap[[d]] <- c()
    for (p in names(series)) {
      if (d %in% series[[p]]) {
        dupmap[[d]] <- c(dupmap[[d]], p)
      }
    }
  }
  return(dupmap)
}

renameseries <- function(dupmap, series) {
  for (s in names(dupmap)) {
    for (p in dupmap[[s]]) {
      newname <- paste(s, ".", p, sep = "")
      series[[p]][series[[p]] == s] <- newname
    }
  }
  return(series)
}

duplicatedata <- function(dupmap, data) {
  for (s in names(dupmap)) {
    for (p in dupmap[[s]]) {
      newname <- paste(s, ".", p, sep = "")
      newdata <- data[, s]
      newcolnames <- c(colnames(data), newname)
      data <- stats::ts.union(data, newdata)
      colnames(data) <- newcolnames
    }
  }
  return(data)
}

duplicatebars <- function(dupmap, bars) {
  for (s in names(dupmap)) {
    if (s %in% bars) {
      for (p in dupmap[[s]]) {
        newname <- paste(s, ".", p, sep = "")
        bars <- c(bars, newname)
      }
      bars <- bars[bars != s]
    }
  }
  return(bars)
}

duplicateattribute <- function(dupmap, attribute) {
  if (is.list(attribute)) {
    for (s in names(dupmap)) {
      for (p in dupmap[[s]]) {
        newname <- paste(s, ".", p, sep = "")
        attribute[[newname]] <- attribute[[s]]
      }
      attribute[[s]] <- NULL
    }
  }
  return(attribute)
}

handleduplicates <- function(data, series, bars, col, pch, lty, lwd, barcol) {
  dups <- finddups(series)
  dupmap <- findduppanels(dups, series)
  series <- renameseries(dupmap, series)
  data <- duplicatedata(dupmap, data)
  bars <- duplicatebars(dupmap, bars)
  col <- duplicateattribute(dupmap, col)
  pch <- duplicateattribute(dupmap, pch)
  lty <- duplicateattribute(dupmap, lty)
  lwd <- duplicateattribute(dupmap, lwd)
  barcol <- duplicateattribute(dupmap, barcol)

  return(list(data = data, series = series, bars = bars, col = col, pch = pch, lty = lty, lwd = lwd, barcol = barcol))
}

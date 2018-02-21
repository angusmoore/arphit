## Constructors

# Annotations
agg_title <- function(text, panel = NULL) {
  if (!is.null(panel)) {
    panel <- as.character(panel)
  }
  return(list(type = "title", text = text, panel = panel))
}

agg_subtitle <- function(text, panel = NULL) {
  if (!is.null(panel)) {
    panel <- as.character(panel)
  }
  return(list(type = "subtitle", text = text, panel = panel))
}

agg_units <- function(units, panel = NULL) {
  if (!is.null(panel)) {
    panel <- as.character(panel)
  }
  return(list(type = "units", units = units, panel = panel))
}

agg_source <- function(source) {
  return(list(type = "source", source = source))
}

agg_footnote <- function(footnote) {
  return(list(type = "footnote", footnote = footnote))
}

# Series
agg_line <- function(data = NULL, aes = NULL, color = NULL, panel = "1") {
  if (is.list(data) && data$type == "aes") {
    stop("You passed aes as the first argument to agg_line rather than data. Did you forget to name the aes argument? (aes = agg_aes(...))")
  }
  return(list(type = "line", data = data, aes = aes, color = color, panel = as.character(panel)))
}

agg_bar <- function(data = NULL, aes = NULL, color = NULL, panel = "1", stacked = FALSE) {
  if (is.list(data) && data$type == "aes") {
    stop("You passed aes as the first argument to agg_bar rather than data. Did you forget to name the aes argument? (aes = agg_aes(...))")
  }
  return(list(type = "bar", data = data, aes = aes, color = color, panel = as.character(panel), stacked = stacked))
}

agg_aes <- function(x, y, group = NULL) {
  x <- as.character(deparse(substitute(x)))
  y <- as.character(deparse(substitute(y)))
  group <- as.character(deparse(substitute(group)))
  if (x == "NULL") {
    x <- NULL
  }
  if (y == "NULL") {
    y <- NULL
  }
  if (group == "NULL") {
    group <- NULL
  }
  return(list(type = "aes", x = x, y = y, group = group))
}

arphitgg <- function(data = NULL, aes = NULL, layout = "1", portrait = FALSE, dropxlabel = TRUE) {
  gg <- list(data = list(parent = data),
             aes = aes,
             x = list(),
             series = list(),
             layout = as.character(layout),
             bars = list(),
             shading = NULL,
             title = NULL,
             subtitle = NULL,
             paneltitles = list(),
             panelsubtitles = list(),
             footnotes = c(),
             sources = c(),
             scaleunits = NULL,
             labels = NULL,
             arrows = NULL,
             bgshading = NULL,
             lines = NULL,
             col = list(),
             pch = NULL,
             lty = 1,
             lwd = 2,
             barcol = NA,
             xlim = NULL,
             ylim = NULL,
             portrait = portrait,
             dropxlabel = dropxlabel,
             stacked = TRUE)

  class(gg) <- "arphit.gg"
  return(gg)
}


## Combining

convert2wide <- function(data, aes) {
  if (!is.null(aes$group)) {
    return(tidyr::spread_(data, key = aes$group, value = aes$y))
  } else {
    return(data)
  }
}

subsetdata <- function(data, x, y, group) {
  if (!is.null(group)) {
    return(dplyr::select_(data, x, y, group))
  } else {
    return(dplyr::select_(data, x, y))
  }
}

getnewcolumns <- function(old, combined) {
  allnames <- colnames(combined)
  originalnames <- colnames(old)
  return(allnames[!(allnames %in% originalnames)])
}

unrename <- function(data) {
  for (col in colnames(data)) {
    if (substr(col, nchar(col)-1, nchar(col)) == ".x") {
      colnames(data)[colnames(data) == col] <- substr(col, 1, nchar(col)-2)
    }
  }
  return(data)
}

applycolours <- function(gg, panel, newseriesnames, colour) {
  i <- 1
  for (name in newseriesnames) {
    gg$col[[panel]][[name]] <- colour[i]
    i <- i %% length(colour) + 1
  }
  return(gg)
}

addnewseries <- function(gg, new, panel) {
  # if aes is new, inherit from parent
  if (is.null(new$aes)) {
    new$aes <- gg$aes
  }

  # if data is null, inherit from parent
  if (is.null(new$data)) {
    new$data <- gg$data[["parent"]]
    if (is.null(new$data)) {
      stop("You have not supplied data for series")
    }
  }

  # Figure out the x variable
  if (!is.null(gg$x[[panel]])) {
    # Have previously set an x variable. Check is the same.
    if (gg$x[[panel]] != new$aes$x) {
      stop(paste("You cannot add a series to panel ", p, " with x variable ", new$aes$x, " because you have already added a series to that panel with x variable ", gg$x[[panel]], sep = ""))
    }
  } else {
    gg$x[[panel]] <- new$aes$x
  }

  # handle data
  newdata <- subsetdata(new$data, new$aes$x, new$aes$y, new$aes$group)
  newdata <- convert2wide(newdata, new$aes)
  if (!is.null(gg$data[[panel]])) {
    # We have already added a series for this panel, so we need to merge the new data on to the old
    mergeddata <- dplyr::full_join(gg$data[[panel]], newdata, by = gg$x[[panel]])
    mergeddata <- unrename(mergeddata)
    newseriesnames <- getnewcolumns(gg$data[[panel]], mergeddata)
    gg$data[[panel]] <- mergeddata
  } else {
    gg$data[[panel]] <- newdata
    newseriesnames <- colnames(newdata)[colnames(newdata) != gg$x[[panel]]]
  }

  if (!is.null(new$color)) {
    gg <- applycolours(gg, panel, newseriesnames, new$color)
  }

  return(list(gg = gg, newseriesnames = newseriesnames))
}

addlineseries <- function(gg, newline) {
  panel <- newline$panel
  out <- addnewseries(gg, newline, panel)
  return(out$gg)
}

addbarseries <- function(gg, newbar) {
  panel <- newbar$panel
  out <- addnewseries(gg, newbar, panel)
  gg <- out$gg
  newbarnames <- out$newseriesnames
  print(newbarnames)

  if (!is.null(gg$bars[[panel]])) {
    gg$bars[[panel]] <- append(gg$bars, newbarnames)
  } else {
    gg$bars[[panel]] <- newbarnames
  }

  if (!is.null(newbar$stacked)) {
    gg$stacked <- newbar$stacked
  }

  return(gg)
}

addtitle <- function(gg, title) {
  if (is.null(title$panel)) {
    gg$title <- title$text
  } else {
    gg$paneltitles[[title$panel]] <- title$text
  }
  return(gg)
}

addsubtitle <- function(gg, subtitle) {
  if (is.null(subtitle$panel)) {
    gg$subtitle <- subtitle$text
  } else {
    gg$panelsubtitles[[subtitle$panel]] <- subtitle$text
  }
  return(gg)
}

addunits <- function(gg, units) {
  if (is.null(units$panel)) {
    gg$scaleunits <- units$units
  } else {
    if (is.null(gg$scaleunits)) {
      gg$scaleunits <- list()
    }
    gg$scaleunits[[units$panel]] <- units$units
  }
  return(gg)
}

addsource <- function(gg, source) {
  gg$sources <- append(gg$sources, source$source)
  return(gg)
}

addfootnote <- function(gg, footnote) {
  gg$footnotes <- append(gg$footnotes, footnote)
  return(gg)
}

## Wrap up

arphit.ggdraw <- function(gg, filename = NULL) {
  # Here we call the arphit drawing function
  gg$data[["parent"]] <- NULL
  arphit(data = gg$data,
          x = gg$x,
          layout = gg$layout,
          bars = gg$bars,
          shading = NULL,
          title = gg$title,
          subtitle = gg$subtitle,
          paneltitles = gg$paneltitles,
          panelsubtitles = gg$panelsubtitles,
          footnotes = gg$footnotes,
          sources = gg$sources,
          scaleunits = gg$scaleunits,
          labels = NULL,
          arrows = NULL,
          bgshading = NULL,
          lines = NULL,
          col = gg$col,
          pch = gg$pch,
          lty = gg$lty,
          lwd = gg$lwd,
          barcol = NA,
          xlim = gg$xlim,
          ylim = gg$ylim,
          portrait = gg$portrait,
          dropxlabel = gg$dropxlabel,
          bar.stacked = gg$stacked,
          filename = filename)
}

print.arphit.gg <- function(gg) {
  arphit.ggdraw(gg)
}


"+.arphit.gg" <- function(gg, element) {
  gg = switch(element$type,
         "line" = addlineseries(gg, element),
         "bar" = addbarseries(gg, element),
         "title" = addtitle(gg, element),
         "subtitle" = addsubtitle(gg, element),
         "units" = addunits(gg, element),
         "source" = addsource(gg, element),
         "footnote" = addfootnote(gg, element),
         stop("Unknown element type for arphit.gg"))
  return(gg)
}

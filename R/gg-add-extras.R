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
    gg$yunits <- units$units
  } else {
    if (is.null(gg$yunits)) {
      gg$yunits <- list()
    }
    gg$yunits[[units$panel]] <- units$units
  }
  return(gg)
}

addxunits <- function(gg, units) {
  if (is.null(units$panel)) {
    gg$xunits <- units$units
  } else {
    if (is.null(gg$xunits)) {
      gg$xunits <- list()
    }
    gg$xunits[[units$panel]] <- units$units
  }
  return(gg)
}

addsource <- function(gg, source) {
  gg$sources <- append(gg$sources, source$source)
  return(gg)
}

addfootnote <- function(gg, footnote) {
  gg$footnotes <- append(gg$footnotes, footnote$footnote)
  return(gg)
}

addannotation <- function(gg, annotation, element) {
  type <- annotation$type
  annotation$type <- NULL
  gg[[element]] <- append(gg[[element]], list(annotation))
  return(gg)
}

addshading <- function(gg, shading) {
  shading$type <- NULL
  if (is.null(shading$panel)) {
    shading$panel <- NULL
  }
  if (is.null(shading$color)) {
    shading$color <- NULL
  }
  gg$shading <- append(gg$shading, list(shading))
  return(gg)
}

addylim <- function(gg, ylim) {
  ylim$type <- NULL
  if (is.null(ylim$panel)) {
    ylim$panel <- NULL
    gg$ylim <- ylim
  } else {
    p <- ylim$panel
    ylim$panel <- NULL
    gg$ylim[[p]] <- ylim
  }
  return(gg)
}

addxlim <- function(gg, xlim) {
  xlim$type <- NULL
  if (is.null(xlim$panel)) {
    xlim$panel <- NULL
    gg$xlim <- c(xlim$min, xlim$max)
  } else {
    p <- xlim$panel
    xlim$panel <- NULL
    gg$xlim[[p]] <- c(xlim$min, xlim$max)
  }
  return(gg)
}

addaxislabel <- function(gg, axislabel, axis) {
  index <- paste0(axis, "axislabels")
  if (is.null(axislabel$panel)) {
    gg[[index]] <- axislabel$axislabel
  } else {
    gg[[index]][[axislabel$panel]] <- axislabel$axislabel
  }
  return(gg)
}

addlegend <- function(gg, legend) {
  if (!is.null(legend$ncol)) {
    gg$legend.ncol <- legend$ncol
  }
  gg$legend <- TRUE
  return(gg)
}

enableautolabel <- function(gg) {
  gg$enable_autolabeller <-  TRUE
  return(gg)
}

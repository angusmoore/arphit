addtitle <- function(gg, title) {
  if (is.null(title$panel)) {
    gg$title <- title$text
  } else {
    for (p in title$panel) {
      gg$paneltitles[[p]] <- title$text
    }
  }

  gg
}

addsubtitle <- function(gg, subtitle) {
  if (is.null(subtitle$panel)) {
    gg$subtitle <- subtitle$text
  } else {
    for (p in subtitle$panel) {
      gg$panelsubtitles[[p]] <- subtitle$text
    }
  }

  gg
}

addunits <- function(gg, units) {
  if (is.null(units$panel)) {
    gg$yunits <- units$units
  } else {
    if (is.null(gg$yunits)) {
      gg$yunits <- list()
    }
    for (p in units$panel) {
      gg$yunits[[p]] <- units$units
    }
  }

  gg
}

addxunits <- function(gg, units) {
  if (is.null(units$panel)) {
    gg$xunits <- units$units
  } else {
    if (is.null(gg$xunits)) {
      gg$xunits <- list()
    }
    for (p in units$panel) {
      gg$xunits[[p]] <- units$units
    }
  }

  gg
}

addsource <- function(gg, source) {
  gg$sources <- append(gg$sources, source$source)

  gg
}

addfootnote <- function(gg, footnote) {
  gg$footnotes <- append(gg$footnotes, footnote$footnote)

  gg
}

addannotation <- function(gg, annotation, element) {
  annotation$type <- NULL
  for (p in annotation$panel) {
    tmp <- annotation
    tmp$panel <- p
    gg[[element]] <- append(gg[[element]], list(tmp))
  }

  gg
}

addshading <- function(gg, shading) {
  shading$type <- NULL
  if (is.null(shading$panel)) {
    gg$shading <- append(gg$shading, list(shading))
  } else {
    for (p in shading$panel) {
      tmp <- shading
      tmp$panel <- p
      gg$shading <- append(gg$shading, list(tmp))
    }
  }

  gg
}

addylim <- function(gg, ylim) {
  ylim$type <- NULL
  if (is.null(ylim$panel)) {
    ylim$panel <- NULL
    gg$ylim <- ylim
  } else {
    for (p in ylim$panel) {
      tmp <- ylim
      tmp$panel <- NULL
      gg$ylim[[p]] <- tmp
    }
  }

  gg
}

addxlim <- function(gg, xlim) {
  xlim$type <- NULL
  if (is.null(xlim$panel)) {
    xlim$panel <- NULL
    gg$xlim <- c(xlim$min, xlim$max)
  } else {
    for (p in xlim$panel) {
      gg$xlim[[p]] <- c(xlim$min, xlim$max)
    }
  }

  gg
}

addxfreq <- function(gg, freq, panel) {
  if (!is.null(panel)) {
    gg$xfreq[[panel]] <- freq
  } else {
    for (p in as.character(1:8)) {
      gg$xfreq[[p]] <- freq
    }
  }

  gg
}

addaxislabel <- function(gg, axislabel, axis) {
  index <- paste0(axis, "axislabels")
  if (is.null(axislabel$panel)) {
    gg[[index]] <- axislabel$axislabel
  } else {
    for (p in axislabel$panel) {
      gg[[index]][[p]] <- axislabel$axislabel
    }
  }

  gg
}

addlegend <- function(gg, legend) {
  if (!is.null(legend$ncol)) {
    gg$legend_ncol <- legend$ncol
  }
  gg$legend_onpanel <- legend$onpanel
  if (legend$onpanel) {
    gg$legend_x <- legend$x
    gg$legend_y <- legend$y
  }
  gg$legend <- TRUE

  gg
}

enableautolabel <- function(gg,
                            quiet,
                            arrow_lines,
                            arrow_bars,
                            ignore_existing_labels) {
  gg$enable_autolabeller <-  TRUE
  gg$autolabel_quiet <- quiet
  gg$arrow_lines <- arrow_lines
  gg$arrow_bars <- arrow_bars
  gg$ignore_existing_labels <- ignore_existing_labels

  gg
}

sanity_check_rename <- function(gg, mapping, panels) {
  missing <- unname(mapping)
  for (p in panels) {
    missing <- setdiff(missing, series_names(gg$data[[p]]))
  }
  if (length(missing) > 0) {
    warning(
      paste0(
        "Unable to rename series `", paste0(missing, collapse = "`, `"),
        "` as this series does not exist in the relevant panels (",
        paste(panels, collapse = ", "), "). Did you misspell it?"
      ),
      call. = FALSE
    )
  }
}

replace_name <- function(name, mapping) {
  if (name %in% mapping) {
    names(mapping)[which(name == mapping)]
  } else {
    name
  }
}

renameseries <- function(gg, mapping, panel) {
  if (is.null(panel)) {
    panel <- names(gg$data)
    panel <- panel[panel != "parent"]
  }

  mapping <- lapply(mapping, function(x) if (is.na(x)) "<NA>" else x)

  sanity_check_rename(gg, mapping, panel)

  for (p in panel) {
    for (s in seq_along(gg$data[[p]]$series)) {
      gg$data[[p]]$series[[s]]$name <-
        replace_name(gg$data[[p]]$series[[s]]$name, mapping)
    }
  }

  gg
}

drawtitle <- function(title, subtitle) {
  if (is.null(subtitle)) {
    graphics::par(lheight = 0.8)
    graphics::title(main = title, outer = TRUE, cex.main = (28/20))
    graphics::par(lheight = 1)
  } else {
    graphics::par(lheight = 0.8)
    slines <- stringr::str_count(subtitle, "\n")
    graphics::title(main = title, outer = TRUE, line = (2+1.6*slines), cex.main = (28/20))
    graphics::mtext(subtitle, outer = TRUE, line = 0.2, side = 3)
    graphics::par(lheight = 1)
  }
}

col_legend_entry <- function(name, fill, border) {
  entry <- list(name = name,
       fill = fill,
       border = border)
  class(entry) <- "col_legend_entry"
  return(entry)
}

line_legend_entry <- function(name, col, pch, lwd, lty) {
  entry <- list(name = name,
                col = col,
                pch = pch,
                lwd = lwd,
                lty = lty)
  class(entry) <- "line_legend_entry"
  return(entry)
}

legend_entry <- function(series) {
  if (series$geomtype == "bar") {
    entry <- col_legend_entry(series$name, series$attributes$col, series$attributes$barcol)
  } else {
    entry <- line_legend_entry(series$name, series$attributes$col, series$attributes$pch,
                               series$attributes$lwd, series$attributes$lty)
  }
  return(entry)
}

getlegendentries <- function(data) {
  legend <- list()
  for (p in names(data)) {
    for (series in data[[p]]$series) {
      if (!identical(series$name, "<NA>")) {
        entry <- legend_entry(series)
        if (!(list(entry) %in% legend)) {
          legend <- append(legend, list(entry))
        }
      }
    }
  }
  return(legend)
}

extract_item <- function(thelist, item) {
  if(!is.null(thelist[[item]])) {
    return(thelist[[item]])
  } else {
    return(NA)
  }
}

rowchars <- function(row, names, ncol) {
  start <- 1 + (row-1)*ncol
  end <- min(row*ncol, length(names))
  return(sum(nchar(names[start:end])))
}

determinelegendcols <- function(data, ncol) {
  series <- getlegendentries(data)
  names <- sapply(series, FUN = extract_item, item = "name")

  if (!is.na(ncol)) {
    nrow <- ceiling(length(names) / ncol)
  } else {
    ncol <- length(names)
    nrow <- ceiling(length(names) / ncol)
    nc <- max(sapply(1:nrow, FUN = rowchars, names = names, ncol = ncol))
    while ((nc+6*ncol) > MAXLEGENDCHARS && ncol > 1) {
      ncol <- ncol - 1
      nrow <- ceiling(length(names) / ncol)
      nc <- max(sapply(1:nrow, FUN = rowchars, names = names, ncol = ncol))
    }
  }
  # special case when we have picked a number of columns that means the names
  # won't actually fill into the last column
  if (length(names)/nrow == (ncol - 1)) {
    ncol <- ncol - 1
  }
  return(list(r = nrow, c = ncol))
}

get_legend_entries <- function(data) {
  series <- getlegendentries(data)

  pch <- sapply(series, FUN = extract_item, item = "pch")
  lty <- sapply(series, FUN = extract_item, item = "lty")
  lwd <- sapply(series, FUN = extract_item, item = "lwd")
  col <- sapply(series, FUN = extract_item, item = "col")
  fill <- sapply(series, FUN = extract_item, item = "fill")
  border <- sapply(series, FUN = extract_item, item = "border")
  names <- sapply(series, FUN = extract_item, item = "name")

  return(list(pch = pch, lty = lty, lwd = lwd, col = col, fill = fill, border = border, names = names))
}

draw_outer_legend <- function(data, ncol, xtickmargin, hasaxislabel) {
  entries <- get_legend_entries(data)

  ph <- graphics::par("pin")[2]
  ylines <- xtickmargin
  if (hasaxislabel) {
    ylines <- ylines + 1.7
  }
  y <- -ylines*CSI/ph

  graphics::legend(x = 0.5, y = y,
                   ncol = ncol,
                   xjust = 0.5,
                   yjust = 1,
                   xpd = NA,
                   bty = "n",
                   plot = TRUE,
                   legend = entries$names,
                   pch = entries$pch,
                   lty = entries$lty,
                   lwd = entries$lwd,
                   col = entries$col,
                   fill = entries$fill,
                   border = entries$border,
                   cex = (18/20),
                   y.intersp = 1.4)
}

draw_onpanel_legend <- function(data, ncol, x, y) {
  entries <- get_legend_entries(data)
  graphics::legend(x = x, y = y,
                   ncol = ncol,
                   xjust = 0.5,
                   yjust = 1,
                   xpd = NA,
                   bty = "n",
                   plot = TRUE,
                   legend = entries$names,
                   pch = entries$pch,
                   lty = entries$lty,
                   lwd = entries$lwd,
                   col = entries$col,
                   fill = entries$fill,
                   border = entries$border,
                   cex = (18/20),
                   y.intersp = 1.4)
}

drawnotes <- function(footnotes, sources, notesstart) {
  graphics::par(lheight = 1)
  nf <- length(footnotes)
  cumuloffset <- notesstart
  if (nf > 0 ) {
    for (i in 1:nf) {
      nlines <- stringr::str_count(footnotes[[i]], "\n")
      replacedtext <- stringr::str_replace_all(footnotes[[i]], "\n", paste0("\n", strrep(" ", NSPACESNOTES)))
      graphics::mtext(strrep("*", i),
                      outer = TRUE,
                      side = 1,
                      adj = 0,
                      padj = 1,
                      line = (cumuloffset + 1.1*(i-1) - 1), # Minus 1 because padj = 1
                      cex = (14/20))
      graphics::mtext(paste(strrep(" ", NSPACESNOTES), replacedtext, sep = ""),
                      outer = TRUE,
                      side = 1,
                      adj = 0,
                      padj = 1,
                      line = (cumuloffset + 1.1*(i-1) - 1), # Minus 1 because padj = 1
                      cex = (14/20))
      cumuloffset <- cumuloffset + 1.1*nlines
    }
  }
  if (nchar(sources$text) > 0) {
    if (sources$plural) {
      graphics::mtext("Sources:", outer = TRUE, side = 1, adj = 0, padj = 1, line = (cumuloffset + 1.1*nf - 1), cex = (14/20))
    } else {
      graphics::mtext("Source:", outer = TRUE, side = 1, adj = 0, padj = 1, line = (cumuloffset + 1.1*nf - 1), cex = (14/20))
    }

    nlines <- stringr::str_count(sources$text, "\n")
    replacedtext <- stringr::str_replace_all(sources$text, "\n", paste("\n", strrep(" ", NSPACESSOURCES), sep = ""))
    graphics::mtext(paste0(strrep(" ", NSPACESSOURCES), replacedtext),
                    outer = TRUE,
                    side = 1,
                    adj = 0,
                    padj = 1,
                    line = (cumuloffset + 1.1*nf - 1),
                    cex = (14/20))
  }
  graphics::par(lheight = 1)
}

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

getlegendentries <- function(panels, bars, attributes) {
  legend <- list()
  for (p in names(panels)) {
    for (series in panels[[p]]) {
      if (!identical(series, "<NA>")) {
        isbar <- series %in% bars[[p]]
        if (isbar) {
          entry <- list(name = series,
                        fill = attributes[[p]]$col[[series]],
                        border = attributes[[p]]$barcol[[series]])
        } else {
          entry <- list(name = series,
                        pch = attributes[[p]]$pch[[series]],
                        lwd = attributes[[p]]$lwd[[series]],
                        lty = attributes[[p]]$lty[[series]],
                        col = attributes[[p]]$col[[series]])
        }
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

determinelegendcols <- function(panels, ncol) {
  series <- getlegendentries(panels, list(), list())
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
  return(list(r = nrow, c = ncol))
}

drawlegend <- function(panels, bars, attributes, ncol, xtickmargin, hasaxislabel) {
  series <- getlegendentries(panels, bars, attributes)

  pch <- sapply(series, FUN = extract_item, item = "pch")
  lty <- sapply(series, FUN = extract_item, item = "lty")
  lwd <- sapply(series, FUN = extract_item, item = "lwd")
  col <- sapply(series, FUN = extract_item, item = "col")
  fill <- sapply(series, FUN = extract_item, item = "fill")
  border <- sapply(series, FUN = extract_item, item = "border")
  names <- sapply(series, FUN = extract_item, item = "name")

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
                   legend = names,
                   pch = pch,
                   lty = lty,
                   lwd = lwd,
                   col = col,
                   fill = fill,
                   border = border,
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

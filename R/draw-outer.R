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

drawnotes <- function(footnotes, sources) {
  graphics::par(lheight = 0.9)
  nf <- length(footnotes)
  cumuloffset <- 0
  if (nf > 0 ) {
    for (i in 1:nf) {
      nlines <- stringr::str_count(footnotes[[i]], "\n")
      replacedtext <- stringr::str_replace_all(footnotes[[i]], "\n", paste("\n", strrep(" ", NSPACESNOTES), sep = ""))
      graphics::mtext(strrep("*", i), outer = TRUE, side = 1, adj = 0, line = 2.55 + 1.1*(i-1) + 1.1*cumuloffset, cex = (14/20))
      graphics::mtext(paste(strrep(" ", NSPACESNOTES), replacedtext, sep = ""), outer = TRUE, side = 1, adj = 0, line = 2.5 + 1.1*(i-1) + 1.1*nlines + 1.1*cumuloffset, cex = (14/20))
      cumuloffset <- cumuloffset + nlines
    }
  }
  if (nchar(sources$text) > 0) {
    if (sources$plural) {
      graphics::mtext("Sources:", outer = TRUE, side = 1, adj = 0, line = 2.7 + 1.1*cumuloffset + 1.1*nf, cex = (14/20))
    } else {
      graphics::mtext("Source:", outer = TRUE, side = 1, adj = 0, line = 2.7 + 1.1*cumuloffset + 1.1*nf, cex = (14/20))
    }

    nlines <- stringr::str_count(sources$text, "\n")
    replacedtext <- stringr::str_replace_all(sources$text, "\n", paste("\n", strrep(" ", NSPACESSOURCES), sep = ""))
    graphics::mtext(paste(strrep(" ", NSPACESSOURCES), replacedtext, sep = ""), outer = TRUE, side = 1, adj = 0, line = 2.7 + 1.1*nlines + 1.1*cumuloffset + 1.1*nf, cex = (14/20))
  }
  graphics::par(lheight = 1)
}

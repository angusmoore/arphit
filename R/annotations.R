
sanitycheck.specificline <- function(line, i) {
  if (is.null(line$x1)) {
    stop(paste("Line ", i, " was specified without x or y (i.e. not a horizontal or vertical line), but is missing x1.", sep = ""))
  }
  if (is.null(line$y1)) {
    stop(paste("Line ", i, " was specified without x or y (i.e. not a horizontal or vertical line), but is missing y1.", sep = ""))
  }
  if (is.null(line$x2)) {
    stop(paste("Line ", i, " was specified without x or y (i.e. not a horizontal or vertical line), but is missing x2.", sep = ""))
  }
  if (is.null(line$y2)) {
    stop(paste("Line ", i, " was specified without x or y (i.e. not a horizontal or vertical line), but is missing y2.", sep = ""))
  }
  return(line)
}

sanitycheck.vhline <- function(line, i) {
  if (!is.null(line$x)) {
    line$x1 <- line$x
    line$x2 <- line$x
    line$y1 <- NA
    line$y2 <- NA
  }
  if (!is.null(line$y)) {
    line$x1 <- NA
    line$x2 <- NA
    line$y1 <- line$y
    line$y2 <- line$y
  }
  return(line)
}

sanitychecklines <- function(linelist) {
  if (!is.null(linelist) && length(linelist) > 0) {
    for (i in 1:length(linelist)) {
      line <- linelist[[i]]
      # can't use null checking, because x1 partial matches x
      if ("x" %in% names(line) || ("y" %in% names(line))) {
        line <- sanitycheck.vhline(line, i)
      } else {
        line <- sanitycheck.specificline(line, i)
      }
      if (is.null(line$panel)) {
        stop(paste("Line ", i, " is missing panel identifier.", sep = ""))
      }
      if (is.null(line$color)) {
        line$color <- "black"
      }
      if (is.null(line$lty)) {
        line$lty <- 1
      }

      linelist[[i]] <- line
    }
  }
  return(linelist)
}

drawbgshading <- function(shading) {
  # if any are NA, give them the ends of the plot
  if (is.na(shading$x1)) {
    shading$x1 <- graphics::par("usr")[1]
  }
  if (is.na(shading$x2)) {
    shading$x2 <- graphics::par("usr")[2]
  }
  if (is.na(shading$y1)) {
    shading$y1 <- graphics::par("usr")[3]
  }
  if (is.na(shading$y2)) {
    shading$y2 <- graphics::par("usr")[4]
  }
  graphics::rect(shading$x1, shading$y1, shading$x2, shading$y2, col = shading$color, lty = 0)
}

drawbgshadings <- function(shadings, p) {
  for (shading in shadings) {
    if (shading$panel == p) {
      drawbgshading(shading)
    }
  }
}

drawannotationline <- function(line) {
  # if any are NA, give them the ends of the plot
  if (is.na(line$x1)) {
    line$x1 <- graphics::par("usr")[1]
  }
  if (is.na(line$x2)) {
    line$x2 <- graphics::par("usr")[2]
  }
  if (is.na(line$y1)) {
    line$y1 <- graphics::par("usr")[3]
  }
  if (is.na(line$y2)) {
    line$y2 <- graphics::par("usr")[4]
  }
  graphics::segments(line$x1, line$y1, line$x2, line$y2, col = line$color, lty = line$lty)
}

drawannotationlines <- function(lines, p) {
  for (line in lines) {
    if (line$panel == p) {
      drawannotationline(line)
    }
  }
}

drawlabel <- function(label) {
  graphics::text(label$text, x = label$x, y = label$y, adj = c(0.5, 0.5), col = label$color)
}

drawlabels <- function(labels, p) {
  for (label in labels) {
    if (label$panel == p) {
      drawlabel(label)
    }
  }
}

drawarrow <- function(arrow) {
  graphics::arrows(arrow$tail.x, arrow$tail.y, arrow$head.x, arrow$head.y, col = arrow$color, lwd = arrow$lwd, length = 0.1)
}

drawarrows <- function(arrows, p) {
  for (arrow in arrows) {
    if (arrow$panel == p) {
      drawarrow(arrow)
    }
  }
}

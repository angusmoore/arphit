testscaleoptions <- function(significand, minval, maxval, permittedsteps) {
  # Now we try the different combinations of and see which works best
  jointdeviation <- matrix(NA, length(PERMITTEDLABELS), length(permittedsteps))

  for (i in 1:length(PERMITTEDLABELS)) {
    step <- PERMITTEDLABELS[i]*10^significand
    minscale <- plyr::round_any(minval, step, f = floor)

    # This bit could be better. There's surely an algebraic way I can determine the best option here
    for (j in 1:length(permittedsteps)) {
      maxscale <- ((permittedsteps[j]-1)*step + minscale)
      if (maxscale > maxval) {
        jointdeviation[i,j] <- (minval - minscale) + (maxscale - maxval)
      } else {
        jointdeviation[i,j] <- NA
      }
    }
  }
  return(jointdeviation)
}

defaultscale <- function(data,permittedsteps=PERMITTEDSTEPS) {
  maxval <- max(data,na.rm=TRUE)
  minval <- min(data,na.rm=TRUE)
  span <- maxval-minval

  significand <- floor(log10(span/min(permittedsteps)))
  if (is.na(significand) || significand == Inf || significand == -Inf) {
    # Happens if span is zero, possibly other cases
    significand <- 0
  }

  jointdeviation <- testscaleoptions(significand, minval, maxval, permittedsteps)
  # Check here if none are feasible
  if (all(is.na(jointdeviation))) {
    # Try a larger significand
    significand <- significand + 1
    jointdeviation <- testscaleoptions(significand, minval, maxval, permittedsteps)
    if (all(is.na(jointdeviation))) {
      message("No feasible scale found, defaulting to something bad.")
      return(list("min" = minval, "max" = maxval, "nsteps" = 5))
    }
  }

  ideal <- which(jointdeviation == min(jointdeviation,na.rm=TRUE), arr.ind=TRUE)
  if (!is.null(nrow(ideal))) {
    ideal <- ideal[1, ] # Just take the first match
  }
  step <- PERMITTEDLABELS[ideal[1]]*10^significand
  nsteps <- permittedsteps[ideal[2]]
  minscale <- plyr::round_any(minval, step, f = floor)
  maxscale <- minscale + (nsteps-1)*step
  return(list("min" = minscale, "max" = maxscale, "nsteps" = nsteps))
}

createscale <- function(minscale,maxscale,nsteps) {
  scale <- rep(NA,nsteps)
  stepsize <- (maxscale-minscale)/(nsteps-1)
  for (i in 1:nsteps) {
    scale[i] <- minscale + (i-1)*stepsize
  }
  return(scale)
}

duplicateaxes <- function(toduplicate, panels, layout) {
  if (is.null(panels[["1"]]) && !is.null(panels[["2"]])) {
    toduplicate[["1"]] <- toduplicate[["2"]]
  }
  if (is.null(panels[["2"]]) && !is.null(panels[["1"]])) {
    toduplicate[["2"]] <- toduplicate[["1"]]
  }
  if (layout == "2b2" || layout == "2h" || layout == "3h" || layout == "3b2" || layout == "4h" || layout == "4b2") {
    if (is.null(panels[["3"]]) && !is.null(panels[["4"]])) {
      toduplicate[["3"]] <- toduplicate[["4"]]
    }
    if (is.null(panels[["4"]]) && !is.null(panels[["3"]])) {
      toduplicate[["4"]] <- toduplicate[["3"]]
    }
  }
  if (layout == "3h" || layout == "3b2" || layout == "4h" || layout == "4b2") {
    if (is.null(panels[["5"]]) && !is.null(panels[["6"]])) {
      toduplicate[["5"]] <- toduplicate[["6"]]
    }
    if (is.null(panels[["6"]]) && !is.null(panels[["5"]])) {
      toduplicate[["6"]] <- toduplicate[["5"]]
    }
  }
  if (layout == "4h" || layout == "4b2") {
    if (is.null(panels[["7"]]) && !is.null(panels[["8"]])) {
      toduplicate[["7"]] <- toduplicate[["8"]]
    }
    if (is.null(panels[["8"]]) && !is.null(panels[["7"]])) {
      toduplicate[["8"]] <- toduplicate[["7"]]
    }
  }
  if (layout == "3v") {
    if (is.null(panels[["2"]]) && !is.null(panels[["1"]])) {
      toduplicate[["2"]] <- toduplicate[["1"]]
    }
    if (is.null(panels[["2"]]) && !is.null(panels[["3"]])) {
      toduplicate[["2"]] <- toduplicate[["3"]]
    }
  }

  return(toduplicate)
}

conformscale <- function(panels, scaleunits) {
  if (is.null(scaleunits)) {
    scaleunits <- DEFAULTSCALEUNITS
  }

  out <- list()
  if(!is.list(scaleunits)) {
    for (p in names(panels)) {
      out[[p]] <- scaleunits
    }
  } else {
    for (p in names(panels)) {
      if (p %in% names(scaleunits)) {
        out[[p]] <- scaleunits[[p]]
      } else {
        out[[p]] <- DEFAULTSCALEUNITS
      }
    }
  }
  return(out)
}

handleunits <- function(panels, scaleunits, layout) {
  out <- conformscale(panels, scaleunits)
  out <- duplicateaxes(out, panels, layout)
  return(out)
}

handlexunits <- function(panels, xunits) {
  conformscale(panels, xunits)
}

ylimconform <- function(panels, ylim, data, layout) {
  ylim_list <- list()
  if (is.null(ylim)) {
    for (p in names(panels)) {
      paneldf <- data[[p]][, panels[[p]], drop = FALSE]
      if (!is.null(paneldf) && any(!is.na(paneldf)) && is.finite(max(paneldf, na.rm = TRUE)) && is.finite(min(paneldf, na.rm = TRUE))) {
        ylim_list[[p]] <- defaultscale(paneldf)
      } else {
        ylim_list[[p]] <- EMPTYSCALE
      }
    }
  } else {
    if ("min" %in% names(ylim) || "max" %in% names(ylim) || "nsteps" %in% names(ylim)) {
      # have supplied a single list to apply to all
      if (is.null(ylim$nsteps) || ylim$nsteps < 2) {
        stop("You must supply nsteps > 2 for the y limit.")
      }
      if (is.null(ylim$min)) {
        stop("You did not supply a min ylimit.")
      }
      if (is.null(ylim$max)) {
        stop("You did not supply a max ylimit.")
      }
      for (p in names(panels)) {
        ylim_list[[p]] <- ylim
      }
    } else {
      # have supplied lims for each
      for (p in names(panels)) {
        if (p %in% names(ylim)) {
          ylim_list[[p]] <- ylim[[p]]
          if (is.null(ylim[[p]]) || ylim[[p]]$nsteps < 2) {
            stop(paste("The y-limit you supplied for panel ", p, " has fewer than 2 points (or you forgot to supply nsteps).", sep = ""))
          }
          if (is.null(ylim[[p]]$max)) {
            stop(paste("You did not supply a max ylimit for panel ", p, ".", step = ""))
          }
          if (is.null(ylim[[p]]$min)) {
            stop(paste("You did not supply a max ylimit for panel ", p, ".", step = ""))
          }
        } else {
          paneldf <- data[[p]][, panels[[p]], drop = FALSE]
          if (!is.null(paneldf) && ncol(paneldf) > 0) {
            ylim_list[[p]] <- defaultscale(paneldf)
          } else {
            ylim_list[[p]] <- EMPTYSCALE
          }
        }
      }
    }
  }
  ylim_list <- duplicateaxes(ylim_list, panels, layout)
  return(ylim_list)
}

handleticks <- function(data, panels, ylim) {
  ticks <- list()
  for (p in names(panels)) {
    if (!is.null(ylim[[p]])) {
      ticks[[p]] <- createscale(ylim[[p]]$min, ylim[[p]]$max, ylim[[p]]$nsteps)
    }
  }
  return(ticks)
}

findlabelstep <- function(start, end, layout_factor) {
  for (i in c(1, 2, 3, 4, 5, 10, 15, 20, 25, 30, 40, 50)) {
    if ((floor((end - start) / i) + 1) < round(MAXXLABELS*layout_factor)) {
      return(i)
    }
  }
}

restrictlabels <- function(ticks, layout_factor) {
  step <- findlabelstep(1, length(ticks), layout_factor)
  n <- floor((length(ticks) - 1) / step) + 1
  return(seq(to = length(ticks), length.out = n, by = step))
}

getlayoutfactor <- function(layout) {
  if (layout == "2v" || layout == "2b2" || layout == "3b2" || layout == "4b2") {
    return(1/2)
  } else if (layout == "3v") {
    return(1/3)
  } else {
    return(1)
  }
}

xlabels.ts <- function(xlim, layout) {
  layout_factor <- getlayoutfactor(layout)
  startyear <- xlim[1]
  endyear <- xlim[2]
  # Create the sequence and offset the labels by half a year so that the labels are centered
  ticks <- seq(from = startyear, to = (endyear-1), by = 1)
  keep <- restrictlabels(ticks, layout_factor)
  labels <- ticks[keep]
  at <- labels + 0.5
  return(list("at" = at, "labels" = labels, "ticks" = ticks))
}

xlabels.categorical <- function(xlim, xvar, layout, showall) {
  start <- xlim[1]
  end <- xlim[2] - 1
  at <- seq(from = start, to = end, by = 1) + 0.5
  labels <- xvar
  if (is.null(showall) || !showall) {
    layout_factor <- getlayoutfactor(layout)
    keep <- restrictlabels(labels, layout_factor)
    at <- at[keep]
    labels <- labels[keep]
  }
  return(list("at" = at, "labels" = labels, "ticks" = seq(from = start, to = end, by = 1)))
}

xlabels.numericcategorical <- function(xlim, xvar, layout, showall) {
  step <- min(diff(xvar))
  start <- xlim[1]
  end <- xlim[2] - 1
  at <- seq(from = start, to = end, by = step) + 0.5*step
  labels <- xvar
  if (!is.null(showall) && !showall) {
    layout_factor <- getlayoutfactor(layout)
    keep <- restrictlabels(labels, layout_factor)
    at <- at[keep]
    labels <- labels[keep]
  }
  return(list("at" = at, "labels" = labels, "ticks" = seq(from = start, to = end, by = step)))
}

xlabels.scatter <- function(xlim, xvalues) {
  scale <- defaultscale(xvalues)
  scale <- createscale(scale$min,scale$max,scale$nsteps)
  return(list(at = scale, labels = scale, ticks = scale))
}

xlabels <- function(xlim, xvar, data, ists, layout, showall) {
  if (stats::is.ts(data) || !is.null(ists)) {
    return(xlabels.ts(xlim, layout))
  } else if (is.scatter(xvar)) {
    return(xlabels.scatter(xlim, xvar))
  } else if (!is.null(xvar)) {
    if (is.numeric(xvar)) {
      return(xlabels.numericcategorical(xlim, xvar, layout, showall))
    } else {
      return(xlabels.categorical(xlim, xvar, layout, showall))
    }
  } else {
    # Empty plot, if we're using the empty scale, that's a TS
    if (identical(xlim, EMPTYXSCALE)) {
      return(xlabels.ts(xlim, layout))
    } else {
      xlabels.scatter(xlim, xlim) # Seems least worse choice?
    }
  }
}

handlexlabels <- function(panels, xlim, xvars, data, layout, showall) {
  out <- list()
  for (p in names(panels)) {
    if (!is.null(data[[p]])) {
      ists <- xvars[[paste(p,"ts",sep="")]]
      out[[p]] <- xlabels(xlim[[p]], xvars[[p]], data[[p]], ists, layout, showall)
    } else {
      ists <- xvars[["1ts"]]
      out[[p]] <- xlabels(xlim[[p]], xvars[[p]], data[[p]], ists, layout, showall)
    }
  }
  return(out)
}

warnifxdiff <- function(panels, xlim) {
  for (p in names(xlim)) {
    for (q in names(xlim)) {
      if (all(xlim[[p]] != xlim[[q]])) {
        warning(paste("Panels ", p, " and ", q, " have differing x limits. This may lead to confusing graphs. Be careful!", sep = ""))
      }
    }
  }
}

is.scatter <- function(x) {
  if (!is.null(x) && is.numeric(x)) {
    if (any(is.na(x))) {
      return(TRUE) # Assume is a scatter if NA in x value
    } else {
      return(!all(diff(x) == max(diff(x))))
    }
  } else {
    return(FALSE)
  }
}

defaultxscale <- function(xvars, xscales, data, ists) {
  if (!is.null(xvars)) {
    if (is.numeric(xvars) && (stats::is.ts(data) || ists)) {
      return( c(floor(min(xvars, na.rm = TRUE)), ceiling(max(xvars, na.rm = TRUE))) )
    } else if (is.scatter(xvars)) {
      scale <- defaultscale(xvars)
      return(c(scale$min,scale$max))
    } else {
      # Handle numerical categories
      if (is.numeric(xvars)) {
        return(c(xvars[1], xvars[length(xvars)]+min(diff(xvars))))
      } else {
        return (c(1, length(xvars)+1))
      }
    }
  } else if (!is.null(xscales[["1"]])) {
    return(xscales[["1"]])
  } else {
    return(EMPTYXSCALE)
  }
}

xlimconform <- function(panels, xlim, xvars, data) {
  out <- list()
  if (!is.list(xlim)) {
    for (p in names(panels)) {
      if (is.null(xlim)) {
        ists <- !is.null(xvars[[paste0(p, "ts")]])
        out[[p]] <- defaultxscale(xvars[[p]], out, data[[p]], ists)
      } else {
        out[[p]] <- xlim
      }
    }
  } else {
    for (p in names(panels)) {
      if (p %in% names(xlim)) {
        out[[p]] <- xlim[[p]]
      } else {
        ists <- !is.null(xvars[[paste(p,"ts",sep="")]])
        out[[p]] <- defaultxscale(xvars[[p]], out, data[[p]], ists)
      }
    }
  }
  # have a check for non-matching xlimits
  warnifxdiff(panels, out)
  return(out)
}

handleaxislabels <- function(labels, panels) {
  if (is.list(labels)) {
    return(labels)
  } else {
    out <- list()
    for (p in names(panels)) {
      out[[p]] <- labels
    }
    return(out)
  }
}

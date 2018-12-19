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

get_data_max_min <- function(data, bars, stacked) {
  series_data <- data[!colnames(data) %in% bars]
  bardata <- data[colnames(data) %in% bars]
  if (length(bardata) > 0) {
    bardata[is.na(bardata)] <- 0
    bardata_p <- bardata
    bardata_n <- bardata
    bardata_p[bardata < 0] <- 0
    bardata_n[bardata >= 0] <- 0
  }

  if (ncol(series_data) > 0 && ncol(bardata) > 0 && stacked) {
    maxval <- max(max(series_data,na.rm=TRUE),max(apply(bardata_p, 1, sum),na.rm=TRUE))
    minval <- min(min(series_data,na.rm=TRUE),min(apply(bardata_n, 1, sum),na.rm=TRUE))
  } else if (ncol(series_data) > 0 || !stacked) {
    maxval <- max(data,na.rm=TRUE)
    minval <- min(data,na.rm=TRUE)
    if (ncol(bardata) > 0) {
      # Include zero if we have bars
      minval <- min(0, minval)
      maxval <- max(0, maxval)
    }
  } else {
    maxval <- max(apply(bardata_p, 1, sum),na.rm=TRUE)
    minval <- min(apply(bardata_n, 1, sum),na.rm=TRUE)
  }
  return(list(maxval=maxval,minval=minval))
}

defaultscale <- function(data,bars,stacked,permittedsteps=PERMITTEDSTEPS) {
  # Split bars and series apart
  out <- get_data_max_min(data,bars,stacked)
  maxval <- out$maxval
  minval <- out$minval
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
  stepsize <- (maxscale-minscale)/(nsteps-1)
  # Get the significand of the minscale
  significand <- -floor(log10(abs(minscale)))
  scale <- round(seq(from=minscale, by=stepsize, length.out=nsteps), significand+4) # Assume the rounding to the level of significance on the minscale + 4 will be _more_ than enough to wipe out small innaccuracies
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

apply_ylim_to_panels <- function(ylim) {
  if (!is.list(ylim)) stop("Ylim should be a list")
  if ("min" %in% names(ylim) || "max" %in% names(ylim) || "nsteps" %in% names(ylim)) {
    ylim_list <- list()
    # have supplied a single list to apply to all
    sanity_check_ylim(ylim)
    for (p in as.character(1:8)) {
      ylim_list[[p]] <- ylim
    }
  } else {
    lapply(ylim, sanity_check_ylim)
    return(ylim)
  }
  return(ylim_list)
}

ylimconform <- function(panels, ylim, data, bars, layout, stacked, xvals, xlim) {
  ylim <- apply_ylim_to_panels(ylim)
  for (p in names(panels)) {
    if (!p %in% names(ylim)) {
      # create a default scale for this panel
      paneldf <- data[[p]][, panels[[p]], drop = FALSE]
      if (!is.null(paneldf) && ncol(paneldf) > 0) {
        x <- getxvals(data[[p]], !is.null(xvals[[paste0(p, "ts")]]), xvals[[p]])
        x_restriction <- x >= xlim[[p]][1] & x <= xlim[[p]][2]
        if (!any(x_restriction)) x_restriction <- TRUE # if no visible x, use all the data as a fallback
        ylim[[p]] <- defaultscale(paneldf[x_restriction, , drop = FALSE], bars[[p]], stacked)
      } else {
        ylim[[p]] <- EMPTYSCALE
      }
    }
  }
  ylim <- duplicateaxes(ylim, panels, layout)
  return(ylim)
}

handleticks <- function(data, ylim) {
  ticks <- list()
  for (p in names(ylim)) {
    if (!is.null(ylim[[p]])) {
      ticks[[p]] <- createscale(ylim[[p]]$min, ylim[[p]]$max, ylim[[p]]$nsteps)
    }
  }
  return(ticks)
}

findlabelstep <- function(start, end, layout_factor, exponent = 1) {
  for (i in c(1, 2, 3, 4, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90)) {
    if ((floor((end - start) / (i*exponent)) + 1) < round(MAXXLABELS*layout_factor)) {
      return(i*exponent)
    }
  }
  # haven't found one, increase exponent (with recursion to keep increasing it)
  return(findlabelstep(start, end, layout_factor, exponent*10))
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
  start <- 1
  end <- length(xvar)
  at <- seq(from = start, to = end, by = 1) + 0.5
  labels <- xvar
  if (!is.null(showall) && !showall) {
    layout_factor <- getlayoutfactor(layout)
    keep <- restrictlabels(labels, layout_factor)
    at <- at[keep]
    labels <- labels[keep]
  }
  return(list("at" = at, "labels" = labels, "ticks" = seq(from = start, to = end, by = 1)))
}

xlabels.numericcategorical <- function(xlim, xvar, layout, showall) {
  if (length(xvar) > 1) {
    step <- min(diff(xvar))
  } else {
    step <- 1
  }
  start <- xlim[1]
  end <- xlim[2] - step
  at <- seq(from = start, to = end, by = step) + 0.5*step
  labels <- seq(from = start, to = end, by = step)
  if (is.null(showall) || !showall) {
    layout_factor <- getlayoutfactor(layout)
    keep <- restrictlabels(labels, layout_factor)
    at <- at[keep]
    labels <- labels[keep]
  }
  return(list("at" = at, "labels" = labels, "ticks" = seq(from = start, to = end, by = step)))
}

xlabels.scatter <- function(xlim, xvalues) {
  scale <- defaultscale(data.frame(x=c(xlim[1],xlim[2]-(xlim[2]-xlim[1])/10000)),NULL,FALSE)
  scale <- createscale(scale$min,scale$max,scale$nsteps)
  return(list(at = scale, labels = scale, ticks = scale))
}

xlabels <- function(xlim, xvar, data, ists, layout, showall) {
  if (!is.null(ists)) {
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
      return(length(x) != 1 && (!all(diff(x) == max(diff(x)))))
    }
  } else {
    return(FALSE)
  }
}

defaultxscale <- function(xvars, xscales, data, ists) {
  if (!is.null(xvars)) {
    if (is.numeric(xvars) && ists) {
      return( c(floor(min(xvars, na.rm = TRUE)), ceiling(max(xvars, na.rm = TRUE))) )
    } else if (is.scatter(xvars)) {
      scale <- defaultscale(data.frame(x=xvars), NULL, FALSE)
      return(c(scale$min,scale$max))
    } else {
      # Handle numerical categories
      if (is.numeric(xvars)) {
        if (length(xvars) > 1) {
          return(c(xvars[1], xvars[length(xvars)]+min(diff(xvars))))
        } else {
          return(c(xvars,xvars+1))
        }
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
  if(!is.list(xlim) && length(xlim) > 0) {
    xlim <- lapply(panels, function(x) xlim)
  }

  out <- list()
  for (p in names(panels)) {
    ists <- !is.null(xvars[[paste(p,"ts",sep="")]])
    out[[p]] <- defaultxscale(xvars[[p]], out, data[[p]], ists)
    if (p %in% names(xlim)) {
      if (is.finite(xlim[[p]][1])) out[[p]][1] <- xlim[[p]][1]
      if (is.finite(xlim[[p]][2])) out[[p]][2] <- xlim[[p]][2]
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

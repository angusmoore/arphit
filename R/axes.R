testscaleoptions <- function(significand, minval, maxval, permittedsteps) {
  # Now we try the different combinations of and see which works best
  jointdeviation <- matrix(NA, length(PERMITTEDLABELS), length(permittedsteps))

  for (i in 1:length(PERMITTEDLABELS)) {
    step <- PERMITTEDLABELS[i]*10^significand
    minscale <- floor(minval/step)*step

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

get_stacked_max_min <- function(data, xlim) {
  bardata <- data.frame(x = data$x, stringsAsFactors = FALSE)
  for (i in seq_along(data$series)) {
    s <- data$series[[i]]
    if (s$bar) {
      series_data <- data.frame(x = series_x_values(data, i), y = series_values(data, i), stringsAsFactors = FALSE)
      names(series_data) <- c("x", i)
      bardata <- dplyr::left_join(bardata, series_data, by = "x")
    }
  }
  x <- get_x_plot_locations(bardata$x, data)
  x_restriction <- x >= xlim[1] & x <= xlim[2]
  bardata <- bardata[names(bardata) != "x"]
  bardata <- bardata[x_restriction,,drop = FALSE]
  if (ncol(bardata) > 0) {
    bardata[is.na(bardata)] <- 0
    bardata_p <- bardata
    bardata_n <- bardata
    bardata_p[bardata < 0] <- 0
    bardata_n[bardata >= 0] <- 0
    return(list(max = max(apply(bardata_p, 1, sum),na.rm=TRUE),
                min = min(apply(bardata_n, 1, sum),na.rm=TRUE)))
  } else {
    return(list(min = Inf, max = -Inf))
  }
}

get_series_max_min <- function(series, data, xlim) {
  x <- get_x_plot_locations(series$x, data)
  x_restriction <- !is.na(x) & x >= xlim[1] & x <= xlim[2]
  if (!any(x_restriction)) x_restriction <- TRUE # if no visible x, use all the data as a fallback
  list(max=max(series$y[x_restriction],na.rm=TRUE),
       min=min(series$y[x_restriction],na.rm=TRUE))
}

get_data_max_min <- function(data, xlim, stacked) {
  if (any(sapply(data$series, function(x) x$bar))) {
    minval <- 0 # bound by zero if we have bars, since we want them to start there
    maxval <- 0
  } else {
    minval <- Inf
    maxval <- -Inf
  }

  # Find smallest and largest series value
  for (s in data$series) {
    if (!s$bar || !stacked) {
      out <- get_series_max_min(s, data, xlim)
      minval <- min(minval, out$min, na.rm = TRUE)
      maxval <- max(maxval, out$max, na.rm = TRUE)
    }
  }

  if (stacked) {
    out <- get_stacked_max_min(data, xlim)
    minval <- min(minval, out$min, na.rm = TRUE)
    maxval <- max(maxval, out$max, na.rm = TRUE)
  }
  return(list(min = minval, max = maxval))
}

defaultscale <- function(maxval, minval, permittedsteps=PERMITTEDSTEPS) {
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
  minscale <- floor(minval/step)*step
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

duplicateaxes <- function(toduplicate, data, layout) {
  if (is_empty(data[["1"]])  && !is_empty(data[["2"]])) {
    toduplicate[["1"]] <- toduplicate[["2"]]
  }
  if (is_empty(data[["2"]]) && !is_empty(data[["1"]])) {
    toduplicate[["2"]] <- toduplicate[["1"]]
  }
  if (layout == "2b2" || layout == "2h" || layout == "3h" || layout == "3b2" || layout == "4h" || layout == "4b2") {
    if (is_empty(data[["3"]]) && !is_empty(data[["4"]])) {
      toduplicate[["3"]] <- toduplicate[["4"]]
    }
    if (is_empty(data[["4"]]) && !is_empty(data[["3"]])) {
      toduplicate[["4"]] <- toduplicate[["3"]]
    }
  }
  if (layout == "3h" || layout == "3b2" || layout == "4h" || layout == "4b2") {
    if (is_empty(data[["5"]]) && !is_empty(data[["6"]])) {
      toduplicate[["5"]] <- toduplicate[["6"]]
    }
    if (is_empty(data[["6"]]) && !is_empty(data[["5"]])) {
      toduplicate[["6"]] <- toduplicate[["5"]]
    }
  }
  if (layout == "4h" || layout == "4b2") {
    if (is_empty(data[["7"]]) && !is_empty(data[["8"]])) {
      toduplicate[["7"]] <- toduplicate[["8"]]
    }
    if (is_empty(data[["8"]]) && !is_empty(data[["7"]])) {
      toduplicate[["8"]] <- toduplicate[["7"]]
    }
  }
  if (layout == "3v") {
    if (is_empty(data[["2"]]) && !is_empty(data[["1"]])) {
      toduplicate[["2"]] <- toduplicate[["1"]]
    }
    if (is_empty(data[["2"]]) && !is_empty(data[["3"]])) {
      toduplicate[["2"]] <- toduplicate[["3"]]
    }
  }
  return(toduplicate)
}

apply_nonempty <- function(toduplicate, data, nonempty) {
  for (p in names(data)) {
    if (is_empty(data[[p]])) {
      toduplicate[[p]] <- nonempty
    }
  }
  return(toduplicate)
}

duplicateaxes_vertical <- function(toduplicate, data, layout) {
  if (layout != "2v" && layout != "3v") {
    rhs <- names(data)[seq(from = 2, to = length(data), by = 2)]
    lhs <- names(data)[seq(from = 1, to = length(data), by = 2)]
    rhs_nonempty <- names(data[rhs])[!sapply(data[rhs], is_empty)]
    lhs_nonempty <- names(data[lhs])[!sapply(data[lhs], is_empty)]
    if (length(rhs_nonempty) > 0) {
      rhs_nonempty <- rhs_nonempty[[1]]
    } else {
      rhs_nonempty <- lhs_nonempty[[1]]
    }

    if (length(lhs_nonempty) > 0) {
      lhs_nonempty <- lhs_nonempty[[1]]
    } else {
      lhs_nonempty <- rhs_nonempty[[1]]
    }

    toduplicate <- apply_nonempty(toduplicate, data[lhs], toduplicate[[lhs_nonempty]])
    toduplicate <- apply_nonempty(toduplicate, data[rhs], toduplicate[[rhs_nonempty]])
  } else {
    nonempty <- names(data)[!sapply(data, is_empty)][[1]]
    toduplicate <- apply_nonempty(toduplicate, data, toduplicate[[nonempty]])
  }
  return(toduplicate)
}

conformscale <- function(panels, scaleunits) {
  if (is.null(scaleunits)) {
    scaleunits <- DEFAULTSCALEUNITS
  }

  out <- list()
  if(!is.list(scaleunits)) {
    for (p in panels) {
      out[[p]] <- scaleunits
    }
  } else {
    for (p in panels) {
      if (p %in% names(scaleunits)) {
        out[[p]] <- scaleunits[[p]]
      } else {
        out[[p]] <- DEFAULTSCALEUNITS
      }
    }
  }
  return(out)
}

handleunits <- function(data, scaleunits, layout) {
  out <- conformscale(names(data), scaleunits)
  out <- duplicateaxes(out, data, layout)
  return(out)
}

handlexunits <- function(panels, xunits) {
  conformscale(panels, xunits)
}

apply_ylim_to_panels <- function(ylim) {
  if ("min" %in% names(ylim) || "max" %in% names(ylim) || "nsteps" %in% names(ylim)) {
    ylim_list <- list()
    # have supplied a single list to apply to all
    for (p in as.character(1:8)) {
      ylim_list[[p]] <- ylim
    }
    return(ylim_list)
  } else {
    return(ylim)
  }
}

ylimconform <- function(ylim, data, layout, stacked, xlim) {
  ylim <- apply_ylim_to_panels(ylim)
  for (p in names(data)) {
    if (!p %in% names(ylim)) {
      # create a default scale for this panel
      if (!is_empty(data[[p]])) {
        out <- get_data_max_min(data[[p]],xlim[[p]],stacked)
        ylim[[p]] <- defaultscale(out$max, out$min)
      } else {
        ylim[[p]] <- EMPTYSCALE
      }
    }
  }
  ylim <- duplicateaxes(ylim, data, layout)
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
    if (ceiling((end - start) / (i*exponent)) < round(MAXXLABELS*layout_factor)) {
      return(i*exponent)
    }
  }
  # haven't found one, increase exponent (with recursion to keep increasing it)
  return(findlabelstep(start, end, layout_factor, exponent*10))
}

restrictlabels <- function(ticks, layout_factor, partial_end_year = FALSE) {
  step <- findlabelstep(1, length(ticks), layout_factor)
  n <- floor((length(ticks) - 1) / step) + 1
  if (!partial_end_year) {
    to <- length(ticks)
  } else {
    to <- length(ticks) - 1
  }
  return(seq(to = to, length.out = n, by = step))
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

xlabels.ts_decade <- function(xlim, layout_factor) {
  startyear <- ceiling(xlim[1]/10)*10 # So that it is inside x limits
  endyear <- floor(xlim[2]/10)*10
  ticks <- seq(from = startyear, to = endyear, by = 10)
  keep <- restrictlabels(ticks, layout_factor, xlim[2] < endyear && xlim[2] - xlim[1] > 3) # Only keep every 3rd or whatever label
  labels <- ticks[keep]
  at <- labels
  # drop any labels that are outside the x limits
  keep <- at >= xlim[1] & at <= xlim[2]
  return(list(at = at[keep], labels = labels[keep], ticks = ticks))
}

xlabels.ts_year <- function(xlim, layout) {
  layout_factor <- getlayoutfactor(layout)
  startyear <- floor(xlim[1])
  endyear <- ceiling(xlim[2])
  # Create the sequence and offset the labels by half a year so that the labels are centered
  labels <- seq(from = startyear, to = (endyear-1), by = 1)
  keep <- restrictlabels(labels, layout_factor, xlim[2] < endyear && xlim[2] - xlim[1] > 3) # Only keep every 3rd or whatever label
  labels <- labels[keep]
  at <- labels + 0.5
  # drop any labels that are outside the x limits
  keep <- at > xlim[1] & at < xlim[2]
  ticks <- seq(from = startyear, to = endyear, by = 1)
  return(list(at = at[keep], labels = labels[keep], ticks = ticks))
}

xlabels.ts_quarter <- function(xlim) {
  startquarter <- floor(xlim[1]*4)/4
  endquarter <- ceiling(xlim[2]*4)/4
  ticks <- seq(from = startquarter, to = (endquarter-0.25), by = 0.25)

  # convert the labels to quarter names
  qtrs <- 1 + 4*(ticks - floor(ticks))
  labels <- substr(month.abb[qtrs*3], 1, 1)
  at <- ticks + 0.5 * 0.25

  # add years
  startyear <- floor(xlim[1])
  endyear <- floor(xlim[2])
  labels <- c(labels, paste0("\n", seq(from = startyear, to = endyear, by = 1)))

  # Manually adjust the years to adjust for partial first and last years
  year_at <- seq(from = startyear + 0.5, to = endyear + 0.5, by = 1)
  year_at[1] <- (xlim[1] + min(startyear + 1, xlim[2])) / 2
  year_at[length(year_at)] <- (xlim[2] + min(endyear, xlim[2])) / 2
  at <- c(at, year_at)

  # drop any labels that are outside the x limits
  keep <- at > xlim[1] & at < xlim[2]
  return(list(at = at[keep], labels = labels[keep], ticks = ticks))
}

xlabels.ts_month <- function(xlim) {
  startmonth <- floor(xlim[1]*12)/12
  endmonth <- ceiling(xlim[2]*12)/12
  ticks <- seq(from = startmonth, to = (endmonth-1/12), by = 1/12)

  # convert the labels to month letters
  months <- seq(from = 1 + (startmonth-floor(startmonth)) * 12, length.out = length(ticks))
  labels <- substr(month.abb[1 + (months - 1) %% 12], 1, 1)
  at <- ticks + 0.5 * 1/12

  # add years
  startyear <- floor(xlim[1])
  endyear <- floor(xlim[2])
  labels <- c(labels, paste0("\n", seq(from = startyear, to = endyear, by = 1)))

  # Manually adjust the years to adjust for partial first and last years
  year_at <- seq(from = startyear + 0.5, to = endyear + 0.5, by = 1)
  year_at[1] <- (xlim[1] + min(startyear + 1, xlim[2])) / 2
  year_at[length(year_at)] <- (xlim[2] + min(endyear, xlim[2])) / 2
  at <- c(at, year_at)

  # drop any labels that are outside the x limits
  keep <- at > xlim[1] & at < xlim[2]
  return(list(at = at[keep], labels = labels[keep], ticks = ticks))
}

xlabels.ts <- function(xlim, layout, xfreq) {
  layout_factor <- getlayoutfactor(layout)
  if (!is.null(xfreq)) {
    if (xfreq == "decade") {
      return(xlabels.ts_decade(xlim, layout_factor))
    } else if (xfreq == "year") {
      return(xlabels.ts_year(xlim,layout))
    } else if (xfreq == "quarter") {
      return(xlabels.ts_quarter(xlim))
    } else if (xfreq == "month") {
      return(xlabels.ts_month(xlim))
    }
  } else {
    if (xlim[2] - xlim[1] >= 50*layout_factor) {
      return(xlabels.ts_decade(xlim, layout_factor))
    } else if (xlim[2] - xlim[1] >= 3) {
      return(xlabels.ts_year(xlim,layout))
    } else if (xlim[2] - xlim[1] >= 1) {
      return(xlabels.ts_quarter(xlim))
    } else {
      return(xlabels.ts_month(xlim))
    }
  }
}

xlabels.categorical <- function(xlim, xvar, layout, showall) {
  start <- 1
  end <- length(xvar)
  at <- seq(from = start, to = end, by = 1) + 0.5
  labels <- xvar
  if (!showall) {
    layout_factor <- getlayoutfactor(layout)
    keep <- restrictlabels(labels, layout_factor)
    at <- at[keep]
    labels <- labels[keep]
  }

  # drop any labels that are outside the x limits
  keep <- at >= xlim[1] & at <= xlim[2]
  labels <- labels[keep]
  at <- at[keep]

  ticks <- seq(from = start, to = end, by = 1)
  keep <- ticks >= xlim[1] & ticks <= xlim[2]
  ticks <- ticks[keep]

  return(list(at = at, labels = labels, ticks = ticks))
}

xlabels.scatter <- function(xlim, xvalues) {
  scale <- defaultscale(xlim[2]-(xlim[2]-xlim[1])/10000, xlim[1])
  scale <- createscale(scale$min,scale$max,scale$nsteps)
  # drop any labels that are outside the x limits
  keep <- scale >= xlim[1] & scale <= xlim[2]
  return(list(at = scale[keep], labels = scale[keep], ticks = scale[keep]))
}

xlabels <- function(xlim, xvar, xfreq, ists, layout, showall) {
  if (ists) {
    return(xlabels.ts(xlim, layout, xfreq))
  } else if (is.scatter(xvar)) {
    return(xlabels.scatter(xlim, xvar))
  } else {
    return(xlabels.categorical(xlim, xvar, layout, showall))
  }
}

handlexlabels <- function(data, xlim, xfreq, layout, showall) {
  out <- list()
  for (p in names(data)) {
    if (!is_empty(data[[p]])) {
      out[[p]] <- xlabels(xlim[[p]], data[[p]]$x, xfreq[[p]],
                          data[[p]]$ts, layout, showall)
    }
  }
  if (length(out) == 0) {
    # empty graph
    for (p in names(data)) {
      out[[p]] <-  xlabels(xlim[[p]], NULL, NULL, TRUE, layout, showall)
    }
  } else {
    # This duplicates horizontal
    out <- duplicateaxes(out, data, layout)
    # Now duplicate vertical
    out <- duplicateaxes_vertical(out, data, layout)
  }
  return(out)
}

warnifxdiff <- function(xlim) {
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

defaultxscale <- function(xvars, ists, layout) {
  if (is.numeric(xvars) && ists) {
    start <- min(xvars, na.rm = TRUE)
    end <- max(xvars, na.rm = TRUE)
    if (end - start >= 3) {
      return(c(floor(start),ceiling(end)))
    } else if (end - start >= 1) {
      return(c(floor(start*4)/4,ceiling(end*4)/4))
    } else if (end - start > 0) {
      return(c(floor(start*12)/12,ceiling(end*12)/12))
    } else {
      # Singleton observation, use years, not obvious what to do here?
      if (start %% 1 == 0) {
        # Right on a year, give half a year either side?
        return(start-0.5,end+0.5)
      } else {
        # Otherwise just give the whole year that the singleton appears in
        return(c(floor(start),ceiling(end)))
      }
    }
  } else if (is.scatter(xvars)) {
    scale <- defaultscale(max(xvars, na.rm =TRUE), min(xvars, na.rm =TRUE))
    return(c(scale$min,scale$max))
  } else {
    # Categorical
    return (c(1, length(xvars)+1))
  }
}

add_lastyear_padding <- function(xvars, xlim, layout) {
  end <- max(xvars, na.rm = TRUE)
  if (getlayoutfactor(layout) < 1 ||
      (xlim[2] - end) / (xlim[2] - xlim[1]) < LASTYEARPADDING) {
    padding <- LASTYEARPADDING*(xlim[2]-xlim[1])
    return(c(xlim, ceiling(padding*4)/4))
  } else {
    return(xlim)
  }
}


xlimconform <- function(xlim, data, layout) {
  panels <- names(data)
  if(!is.list(xlim) && length(xlim) > 0) {
    xlim <- as.list(rep(list(xlim), length(panels)))
    names(xlim) <- panels
  }

  out <- list()
  for (p in panels) {
    if (!is_empty(data[[p]])) {
      out[[p]] <- defaultxscale(data[[p]]$x, data[[p]]$ts, layout)

      if (is.null(xlim[[p]])) {
        if (data[[p]]$ts && (out[[p]][2] - out[[p]][1]) > 3) {
          out[[p]] <- add_lastyear_padding(data[[p]]$x, out[[p]], layout)
        }
      } else {
        if (is.finite(xlim[[p]][1])) {
          out[[p]][1] <- xlim[[p]][1]
        }
        if (is.finite(xlim[[p]][2])) {
          out[[p]][2] <- xlim[[p]][2]
        } else {
          # Only try to add padding if we haven't set an upper bound
          if (data[[p]]$ts && (out[[p]][2] - out[[p]][1]) > 3) {
            out[[p]] <- add_lastyear_padding(data[[p]]$x, out[[p]], layout)
          }
        }
      }
    }
  }

  if (length(out) == 0) {
    # empty graph
    for (p in names(data)) {
      out[[p]] <-  EMPTYXSCALE
    }
  } else {
    out <- duplicateaxes(out, data, layout)
    out <- duplicateaxes_vertical(out, data, layout)
  }

  # have a check for non-matching xlimits
  warnifxdiff(out)
  return(out)
}

collapse_in_padding <- function(xlim) {
  lapply(xlim, function(x)
    if (length(x) == 3) {
      return(c(x[1], x[2] + x[3]))
    } else {
      return(x)
    })
}

handleaxislabels <- function(labels, panels) {
  if (is.list(labels)) {
    return(labels)
  } else {
    out <- list()
    for (p in panels) {
      out[[p]] <- labels
    }
    return(out)
  }
}

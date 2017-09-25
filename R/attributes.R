fillwith <- function(tofill, default) {
  out <- list()
  ct <- 1
  for (s in names(tofill)) {
    # Give it a default
    out[s] <- default[ct]
    ct <- ct + 1
    # Reset the default cycle counter if necessary
    if (ct > length(default)) {
      ct <- ct %% length(default)
      if (ct == 0) {
        ct <- 1 # handles defaults with length 1, because mod 1 returns 0
      }
    }
  }
  return(out)
}

handleattribute <- function(series, duplicates, att, default) {
  if (is.null(att)) {
    return(fillwith(series, default))
  } else if (!is.list(att)) {
    return(fillwith(series, att))
  } else {
    # Have supplied a list
    out <- list()
    ct <- 1
    for (s in names(series)) {
      if (s %in% names(att)) {
        # Has been suplied
        out[s] <- att[s]
      } else if (s %in% names(duplicates) && duplicates[[s]] %in% names(att)) {
        # Has been supplied, but with the non-duplicate name
        out[s] <- att[duplicates[[s]]]
      } else {
        # Give it a default
        out[s] <- default[ct]
        ct <- ct + 1
        # Reset the default cycle counter if necessary
        if (ct > length(default)) {
          ct <- ct %% length(default)
          if (ct == 0) {
            ct <- 1 # handles defaults with length 1, because mod 1 returns 0
          }
        }
      }
    }
    return(out)
  }
}

handleattributes <- function(series, duplicates, colin, pchin, ltyin, lwdin, barcolin) {
  col <- handleattribute(series, duplicates, colin, DEFAULTCOLORS)
  pch <- handleattribute(series, duplicates, pchin, DEFAULTPCH)
  lty <- handleattribute(series, duplicates, ltyin, DEFAULTLTY)
  lwd <- handleattribute(series, duplicates, lwdin, DEFAULTLWD)
  barcol <- handleattribute(series, duplicates, barcolin, DEFAULTBARCOL)
  return(list("col" = col, "pch" = pch, "lty" = lty, "lwd" = lwd, "barcol" = barcol))
}

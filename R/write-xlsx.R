create_data_panel <- function(panel, data) {
  df <- data.frame(agg_x_variable = panel$x, stringsAsFactors = FALSE)
  for (i in seq_along(panel$series)) {
    newdf <- data.frame(agg_x_variable = series_x_values(panel, i),
                        y = series_values(panel, i),
                        stringsAsFactors = FALSE)
    names(newdf) <- c("agg_x_variable", panel$series[[i]]$name)
    df <- dplyr::full_join(df, newdf, by = "agg_x_variable")
  }
  colnames(df)[1] <- ""
  return(df)
}

write_to_excel <- function(gg, filename) {
  metadata_names <- c("Title", "Subtitle", "Footnotes", "Sources")
  metadata_values <- c(ifelse(is.null(gg$title), "", gg$title),
                       ifelse(is.null(gg$subtitle), "", gg$subtitle),
                       paste(gg$footnotes, collapse = "; "),
                       paste(gg$sources, collapse = "; "))
  metadata <-
    data.frame(
      Metadata = metadata_names,
      Value = metadata_values,
      stringsAsFactors = FALSE
    )

  panels <- lapply(gg$data, create_data_panel, data = gg$data)

  writexl::write_xlsx(append(list(Meta = metadata), panels),
                      path = filename,
                      col_names = TRUE,
                      format_headers = TRUE)
}

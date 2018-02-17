#which_output()
#' @rdname Output
#' @description Output mit  knitr::kable Funftioniert nur wenn options(knitr.table.format = "latex")
#' gesetzt wird (das gilt nur fuer Pdf)
#' @export
#'
Output_kable <- function(x, ...) {
  UseMethod("Output_kable")
}

#' @rdname Output
#' @export
Output_kable.list <- function(x, ...) {
  cname <- names(x)
  #cat("\nnames: ", cname)
  for (i in  cname)
    Output_kable.default(x[[i]], ...)
}

#' @rdname Output
#' @param booktabs kable an Latex Latex
#' @param col.names  Output_kable:  fuer tintPdf format = "latex"
#' @export
Output_kable.default <-
  function(x, caption = NULL,
           col.names = colnames(x),
          # format = "latex",
           booktabs=TRUE,



           note = NULL,
           output =  which_output(),
           print_col = NULL,
           ##Sprachuebesaetzung
           fix_colnames = TRUE,
           ...) {

    # format <- switch(
    #   tolower(format),
    #   tint = "latex",
    #   pdf = "latex",
    #   latex = "latex",
    #   html = "html",
    #   "html"
    # )

    caption <- Caption(caption, attr(x, "caption"))
    note <- Note(note, attr(x, "note"))

    if (!is.null(print_col)) {
      x <- x[, print_col]
      col.names <- colnames(x)
    }

    result_tbl_names <- stringr::str_split(col.names, "_")
    ebenen <- max(lengths(result_tbl_names), na.rm = TRUE)

    if (ebenen == 3) {
      #Fehler mit Namee_name_SD abfangen
      result_tbl_names <- stringr::str_split(col.names, "_", 2)
      ebenen <- max(lengths(result_tbl_names), na.rm = TRUE)
    }


    dt <- cleanup_for_latex(x)

    if (ebenen == 1 & output == "markdown") {
      print(
        kableExtra::kable_styling(
        knitr::kable(
        dt,
        #format,
        row.names = FALSE,
        col.names = col.names,
        booktabs = booktabs,
        caption = caption
      )))

    }
    else if (ebenen == 2 & output == "markdown") {
      a1 <- sapply(result_tbl_names, "[", 1)
      a2 <- sapply(result_tbl_names, "[", 2)
      nas <- is.na(a2)
      a2[nas] <- a1[nas]
      a1[nas] <- ""
      header <- a2
      cgroup <-  rle(a1)$values
      n.cgroup <- rle(a1)$lengths
      header_above <- ifelse(n.cgroup == 1, " ", n.cgroup)
      names(header_above) <-  gsub("&nbsp;", ' ', cgroup)

      print(kableExtra::add_header_above(
        knitr::kable(
          dt,
         # format,
          row.names = FALSE,
          col.names = header,
          booktabs = booktabs,
          caption = caption,
          ...
        ),
        header_above
      ))

    }
    else{
      print(dt)
    }

    invisible(dt)
  }






cleanup_for_latex <- function(x) {
  data.frame(plyr::llply(x, function(strg) {
    if (is.character(strg) | is.factor(strg)) {
      strg <- gsub("&nbsp;", ' ', strg)
      strg[is.na(strg)] <- ""
      strg
    }
    else
      strg
  }))
}


#' Ausgabe von Tabellen
#' 
#' Output.data.frame ist die Standart-Funktion fuer die
#' Ausgabe. Sie Arbeitet mit \code{htmlTable}, die einzelnen
#' Header-Ebenen werden ueber Header1_M, Header1_SD gesteuert.
#' Auch die Funktionen Output.table und xtabl arbeiten ueber diese Funktion.
#' @param x dataframe
#' @param caption,note  Ueberschrift Fussnote
#' @param output welcher output, text, html, markdown
#' @param print_col Spalten nicht Ausgeben (data.frame)
#' @param col_names,fix_colnames Spalten Ueberschrift aendern (data.frame)
#' @param css.table,css.cell,align Format an Output.data.frame (htmlTable)
#'  padding-left: .5em; padding-right: .2em; Output.data.frame (htmlTable)
#' @param booktabs,latex_options an kableExtra
#' @param ... an knit
#'
#' @return null
#' @export
#' @examples 
#' 
#' #' 
#'  df1 <- data.frame(
#' term = c("A", "B", "C", "D"),
#' n = c(23, 14, 56, 2),
#' m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA)
#' )
#' 
#' 
#' 
#' df2 <- data.frame(
#'   term = c("A", "B", "C", "D"),
#'   G1_k_n = c(23, 14, 56, 2),
#'   G1_k_m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA),
#'   G2_n = c(33, 35, 78, 21),
#'   G2_m = c("4.9 (2.7)", "4.7 (2.5)", "4.1 (5.6)", "4.2 (5.6)")
#'   
#' )
#'  Output2(df1)
#' Output2(df2)
#' 
#' Output2(df1, output = "html")
#' Output2(df2, output = "html")
#' 
#' 
#' #"markdown"
#' 
#' Output2(df1, output = "markdown")
#' Output2(df2, output = "markdown")
 
Output.data.frame <-
  function(x,
           caption = NULL,
           note = NULL,
           output =  which_output(),
           print_col = NULL,
           col_names = NULL,
           fix_colnames = TRUE,
           ##Sprachuebesaetzung
           css.table = "padding-left: .5em; padding-right: .2em;",
           css.cell = 'padding-left: .5em; padding-right: .2em;',
           booktabs = TRUE,
           latex_options = c("hold_position"),
           align = "l",
           ...) {
    
    if (is.na(output) | nrow(x) == 0)
      return(NULL)
  col.names <-  names(x) 
    caption <- Caption(caption, attr(x, "caption"))
    note <- Note(note, attr(x, "note"))
    
    if (!is.null(print_col))
      x <- x[, print_col]
    else if (all(x[, 2] == "")) {
      # loesen der leeren Spalte bei APA2
      x <- x[,-2]
      col.names <- col.names[-2]
    }
    
    tbl <- tbl_header(x, col.names)
    # print(tbl)
    if (output == "html" | output == "markdown_html") {
      x <- insert_nbsp(x)
      tbl$header <-  gsub(" +", '&nbsp;', tbl$header)
      tbl$cgroup <-  gsub(" +", '&nbsp;', tbl$cgroup)
      if (is.null(tbl$header_above)) {
        res <- htmlTable::htmlTable(
          x,
          caption = caption,
          header = tbl$header,
          rnames = FALSE,
          align = align,
          tfoot = note,
          escape.html = FALSE,
          css.table = css.table,
          css.cell = css.table,
          ...
        )
      }
      else{
        res <- htmlTable::htmlTable(
          x,
          caption = caption,
          header = tbl$header,
          rnames = FALSE,
          cgroup = tbl$cgroup,
          n.cgroup = tbl$n.cgroup,
          align = align,
          tfoot = note,
          escape.html = FALSE,
          css.table = css.table,
          css.cell = css.table,
          ...
        )
      }
      if (output == "html") {
        HTML_BR()
        HTML_CENTER(res)
        HTML_BR()
      } else{
        print(res)
      }
      
      
    }
    else if (output ==  "markdown") {
    
      x <- cleanup_nbsp(x)

      if (is.null(tbl$header_above)) {
        print(kableExtra::kable_styling(
          knitr::kable(
            x,
            row.names = FALSE,
            col.names = tbl$header,
            booktabs = booktabs,
            caption = caption,
            ...
          ),
          latex_options = latex_options
        ))
        
      }
      else {
 
        print(
          kableExtra::add_header_above(
            kableExtra::kable_styling(
              knitr::kable(
                x,
                row.names = FALSE,
                col.names = tbl$header,
                booktabs = booktabs,
                caption = caption,
                ...
              ),
              latex_options = latex_options
            ),
            tbl$header_above
          )
        )
      }
    }
    else{
      x <- cleanup_nbsp(x)
      
      if (!is.null(tbl$header_above))
        tbl$header <-
          ifelse(tbl$header_above2 == "",
                 tbl$header,
                 paste0(tbl$header_above2, " / ", tbl$header))
      
    print(
        knitr::kable(
          x,
          row.names = FALSE,
          col.names = tbl$header,
          booktabs = booktabs,
          caption = caption,
          format = "pandoc"
        )
      )
    }
    
    invisible(x)
  }





#' @rdname Output
#' @description Output.iste: einzeln in einen data.frame transformieren
#' @export
Output.list <- function(x,
                        ...) {
  for (i in 1:length(x))
    Output(x[[i]], ...)
}




#' @rdname Output
#' @description Output.matrix: umwandeln in einen data.frame
#' @export
Output.matrix <- function(x, ...) {
  Output(fix_to_data_frame(x), ...)
}



#' @rdname Output
#' @description Output.stp25: experimenteller Prototyp
#' @export
Output.stp25 <- function(x, ...) {
  # noch nicht getestet
  if (is.list(x)) {
    for (i in i:length(x))
      Output.data.frame(x[[i]], ...)
  }
  else
    Output.data.frame(x, ...)
}

#' Header aufbereiten
#'
#'Interne Funktion
#' @param x data.frame
#' @param col.names colnames(x)
#' @param col_names optionale Namen 
#' @param fix_colnames an translate TRUE/FALSE
#'
#' @return list()

tbl_header <-
  function(x,
           col.names = colnames(x),
           col_names = NULL,
           fix_colnames = FALSE) {
    header <- col.names
    header_above <- NULL
    cgroup <- NULL
    n.cgroup <- NULL
    a1 <- a2 <- NULL
    
    result_tbl_names <- stringr::str_split(col.names, "_")
    ebenen <- max(lengths(result_tbl_names), na.rm = TRUE)
    
    if (ebenen == 3) {
      #Fehler mit Name_name_SD abfangen
      result_tbl_names <-
        stringr::str_split(col.names, "_", 2)
      ebenen <- max(lengths(result_tbl_names), na.rm = TRUE)
    }
    
    if (ebenen == 2) {
      a1 <- sapply(result_tbl_names, "[", 1)
      a2 <- sapply(result_tbl_names, "[", 2)
      
      nas <- is.na(a2)
      a2[nas] <- a1[nas]
      a1[nas] <- ""
      header <- a2
      cgroup <-  rle(a1)$values
      n.cgroup <- rle(a1)$lengths
      
      
      header_above <- n.cgroup
      # add_header_above zero-length variable
      # macht probleme daher
  
      names(header_above) <-  ifelse( nchar(cgroup) == 0, " ", cgroup)
      
    }
    
    header <-
      find_col_names(col_names, header, fix_colnames)
    
    list(
      ebenen = ebenen,
      header = header,
      header_above = header_above,
      cgroup = cgroup,
      n.cgroup = n.cgroup,
      header_above2 = a1
    )
    
  }

#' Leehrzeiche auffüllen
#'
#' Interne Funktion Html &nbsp; ergaenzen
#' @param x data.frame
#'
#' @return data.frame
insert_nbsp <- function(x) {
  data.frame(plyr::llply(x, function(x) {
    if (is.character(x))
      gsub(" +", '&nbsp;', x)
    else
      x
  }))
}

#' nbsp entfernen
#'
#' Interne Funktion Html &nbsp; erntfernen
#' @param x data.frame
#'
#' @return data.frame
cleanup_nbsp <- function(x) {
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


#





 

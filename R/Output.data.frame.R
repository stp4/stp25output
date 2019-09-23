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
#' @param linesep linesep = ""  linesep = c("", "", "\\midrule")
#' @param ...  
#'
#' @return null
#' @export
#'
#' @examples
#' #
#' 
#' 
#' 
#' df1 <- data.frame(
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
#'   )
#' #+ df-default ,  results='asis'
#' df1 %>% Output()
#' 
#' #+ df-false ,  results='asis'
#' df1 %>% Output(output=FALSE)
#' 
#' #+ df-true
#' df1 %>% Output(output=TRUE)
#' 
#' #+ df-html
#' df1 %>% Output(output="html")
#' 
#' #+ df-text ,  results='asis'
#' df1 %>% Output(output="text")
#' 
#' #+ df-word ,  results='asis'
#' df1 %>% Output(output="word")
#' 
#' 
#' #+ df-mark
#' df1 %>% Output(output="markdown")
#' 
#' 
#' 
#' # df1 %>%  Output(linesep = c("", "", "\\midrule"))

#' 
Output.data.frame <-
  function(x,
           caption = NULL,
           note = NULL,
           output =  which_output(),
           print_col = NULL,
           col_names = NULL, 
           fix_colnames = options()$stp25$language != "",
           add_row = NULL,
           css.table = 'padding-left: .5em; padding-right: .2em;',
           css.cell = 'padding-left: .5em; padding-right: .2em;',
           booktabs = TRUE,
           latex_options = c("hold_position"),
           linesep = "",
           align = "l",
           ...) {
    
    if (nrow(x) == 0)  return(NULL)
    # abbruch bei output=FALSE bei TRUE Ausgabe als Text
    if (is.logical(output)) {
      if (output)
        output <- "text"
      else
        return(NULL)
    }
    
 #   cat(" In Output.data.frame output=", output, "  \n")
    
    # Zellen-Spalten bearbeiten
    if (!is.null(print_col)) {
      x <- x[print_col]
    }
    else if (all(x[, 2] == "")) {
      # loesen der leeren Spalte bei APA2
      
      if (!is.null(col_names)) {
        if (length(names(x)) == length(col_names))
          names(x) <- col_names
         
        else
          warnings("Die col_names stimmen nicht!")
       fix_colnames <- FALSE  
         
      }
      x <- x[,-2]
    }
    
    if (!is.null(col_names)) {
      if (length(names(x)) == length(col_names))
        names(x) <- col_names
      else
        warnings("Die col_names stimmen nicht!")
      fix_colnames <- FALSE 
    }
    
    tbl <- tbl_header(x, fix_colnames = fix_colnames)
    
    if (!is.null(add_row)) {
      x <- add_row_df(x, add_row)
    }
    
    
    caption <- Caption(caption, attr(x, "caption"))
    note <- Note(note, attr(x, "note"))
    
    # wen Output() in spin-word direkt aufgerufen wird soll Output_word() die Tabelle erstellen
    # APA(..., output=FALSE) %>% Output()
    ## output ==  "word" geht nicht weil  ausgabe nicht an knit_print weitergegeben wird.
    if (output == "word") {
      return(Output_word(x,
                         caption, note, output,
                         print_col, col_names, fix_colnames))
    }
    else if (output == "text") {
      if (!is.null(tbl$header_above))
        names(x) <-
          ifelse(tbl$header_above2 == "",
                 tbl$header,
                 paste0(tbl$header_above2, "_", tbl$header))
      else{
        names(x) <- tbl$header
      }
      cat("\n", caption, "\n")
      print(x)
      cat("\n", note, "\n\n")
    }
    else if (output == "html" | output == "markdown_html") {
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
    else if (output ==  "markdown" | output=="md") {
      
    #  cat("\nin output markdown\n")
      x <- cleanup_nbsp(x)
      
      if (is.null(tbl$header_above)) {
        print(kableExtra::kable_styling(
          knitr::kable(
            x, format="latex",
            #  wegen kableExtra in stp25output ( Null ist aber die Default Einstellung)
            #  die Caption wird bei Null oberhalb der Tabelle angebracht bei
            #  Latex am Rand (Margin)
            row.names = FALSE,
            col.names = tbl$header,
            booktabs = booktabs,
            caption = caption,
            linesep = linesep
          ),
          latex_options = latex_options
        ))
      }
      else {
        print(kableExtra::add_header_above(
          kableExtra::kable_styling(
            knitr::kable(
              x, format="latex",
              row.names = FALSE,
              col.names = tbl$header,
              booktabs = booktabs,
              caption = caption,
              linesep = linesep
            ),
            latex_options = latex_options
          ),
          tbl$header_above
        ))
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
          format = "pandoc",
          linesep = linesep
        )
      )
      cat("\n", note, "\n\n")
    }
    
    invisible(x)
  }





#' @rdname Output
#' @description Output.iste: einzeln in einen data.frame transformieren
#' @export
#' 
Output.list <- function(x,
                        output =  which_output(),
                        ...) {
  res<- list()
    for (i in 1:length(x))
     res[[i]] <- Output(x[[i]], output=output, ...)
  
  if(output=="word"){ return(res) }
  else { invisible(x)}
}




#' @rdname Output
#' @description Output.matrix: umwandeln in einen data.frame
#' @export
#' 
Output.matrix <- function(x,
                          output =  which_output(),
                          ...) {
  Output(fix_to_data_frame(x), output=output, ...)
}



#' @rdname Output
#' @description Output.stp25: experimenteller Prototyp
#' @export
#' 
Output.stp25 <- function(x,
                         output =  which_output(),
                         ...) {
  # noch nicht getestet
  if (is.list(x)) {
    for (i in i:length(x))
      Output.data.frame(x[[i]], output=output, ...)
  }
  else
    Output.data.frame(x, output=output, ...)
}

#' Header aufbereiten
#'
#' Interne Funktion
#' @param x data.frame
#' @param col.names colnames(x)
#' @param fix_colnames an translate TRUE/FALSE
#'
#' @return list(header,header_above,cgroup,n.cgroup,header_above2,)
#'
tbl_header <-
  function(x,
           fix_colnames = FALSE,
           col.names = colnames(x)) {
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
      # add_header_above zero-length variable macht probleme daher
      names(header_above) <-
        ifelse(nchar(cgroup) == 0, " ", cgroup)
    }
    
    if (fix_colnames)
      header <- Names2Language(header)
    
    list(
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
#' 
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
#' 
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


add_emty_col <- function (x,  df = "hallo welt", na_value = NA)
{
  df <- tibble::tibble(df)
  names(df) <-  names(x)[1L]
  attr(df, "row.names") <- .set_row_names(max(1L, nrow(df)))
  
  missing_vars <- setdiff(names(x), names(df))
  df[missing_vars] <- na_value
  df <- df[names(x)]
  df
}


#' Orginal  tibble:::rbind_at
#' @noRd
#' 
rbind_at <- function (old, new, pos)
{
  idx <- NULL
  if (nrow(old) == 0) {
    old <- old[1,]
    out <- rbind(old, new)[-1,]
  }
  else {
    out <- rbind(old, new)
    pos_old <- seq_len(nrow(old))
    pos_new <- seq_len(length(new)) + nrow(old)
    for (i in pos_old) {
      if (any(i == pos)) {
        idx <- c(idx, pos_new[which(i == pos)], i)
      }
      else{
        idx <- c(idx, i)
      }
    }
    out <- out[idx,]
  }
  out
}

#' @rdname Output
#' @param add_row list (c("Erste Zeile" = 1, "Dritte" = 3))
#' @examples
#' df <-   data.frame(
#' Source = c("A", "B", "C", "F"),
#' x = 1:4,
#' y = 1:4,
#' stringsAsFactors = FALSE
#' )
#'
#' # add_row_df(df, c("Erste Zeile" = 1, "Dritte" = 3))
#'
#'
add_row_df <- function(x, add_row = NULL) {
  new_element <- add_emty_col(x, names(add_row))
  rbind_at(x, new_element, pos = as.numeric(add_row))
}

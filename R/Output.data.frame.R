#' @rdname Output
#' @description  Output.data.frame ist die Standart-Funktion fuer die
#' Ausgabe. Sie Arbeitet mit htmlTable, die einzelnen
#' Header-Ebenen werden ueber Header1_M, Header1_SD gesteuert.
#' Auch die Funktionen Output.table und xtabl arbeiten ueber diese Funktion.
#' 
#' @param x dataframe
#' @param caption,note  Ueberschrift Fussnote
#' @param output welcher output, text, html, markdown
#' @param print_col Spalten nicht Ausgeben (data.frame)
#' @param col_names,fix_colnames Spalten Ueberschrift aendern (data.frame)
#' @param css.table,css.cell,align Format an Output.data.frame (htmlTable)
#'  padding-left: .5em; padding-right: .2em; Output.data.frame (htmlTable)
#' @param booktabs,latex_options an kableExtra
#' @param linesep linesep = ""  linesep = c("", "", "\\midrule")
#' @param rgroup,n.rgroup,cgroup,n.cgroup an htmlTable
#' @param ...  
#'
#' @return nix
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
           
           # htmlTable
           # header = NULL,
           # rnames = NULL,
           # rowlabel = NULL,
           # tfoot = NULL,
           # label = NULL,
           rgroup = attr(x, "rgroup", TRUE),
           n.rgroup =  attr(x, "n.rgroup", TRUE),
           cgroup = NULL,
           n.cgroup = NULL,
           # tspanner = NULL,
           # n.tspanner = NULL,
           # total = NULL,
           # ctable = TRUE,
           # compatibility = getOption("htmlTableCompat", "LibreOffice"),
           # cspan.rgroup = "all",
           # escape.html = FALSE,
      
           ...) {
# abbruch bei output=FALSE bei TRUE Ausgabe als Text    
if (nrow(x) == 0)  return(NULL)
  
if (is.logical(output)) {
      if (output)
        output <- "text"
      else
        return(NULL)
    }
    
# Zellen-Spalten bearbeiten
if (!is.null(print_col)) {
      x <- x[print_col]
}
else if (all(x[, 2] == "")) {
# loeschen der leeren Spalte bei APA2
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
      x <- stp25aggregate::add_row_df(x, add_row)
    }
    
if (output == "docx") {
# In spin-word geht Word.doc  nicht weil die Ausgabe nicht an knit_print weitergegeben wird. 
# Alternative APA(..., output=FALSE) %>% Output()")
  return(Output_word(x,
                     caption,
                     note,
                     output,
                     print_col,
                     col_names,
                     fix_colnames))
  
}
    
if (output == "text") {
  caption <- Caption(caption, attr(x, "caption"))
  note <- Note(note, attr(x, "note"))
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
  if (is.character(note))
    cat("\n", note, "\n\n")
}
else if ( output == "html" | output=="markdown_html") {
  caption <- Caption(caption, attr(x, "caption"))
  note <- Note(note, attr(x, "note"))  
      
      tbl$header <-  gsub(" +", '&nbsp;', tbl$header)
      tbl$cgroup <-  gsub(" +", '&nbsp;', tbl$cgroup)
      
      if (is.null(tbl$header_above)) {
        res <- htmlTable::htmlTable(
          insert_nbsp(x),
          caption = caption,
          header = tbl$header,
          rnames = FALSE,
          rgroup = rgroup,
          n.rgroup = n.rgroup,
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
          insert_nbsp(x),
          caption = caption,
          header = tbl$header,
          rnames = FALSE,
          rgroup = rgroup,
          n.rgroup = n.rgroup,
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
      
      # Copy-Paste problem mit Word
      res<- gsub( "grey;", "black;", res)
      
      if (output == "html") {
        HTML_BR()
        HTML_CENTER(res)
      #  if (is.character(note))   Text(note)
        HTML_BR()
      } else{
        print(res)
      #  if (is.character(note))  print(note)
      }
    }
else if (output =="latex") {

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
      
     if (is.character(note)) Text(note)
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
          format = output,
          linesep = linesep
        )
      )
      if (is.character(note))   cat("\n", note, "\n\n")
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
  if (output == "docx") {
    if (length(x) > 1)
      Text("Weitere Tabellen:", names(x)[-1])
    x <- x[[1]]
    return(Output_word(x,
                       output = output,
                       ...))
  } else {
    res <- list()
    for (i in 1:length(x))
      res[[i]] <- Output(x[[i]],
                         output = output, ...)
    
    
    invisible(x)
  }
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




#' @rdname Output
#' @description Output.matrix: umwandeln in einen data.frame
#' @export
Output.matrix<- function(x, ...) {
 Output(fix_to_data_frame(x, ...))
}

#' @rdname Output
#' @description Output.iste: einzeln in einen data.frame transformieren
#' @export
Output.list<- function(x, ...) {
# noch nicht getestet
  for(i in 1:length(x))
  Output(fix_to_data_frame(x[[i]], ...))
}

#' @rdname Output
#' @description Output.stp25: experimenteller Prototyp
#' @export
Output.stp25<- function(x, ...) {
  # noch nicht getestet
  if(is.list(x)){
  for(i in i:length(x))
    Output.data.frame(x[[i]], ...)
  }
  else Output.data.frame(x, ...)
}
##methods(Output)

#' @rdname Output
#' @description Output.data.frame ist die Standart-Funktion fuer die
#' Ausgabe. Sie Arbeitet mit \code{htmlTable}, die einzelnen
#' Header-Ebenen werden ueber Header1_M, Header1_SD gesteuert.
#' Auch die Funktionen Output.table und xtabl arbeiten ueber diese Funktion.
#' 
#' @param caption,note  Ueberschrift Fussnote
#' @param print_col Spalten nicht Ausgeben (data.frame)
#' @param col_names,fix_colnames Spalten Ueberschrift aendern (data.frame)
#' @param css.table,css.cell Format an Output.data.frame (htmlTable)
#'  padding-left: .5em; padding-right: .2em; Output.data.frame (htmlTable)
#' @export
Output.data.frame <-
  function(x,
           caption = NULL, note = NULL,
           output =  which_output(),
           print_col = NULL,
           col_names = NULL,
           fix_colnames = TRUE,  ##Sprachuebesaetzung
           css.table = "padding-left: .5em; padding-right: .2em;",
           css.cell = 'padding-left: .5em; padding-right: .2em;',
           ...) {
    
    if(is.na(output)) return()

  HTML_BR()
  if(nrow(x) == 0) return("Kein Input!")
  caption <- Caption(caption, attr(x, "caption"))
  note <- Note(note, attr(x, "note"))

  align <- "l"
  my_names <- names(x)

  #-- Sub header
  result_tbl_names <- stringr::str_split(colnames(x), "_")
  ebenen <- max(lengths(result_tbl_names), na.rm = TRUE)
  if(ebenen==3){ #Fehler mit Namee_name_SD abfangen
   result_tbl_names <- stringr::str_split(colnames(x), "_", 2)
   ebenen <- max(lengths(result_tbl_names), na.rm = TRUE)
  }

  if (!is.null(print_col))
      x <- x[, print_col]
  else if (all(x[, 2] == "")) {# loesen der leeren Spalte
      x <- x[, -2]
      my_names <- my_names[-2]
  }

  if (output == "html") {
  #  if (output %in% c("html", "markdown")) {
    #cleanup_leerzeichen
    x <- data.frame(
           plyr::llply(x, function(x){
                            if(is.character(x)) gsub(" +", '&nbsp;', x)
                            else x}))
   names(x) <-
      find_col_names(col_names, my_names, fix_colnames)

      if (ebenen == 1) {
        res <- htmlTable::htmlTable(
          x,
          caption = caption,
          rnames = FALSE,
          align = align,
          tfoot = note,
          escape.html = FALSE,
          css.table = css.table, css.cell = css.table,
          ...
        )

#        print(as.character(res))
        if (output == "html" ) HTML_CENTER(res)
        else print(res)
      }
      else if (ebenen == 2) {
        a1 <- sapply(result_tbl_names, "[", 1)
        a2 <- sapply(result_tbl_names, "[", 2)
        nas <- is.na(a2)
        a2[nas] <- a1[nas]
        a1[nas] <- ""
        header <- a2
        cgroup <- rle(a1)$values
        n.cgroup <- rle(a1)$lengths
        res<-
          htmlTable::htmlTable(
            x,
            rnames = FALSE,
            align = align,
            header = header,
            cgroup = cgroup,
            n.cgroup = n.cgroup,
            caption = caption ,
            tfoot = note,
            escape.html = FALSE,
            css.table = css.table, css.cell = css.table,
            ...)
        # fuer markdown  else
       # print(res)

        if (output == "html" ) HTML_CENTER(res)
        else print(res)
      }
   else{ cat("\n mehere Ebenen\n")
         print(x)
     }
  }
  else if (output ==  "markdown") {

    print(knitr::kable(x,
                       row.names = FALSE,
                       caption = caption))
  }

    else{
      print(x)
    }
}

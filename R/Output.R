#' Ausgabe von HTML, Knit oder Text
#'  
#' Erstellt den Output von Objekten, diese koennen zB. mit Uberschriften versehen sein.
#' @name Output
#' @param x Objekt liste oder Dataframe
#' @param output HTML, Knit  oder Textfile (ist nicht gedacht zum Aendern)
#' @param ... weitere Einstellungen
#' @return HTML oder Textausgabe
#' @author Wolfgang Peter
#' @export
#' 
Output <- function(x, ...) {
  UseMethod("Output")
}


#' @rdname Output
#' @export
which_output<- function(...){
  x<- "text"
  if(is.null(knitr:::out_format())) {
    if (options()$prompt[1] == "HTML> ") x<-"html"
  } else{ if(knitr:::out_format() == "markdown") x<-"markdown"}

  x
}


Output.character <- function(x, output = options()$prompt[1] == "HTML> ", ...) {
  if(output){
  if(any(class(x)== "texregTable")) HTML_CENTER(x)
   else
     HTML_P(x)}
  else x
}

#' @rdname Output
#' @export
Output.vector <- function(x,
                          output = options()$prompt[1] == "HTML> ",
                          ...) {
  if(output)
      HTML_P(x)
  else x
}




#' @rdname Output
#' @description Output.htmlTable: Verarbeitet htmlTable-Objekt als auch Listen
#'  oder auch einfach nur ein string. Die Tabelle mit Header usw ist dabei
#'  schon vorbereitet. Bei Markdown kommt output=FALSE
#' @export
Output.htmlTable <- function(x,
                             output = which_output()) {

  if (output=="html") {
    if (is.list(x)) # von htmlTable
      HTML_CENTER( x[[1]] )
    else HTML_CENTER( x )

  }
  else if(output=="markdown"){
    if (is.list(x)) # von htmlTable
      print( x[[1]] )
    else print( x )
  }
  else{
    return(x[[1]])
  }
}


#' @rdname Output
#' @export
Output.NULL <- function(x, ...){
  if (!exists("Tab_Index"))
  cat("\nNach Tabelle ", Tab_Index, " ist der Output NULL!\n")
  
  HTML_BR()
}
#' @rdname Output
#' @export
Output.default <- function(x,
                           caption = "", note = "",
                           output = options()$prompt[1] == "HTML> ",
                           # print = TRUE,
                           col_names = NULL,
                           print_col = NULL,
                           ...) {
  
  Head("Warning in Output.default")
  cat("\nin Output.default\n")
  print(class(x))
  Caption2 <- function(...) {
    cptn <- gsub(" {2,}", " ", paste(...))
    #  cat("\n", cptn)
 #   Output_info$table <<-  c(Output_info$table, cptn)
    cptn
  }
  
  if (output) {
    ##htmlreg2Print2
    if (is.list(x) ) {
      for (i in names(x)) {
        if (!is.null(print_col))
          x[[i]] <- x[[i]][, print_col]
        else if (all(x[[i]][, 2] == ""))  {
          mynames <- names(x[[i]])
          x[[i]] <- x[[i]][, -2]
          names(x[[i]]) <- mynames[-2]
        }
        names(x[[i]]) <-
          find_col_names(col_names, names(x[[i]]), fix_colnames)
        htmlreg2Print2(
          x[[i]],
          caption = Caption2(Tab(), i, caption),
          caption.above = TRUE,
          inline.border = FALSE,
          note = note,
          center = TRUE
        )
      }
    } else if (is.matrix(x)) {
      Text("is.matrix")
      tab <- as.data.frame.matrix(x)
      tab <- cbind(" " = rownames(x), tab)
      htmlreg2Print2(
        tab,
        caption =  Caption2(Tab(), caption),
        caption.above = TRUE,
        inline.border = FALSE,
        note = note,
        center = TRUE
      )
    } else {
      # Text("else")
      if (!is.null(print_col))
        x <- x[, print_col]
      else if (all(x[, 2] == "")) {
        mynames <- names(x)
        x <- x[, -2]
        names(x) <- mynames[-2]
      }## x <- x[,-2] ## Spalte charactristik loeschen
      #  names(x) <- if(!is.null(col_names)) col_names else Names2Language(names(x))
      names(x) <-
        find_col_names(col_names, names(x), fix_colnames)
      
      htmlreg2Print2(
        x,
        caption = Caption2(Tab(), caption),
        caption.above = TRUE,
        inline.border = FALSE,
        note = note,
        center = TRUE
      )
    }
    x <- caption
    Text("")    #-- Leerzeichen zum besseren kopieren
  } else {
    if (!is.data.frame(x) & is.list(x)) {
      ##  ?
      for (i in names(x)) {
        if (!is.null(print_col))
          x[[i]] <- x[[i]][, print_col]
        else if (all(x[[i]][, 2] == "")) {
          mynames <- names(x[[i]])
          x[[i]] <- x[[i]][, -2]
          names(x[[i]]) <- mynames[-2]
        }
        names(x[[i]]) <-
          find_col_names(col_names, names(x[[i]]), fix_colnames)
      }
    }
    else if (is.data.frame(x)) {
      if (!is.null(print_col)) {
        x <- x[, print_col]
      }
      else if (all(x[, 2] == ""))  {
        mynames <- names(x)
        x <- x[, -2]
        names(x) <- mynames[-2]
      } else {
        cat("\n", class(x), "\n")
      }
      names(x) <-
        find_col_names(col_names, names(x), fix_colnames)
    }
    else {
      cat("\n", class(x), "\n")
    }
  }
  return(x)
}
#-----------------------------------------------------------------


 
#' @rdname Output
text_as_table <- function(x, ...) {
  strg <-  str_split(str_split(x, "\n")[[1]], "\\|")
  strg <- lapply(strg, function(x) {
    x <- gsub("\\t", "", x)
    gsub("(^ +)|( +$)", "", x)
  })
  if (length(strg) == 1) {
    Text(x)
    return(strg)
  }
  #Formatierungssymbole
  if (!all(lengths(strg) == lengths(strg)[1])) {
    wer_is_line <-
      function(s)
        lengths(lapply(s, function(x)
          grep("---", x[1])))
    wo_is_line <- wer_is_line(strg)
    wo_is_leer <- lengths(strg) == 1 - wo_is_line
    strg <- strg[!wo_is_leer]
    wo_is_line <- wer_is_line(strg)
    strg <- strg[!wo_is_line]
    wo_is_line <- which(wo_is_line == 1)
  }

  if (wo_is_line == 2) {
    head_names <- strg[[1]]
    mx <- t(sapply(strg[-1], c))
    colnames(mx) <- head_names
    htmlTable(mx, escape.html = FALSE, ...) #--library(htmlTable)
  }
  else {
    headernr <- 1:(wo_is_line - 1)
    mx <- t(sapply(strg[-headernr], c))
    head_names <- strg[headernr]
    header <- head_names[[2]]
    cgroup <- head_names[[1]]
    if (header[1] == "") {
      #-erste Spalte Loeschen
      header <- header[-1]
      cgroup <- cgroup[-1]
      cgroup_space <- cgroup == ""
      n.cgroup <-
        diff(c(which(!cgroup_space), length(cgroup_space) + 1))
      cgroup <- cgroup [!cgroup_space]
      rnames <- mx[, 1]
      mx <- mx[, -1]
    }
    Output(
      htmlTable(
        mx,
        cgroup = cgroup,
        n.cgroup = n.cgroup,
        rnames = rnames,
        header = header,
        escape.html = FALSE,
        ...
      ),output=output
    )
  }
}

#' @rdname Output
#' @param atr in Caption: alternativer Text
#' @description  Ueberschrift aus stp-Objekt: Caption(caption, attr(x, "caption"))
#' oder Note(note, attr(x, "note")) mit
Caption <- function(x=NULL, atr=NULL, output =  which_output()){
  caption <- if(is.null(x) & (!is.null(atr))) atr
  else if(!is.null(x)) x
  else ""
 if(output=="html") Tab(caption)
  else caption
}

Note<- function(x=NULL, atr=NULL){
  if(is.null(x) & (!is.null(atr))) atr
  else if(!is.null(x)) x
  else ""
}

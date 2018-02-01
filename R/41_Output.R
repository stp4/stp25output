#' @name Output
#' @rdname Output
#' @title Ausgabe von HTML
#' @description Erstellt Output von Objekten diese koennen mit Caption(caption, attr(x, "caption")),
#' oder Note(note, attr(x, "note")) mit Uberschriften versehen sein.
#' @param x Objekt liste oder Dataframe
#' @param caption  Ueberschrift
#' @param note  Fussnote
#' @param output HTML oder text
#' @return HTML oder Textausgabe
#' @author Wolfgang Peter
#' @export
Output <- function(x, ...) {
  UseMethod("Output")
}


# Helper
which_output<- function(...){

  x<- "text"
  if(is.null(knitr:::out_format())) {
    if (options()$prompt[1] == "HTML> ") x<-"html"
  } else{ if(knitr:::out_format() == "markdown") x<-"markdown"}

  x
}



#' @rdname Output
#' @description Output.character fuer brerits erstellten Html-Code
#' @export
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



# @rdname Output
# @export
#--  Helper
text_as_table <- function(x, ...) {
  strg <-  stringr::str_split(stringr::str_split(x, "\n")[[1]], "\\|")
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
    htmlTable::htmlTable(mx, escape.html = FALSE, ...) #--library(htmlTable)
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
      htmlTable::htmlTable(
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

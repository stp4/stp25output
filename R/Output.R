#' Ausgabe von HTML, Knit oder Text
#'
#' HTML :  htmlTable::htmlTable
#' 
#' markdown: knitr::kable -> kableExtra::kable_styling 
#' 
#' Text: knitr::kable
#' 
#' Word: knitr::kable(x, format = "pandoc")  
#' 
#' Output_word() flextable::regulartable
#'
#'
#' @name Output
#' @param x Objekt liste oder Dataframe
#' @param output FALSE = NULL,
#' TRUE = print,
#' "html" = htmlTable(), "markdown_html"= htmlTable(),
#' "markdown"= kable(),
#' "text"=kable(... "pandoc") und
#' "word"= flextable()
#' @param ... weitere Einstellungen
#' @return HTML oder Textausgabe
#' @author Wolfgang Peter
#' @export
#'
Output <- function(x, ...) {
  UseMethod("Output")
}

#' @rdname Output
Output.character <- function(x, output = which_output(), ...) {
  if (output=="html" | output=="markdown_html") {
    if (any(class(x) == "texregTable"))
      HTML_CENTER(x)
    else
      HTML_P(x)
  }
  else
    x
}

#' @rdname Output
Output.vector <- function(x,
                          output = which_output(),
                          ...) {
  if (output=="html" | output=="markdown_html")
    HTML_P(x)
  else
    x
}




#' @rdname Output
#' @description Output.htmlTable: Verarbeitet htmlTable-Objekt als auch Listen
#'  oder auch einfach nur ein string. Die Tabelle mit Header usw ist dabei
#'  schon vorbereitet. Bei Markdown kommt output=FALSE
#' @export
Output.htmlTable <- function(x,
                             output = which_output(), ...) {
  if (output == "html") {
    if (is.list(x))
      # von htmlTable
      HTML_CENTER(x[[1]])
    else
      HTML_CENTER(x)
  }
  else if (output == "markdown") {
    if (is.list(x))
      # von htmlTable
      print(x[[1]])
    else
      print(x)
  }
  else{
    return(x[[1]])
  }
}

#' @rdname Output
#' @export
Output.NULL <- function(x, ...) {
  if (!exists("Tab_Index"))
    cat("\nNach Tabelle ", Tab_Index, " ist der Output NULL!\n")
}
#' @rdname Output
#'
Output.default <- function(x, ...) {
  cat("\nin Output.default class: class(x)[1] \n")
}




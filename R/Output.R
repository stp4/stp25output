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
#' @description Output.xtable mit xtable lassen sich alternativ r-Objekte ausgeben. 
#'
#' @export
#' 
#' @examples 
#' 
#'  require(xtable)
#' ## Load example dataset
#' data(tli)
#' 
#' ## Demonstrate aov
#' fm1 <- aov(tlimth ~ sex + ethnicty + grade + disadvg, data = tli)
#' fm1.table <- xtable(fm1)
#' 
#' Output(fm1.table)
#' 
#' # ## Demonstrate lm
#' fm2 <- lm(tlimth ~ sex*ethnicty, data = tli)
#' fm2.table <- xtable(fm2)
#' fm2b <- lm(tlimth ~ ethnicty, data = tli)
#' 
#' Output(fm2.table)
#' Output(xtable(anova(fm2)))
#' Output(xtable(anova(fm2b, fm2)))
#' 
#' 
Output.xtable <- function(x,
                          caption = attr(x, "heading"),
                          digits = attr(x, "digits"),
                          output=which_output(),
                          ...) {
  
  caption <-  paste(caption, collapse= " ")
  res <- as.data.frame(x)
  
  res <- cbind(Source = row.names(res), res)
  c_names <- names(res)
  res <- fix_format(res, digits = digits)
  names(res) <- c_names
  
  Output.data.frame(res, caption=caption, output=output, ...)
  
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
  cat("\nin Output.default class: ",class(x)[1], " \n")
 Output( broom::tidy(x), ...) 
  
}






#' @rdname Output
#'
Output.psychobject <- function(x, 
                               digits = c(2, 2, 1, 3, 2, 2), 
                               ...) {
  Output(fix_format(x$summary,
                    digits = digits), 
         ...)
}

#' @rdname Output
#' @export
#'
#' @examples
#' warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
#' x <- emmeans::emmeans(warp.lm,  ~ wool | tension)
#' Output(x)
#' 
Output.emmGrid <- function(x, 
                           caption="Estimated marginal means", 
                           note="Least-squares means", ...) {
Output(fix_format(as.data.frame(x)), 
       caption = caption, 
       note = note,
       ...)
}

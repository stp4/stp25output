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
#' @export
#' 
Output.default <- function(x,            
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
  #  cat("\nin Output.default class: ",class(x)[1], " \n")
  Output( stp25tools::fix_to_df(x),  
          caption = caption,
          note = note,
          output =  output,
          print_col = print_col,
          col_names = col_names, 
          fix_colnames = fix_colnames,
          add_row = add_row,
          css.table =css.table,
          css.cell = css.cell,
          booktabs = booktabs,
          latex_options = latex_options,
          linesep = linesep,
          align=align) 
  
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
#' @export
#'
Output.character <- function(x,
                             caption = NULL,
                             note = NULL,
                             output = which_output()) {
  if (output == "html" | output == "markdown_html") {
    if (any(class(x) == "texregTable") | grepl("<table", x)) {
      Text(Caption(caption))
      HTML_CENTER(x)
      Text(Note(note))
    }
    else
      HTML_P(x)
  }
  else
    x
}

#' @rdname Output
#' @export
#' 
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
      HTML_CENTER( gsub("grey;", "black;", x[[1]]))
    else
      HTML_CENTER(gsub("grey;", "black;", x))
  }
  else if (output == "markdown") {
    if (is.list(x))
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
#' @export
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

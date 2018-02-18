#' HTML Text Formatting Elements
#'
#' HTML_ schreibt direkt ins HTML-File
#' @param x Text
#' @param output HTML, Knit  oder Textfile (ist nicht gedacht zum Aendern)
#'
#' @return Null doere Character-String
#' @export
#'
#' @examples
#' # HTML_P( "Hallo Welt", file="")
#'
HTML_ <- function(x, output = which_output()) {
  HTML_default(x, output)
}

HTML_default <- function(x, output =  which_output()) {
  if (output == "html")
    cat("\n", x, "\n",
      file = HTMLGetFile(), sep = "",append = TRUE
    )
  else
    cat(x)
}


#' @rdname HTML_
#' @description HTML_P <p> Paragraph
#' @export
HTML_P <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0("\n<p>", x, "</p>\n"), output)
else
  HTML_default(x, output)
}


#' @rdname HTML_
#' @description HTML_I <i>	Defines italic text
#' @export
HTML_I <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0("<i>", x, "</i>"), output)
else if (output == "markdown")
  HTML_default(paste0("_", x, "_"), output)
else
  HTML_default(x, output)}

#' @rdname HTML_
#' @description HTML_B <b>	Defines bold text
#' @export
HTML_B <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0("<b>", x, "</b>"), output)
else if (output == "markdown")
  HTML_default(paste0("__", x, "__"), output)
else
  HTML_default(x, output)
}

#' @rdname HTML_
#' @description HTML_S <small>	Defines smaller text
#' @export
HTML_S <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0("\n<small>", x, "</small>\n"), output)
else
  HTML_default(x, output)
}

#' @rdname HTML_
#' @description HTML_HR <hr> Lienie
#' @export
HTML_HR <- function (x, output = which_output()){
  if (output == "html")
    HTML_default("<hr>",  output)
else if (output == "markdown")
  HTML_default("***", output)
else
  HTML_default("--------------------", output)
}
#' @rdname HTML_
#' @description HTML_DIF <dif> Formatierungsbereich
#' @export
HTML_DIF <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0("<dif>", x, "</dif>"), output)
else
  HTML_default(x, output)
}
#' @rdname HTML_
#' @description HTML_CENTER <center> zentrieren
#' @export
HTML_CENTER <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0("<center>", x, "</center>"), output)
else
  HTML_default(x, output)
}
#' @rdname HTML_
#' @description HTML_NULL wie  HTML_BR <br> Zeilenumbruch aber mit Text
#' @export
HTML_NULL <- function (x, output = which_output()){
  if (output == "html")
    HTML_default(paste0(x, "<br>"), output)
  else
  NULL}


#' @rdname HTML_
#' @description HTML_BR <br> Zeilenumbruch aber ohne Text
#' @export
HTML_BR <- function (x, output = which_output()){
  if (output == "html")
    HTML_default("<br>", output)
else
  NULL}

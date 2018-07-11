#' Text Ausgabe
#' 
#' Ausgabe von \code{Text()} und Ueberschriften. \code{Head} ist dabei eine Kopie von \code{Text} mit
#' dem Parameter \code{style = 2}
#' @param ... one or more R objects, to be converted to character vectors.
#' @param style 0 = p oder normaler Text 2 = h2 also Ueberschrift bei Consolen-Ausgabe
#' @param char ist nicht zu Verwenden Text Trennzeichen bei Consolen-Ausgabe
#' @param output Abrage nach dem Ausgabeformat  which_output()
#' @return Vector
#' @name Text
#' @export
#' @examples
#' #library(stp25)
#' #Projekt("html")
#' Head("Ueberschrift") ## HR=2
#'
#' Text("hallo", "ich bin ein Test")
#' ###plot(sin, -pi, 2*pi,main="Sinus")
#' ###HTMLplot( Caption="Look at this curve!")
#' #End()
Text <- function(...,
                 style = 0,
                 char = "-",
                 output =  which_output()) {
  report_html <- function(msg) {
    if (is.null(msg))
      msg <- ""
    if (is.vector(msg)) {
      msg <- unlist(msg)  #-Zeilenumbrueche
      msg <- gsub("\\n", "<BR>", msg)
    } else
      stop("msg must be of type vector")
    if (style == 0 | style == "p"){
      HTML_P(msg)
      }
    else{
      
     # R2HTML::HTML.title(msg, HR = style)
      HTML_default(paste("\n <h", style, "> ", msg, "</h", style, ">\n", sep = "") )
      
      
      }
  }

  report_txt <- function(msg = NULL) {
    if (is.null(msg))
      msg <- ""
    if (is.vector(msg)) {
      msg <- unlist(msg)
      msg <- strsplit(msg, "\\n")  #-Zeilenumbr?che
      msg <- unlist(msg)
    } else {
      stop("msg must be of type vector")
    }

    char <- substr(char, 1, 1)
    underlined <- function(arg) {
      c(arg, paste(rep(char, max(nchar(
        msg
      ))), collapse = ""))
    }
    border <- function(arg) {
      n <- length(msg)
      ml <- max(nchar(msg))
      space <- paste(rep(" ", ml), collapse = "")
      line <- paste(rep(char, ml + 4), collapse = "")
      msg <-
        paste(msg, substr(rep(space, n), rep(1, n), ml - nchar(msg)), sep = "")
      c(line, paste(char, msg, char), line)
    }
    sfun <- list(underlined = underlined, border = border)
    if (is.numeric(style) &&
        length(style) == 1 && any(style == 1:length(sfun)))
      msg <-
      sfun[[style]](msg)
    else if (is.character(style) &&
             length(style) == 1 && !is.null(sfun[[style]]))
      msg <- sfun[[style]](msg)
    m <- matrix(msg, ncol = 1)
    colnames(m) <- ""
    rownames(m) <- rep("", length(msg))
    print.noquote(m)
  }



  if (output == "html")
    report_html(paste0(...))
  else if (output == "markdown")
    cat(...)
  else
    report_txt(paste0(...))
}

#' @rdname Text
#' @export
Head<- function( ...,
                 style=3,
                 char = "-"){
  Text(..., style = style, char = char)}

#' @rdname Text
#' @description  Anmerkung() ist ein blauer Text.
#' @export
Anmerkung <- function(...)
  HTML_default(
    paste('<p style="color: #0000FF"><b>Anmerkung:</b> <br>
          ', paste0(...), "</p><br>"))
#  Text('<div style="color:#0000FF"><b>Anmerkung:</b><br>', ..., "<br></div>")



#' @rdname Text
#' @description  Kunde() ist ein rot-brauner Text.
#' @export
Kunde <- function(x = "",
                  msg = NULL,
                  name = NULL) {
  if (is.null(name)) {
    name <- stringr::str_split(getwd(), "/"  , simplify = TRUE)
    name <- strtrim(gsub("[^::A-Z::]", "", name[length(name)]), 2)
  }
  
  if (is.null(msg))
    HTML_default(paste('<p style="color:#800000"> <b>', name,': </b> ',x,'</p>'))
  else
    HTML_default(paste('<p style="color:#800000"> <b>', name, x,': </b>', msg,'</p>'))
  
}





#' @rdname Text
#' @param lib Paketname
#' @description  CitationLib(car) gibt Pakete beschreibungen aus.
#' @export
CitationLib<-function(lib){
    x<- citation(lib)
    paste0(x$author[1],", (", x$year, "), ", x$title, ", URL ", x$url)
  }
 


#' @rdname Text
#' @description Zitat() ist eine Text  vom Type <blockquote>
#' @export
Zitat <- function(...)
  Text('<blockquote>', ..., "<br></blockquote>")


#' @rdname Text
#' @description Arbeitszeit() Tabelle zur Dokumentation der Arbeiszeit.
#' @param Lines in Arbeitszeit der Input-String
#' @param sep in Arbeitszeit das Input-String-Trennzeichen
#'
#' @export
Arbeitszeit<- function(Lines,
                       sep = "\\t"
) {
  zeit <- read.table(
    zz <- textConnection(gsub(sep, " ", Lines)),
    header = TRUE)
  close(zz)
  names(zeit) <- c("Datum",  "Start",   "Ende",   "Task")
  zeit$strt <- strptime(zeit$Start, "%H:%M")
  zeit$end <- strptime(zeit$Ende, "%H:%M")
  zeit$Zeit_Stunden<-  round(as.numeric(difftime(zeit$end,zeit$strt, units = "hours")), 2)
  zeit$Zeit_Summe <- cumsum(zeit$Zeit_Stunden)
  Output(zeit[, c("Datum", "Start", "Ende", "Task", "Zeit_Stunden", "Zeit_Summe" )])
  invisible(zeit)
}

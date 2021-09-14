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
#' 
#' 
#' 
#' 
#' 
Text <- function(x, ...) {
  UseMethod("Text")
}


#' @rdname Output
#'
#' @export
Text.default<- function(x, ...){
  
  print(class(x))
  # txt<-  psycho::analyze(fit)
  # text <- txt$text
  # paste(text, collapse ="\n ")
}

#' @rdname Output
#'
#' @export
Text.character <- function(...,
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
    if (style == 0 | style == "p") {
      HTML_P(msg)
    }
    else{
      HTML_default(paste("\n <h", style, "> ", msg, "</h", style, ">\n", sep = ""))
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
  
  msg <- paste0(...)
  msg <- gsub("#' ", "", msg)
  
  if (output == "html")
    report_html(msg)
  else if (output == "markdown" | output == "markdown_html") {
    if (style > 0) {
      cat(paste(rep("#", style), collapse = ""), msg)
    } else
      cat(msg)
  }
  else
    
    report_txt(msg)
}

#' @rdname Text
#' @export
Head<- function( ...,
                 style=3,
                 char = "-"){
  Text(..., style = style, char = char)
  
}


#' @rdname Text
#' @export
Head1<- function( ... ){
  pagebreak()
  Text(..., style = 1, char = "-")
}
#' @rdname Text
#' @export
Head2<- function( ... ){
  pagebreak()
  Text(..., style = 2, char = "-")
}
#' @rdname Text
#' @export
Head3<- function( ... ){
  Text(..., style = 3, char = "-")
}
#' @rdname Text
#' @export
Head4<- function( ... ){
  Text(..., style = 4, char = "-")
}
#' @rdname Text
#' @export
Head5<- function( ... ){
  Text(..., style = 5, char = "-")
}
#' @rdname Text
#' @description  Anmerkung() ist ein blauer Text.
#' @param prafix Anmerkung
#' @export
Anmerkung <- function(..., prafix = "Anmerkung"){
  HTML_default(
    paste0('<p style="color: #0000FF"><b>',prafix, ':</b> <br>
          ', paste0(...), "</p><br>"))
  }
#  Text('<div style="color:#0000FF"><b>Anmerkung:</b><br>', ..., "<br></div>")



#' @rdname Text
#' @description  Kunde() ist ein rot-brauner Text.
#' @export
#' @importFrom stringr str_split
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
#' @param ... namen der Librarys
#' @param style,.bibstyle an format
#' @param output html oder text
#' @return character
#' @export
#'
#' @examples
#' 
#' citation_library(base,Hmisc,car,lattice)
#' 
citation_library <- function(...,
                             style = "text",
                             .bibstyle = NULL,
                             output="text") {
  libs <-
    sapply(lazyeval::lazy_dots(...), function(x)
      as.character(x[1]))
  
  res <-  format(citation(), style, .bibstyle)
  
  for (lib in libs) {
    x <- citation(lib)
    y <- format(x, style, .bibstyle)
    #  res <- append(res,  "\n  ")
    res <- append(res, y)
  }
  
  if(output=="text")
    gsub("[<>]", "", paste(res, collapse = "\n\n"))
  else if(output=="html")
    paste("<p>", 
          paste( gsub("[<>]", "",res) , collapse = "</p> <p>"),
          "</p>")
  else res
  
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
Arbeitszeit <- function(Lines,
                        sep = "\\t",
                        ...) {
  zeit <- read.table(zz <- textConnection(gsub(sep, " ", Lines)),
                     header = TRUE)
  close(zz)
  
  names(zeit) <- c("Datum",  "Start",   "Ende",   "Task")
  zeit$strt <- strptime(zeit$Start, "%H:%M")
  zeit$end <- strptime(zeit$Ende, "%H:%M")
  zeit$Zeit_Stunden <-
    round(as.numeric(difftime(zeit$end, zeit$strt, units = "hours")), 2)
  zeit$Zeit_Summe <- cumsum(zeit$Zeit_Stunden)
  
  zeit$Pos <-  stringi::stri_extract_first_regex(zeit$Task, "[0-9]+")
  zeit$Task <- gsub("_", " ", zeit$Task)
  zeit$Task <- gsub("[0-9]+", "", zeit$Task)
  
  
  Output(zeit[, c("Datum",
                  "Start",
                  "Ende",
                  "Pos",
                  "Task",
                  "Zeit_Stunden",
                  "Zeit_Summe")], ...)
  invisible(zeit)
}

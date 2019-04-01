
#' citation_library
#'
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

 
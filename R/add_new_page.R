#' add_new_page
#'
#' Neue Seite bei copy and paste in Word.
#'
#' @return nichts
#' @export
#' 
add_new_page <- function() {
  if (which_output() == "html")
    code <- '<br style="page-break-before: always">'
  
  else
    code <- "\n----------------\n\n"
  HTML_default(code)
  
}

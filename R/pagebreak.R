#' pagebreak
#'
#' Neue Seite bei copy and paste in Word.
#'
#' @return nichts
#' @export
#' 
#' @examples 
#' 
#' # in MD-Files
#' # `r pagebreak()`
#' 
pagebreak <- function() {
  if(knitr::is_latex_output())
    return("\\newpage")
  else if(knitr::is_html_output())
    return('<div style="page-break-before: always;" />')
  else  if (which_output() == "html")
      HTML_default('<br style="page-break-before: always">') 
  else cat("\n\n***\n\n")
}

 

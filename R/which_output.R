#' which_output
#'
#' @return values are text, latex, html, markdown_html markdown, pandoc, and rst
#' @export
#'
#' @examples
#'
#' which_output()
#' 
#' 
which_output <- function() {
  in_opt <- options()$stp25$output
  in_formats <- c("text", "markdown", "md", "pandoc", "rst", "html", "docx", "word", "latex")
  if (is.null(in_opt)) in_opt <- ""
  in_formats <- in_formats[pmatch(in_opt, in_formats, dup = TRUE)]
  
  in_formats <- switch(in_formats,
                       md = "markdown",
                       word = "docx",
                       in_formats)
  
  if (is.na(in_formats)) {
    out <- knitr:::out_format()
    
    if (is.null(out)) {
      # Das hier ist mein eigenes Ausgabe-Format.
      if (options()$prompt[1] == "HTML> ")  {
        in_formats <-   "html"
      }  else {
        in_formats <-   "text"
      }
    } else {
      
      in_formats <- knitr:::pandoc_to()
      if(in_formats=="html")  in_formats <- "markdown_html"
      else if(in_formats=="beamer")  in_formats <- "latex"
      else if(in_formats=="gfm")  in_formats <- "markdown_html"   #github_document
      
      
    }
    
  }  
  
  in_formats[1]
 
  }
# 
# which_output <-
#   function() {
#     
# 
#   my_out<- options()$stp25$output
#   
#   if(is.logical(my_out)) return(my_out)
#   if( is.null( my_out )) my_out <- "" 
#     
#     if (my_out %in% c("text",  
#                       "pandoc", 
#                       "html",  
#                       "markdown", 
#                       "md", 
#                       "word", 
#                       "latex", 
#                       "docx")) {
#       my_out
#     }
#     else{    
#       out <- knitr:::out_format()
#       
#       if (is.null(out)) {
#         # Das hier ist mein eihenes Ausgabe-Format
#         if (my_out == "html" | options()$prompt[1] == "HTML> ")  {
#           "html"
#         }  else {
#           "text"
#         }
#       } else {
#         if (my_out == "") {
#           # Hier ist entweder Rmd oder spin oder Rpres
#           if (knitr::is_latex_output()) {
#             "markdown"
#           }
#           else  if (knitr::is_html_output()) {
#             
#             "markdown_html"
#           }
#           else if (knitr:::pandoc_to() == "docx") {
#             "word"
#           }
#           else {
#             "text"
#           }
#           #
#         } else{
#           if (knitr:::out_format() == "markdown") {
#             switch(
#               options()$stp25$output,
#               text =  "text",
#               pdf =   "markdown",
#               html =  "markdown_html",
#               word =  "word",
#               doc = "word",
#               #spin =  "markdown",
#               "markdown"
#             )
#           }
#         }
#         
#       }
#     }
#   }

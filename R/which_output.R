#' which_output
#'
#' @return html, text, markdown, word
#' @export
#'
#' @examples
#'
#' which_output()
which_output <-
  function() {
    if (is.null(knitr:::out_format())) {
      if (options()$prompt[1] == "HTML> " |
          options()$stp25$output == "html")  {
        "html"
      }  else {
        "text"
      }
    } else {
      if (knitr:::out_format() == "markdown") {
        switch(
          options()$stp25$output,
          text =  "text",
          pdf =   "markdown",
          html =  "markdown_html",
          word =  "word",
          spin =  "markdown",
                  "markdown"
        )
      }
      
    }
  }


# stp25output::which_output
# function() {
#   if (is.null(knitr:::out_format())) {
#     if (options()$prompt[1] == "HTML> " |
#         options()$stp25$output == "html")  {
#       "html"
#     }  else {
#       "text"
#     }
#   } else {
#     if (knitr:::out_format() == "markdown") {
#       if (options()$stp25$output == "spin") {
#         "html"
#       }   else if (options()$stp25$output == "html") {
#         "markdown_html"
#       }  else if (options()$stp25$output == "text") {
#         "text"
#       } else {
#         "markdown"
#       }
#     } else {
#       "text"
#     }
#   }
# }
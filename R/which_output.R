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
    out <- knitr:::out_format()
    my_out <-
      ifelse(is.null(options()$stp25$output), "", options()$stp25$output)
    
    if (is.null(out)) {
      # Das hier ist mein eihenes Ausgabe-Format
      if (my_out == "html" | options()$prompt[1] == "HTML> ")  {
        "html"
      }  else {
        "text"
      }
    } else {
      if (my_out == "") {
        # Hier ist entweder Rmd oder spin oder Rpres
        if (knitr::is_latex_output()) {
          "markdown"
        }
        else  if (knitr::is_html_output()) {
          "markdown_html"
        }
        else if(knitr:::pandoc_to()== "docx"){
          "word"
        }
        else {
          "text"
        }
        #
      } else{
        if (knitr:::out_format() == "markdown") {
          switch(
            options()$stp25$output,
            text =  "text",
            pdf =   "markdown",
            html =  "markdown_html",
            word =  "word",
            doc = "word",
            #spin =  "markdown",
            "markdown"
          )
        }
      }
      
    }
  }


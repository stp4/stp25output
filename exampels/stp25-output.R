
#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE, warnings=FALSE)
require(stpvers)


Projekt("html", "stp25")

get_my_options()$output
which_output()


#+ which_output
 Text("out_format")
HTML_(knitr:::out_format())
Text("is_latex_output")
HTML_(knitr::is_latex_output())
Text("is_html_output")
      HTML_(knitr::is_html_output())
      Text("pandoc_to")
            HTML_(knitr:::pandoc_to())
            Text("which_output")
                  HTML_(which_output())
 

#+ data, include = FALSE
dat <- data.frame(
  term = c("A", "B", "C", "D"),
  n = c(23, 14, 56, 2),
  m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA),
  stringsAsFactors = FALSE
)
DF2 <-data.frame(
  term = c("A", "B", "C", "D"),
  G1_k_n = c(23, 14, 56, 2),
  G1_k_m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA),
  G2_n = c(33, 35, 78, 21),
  G2_m = c("4.9 (2.7)", "4.7 (2.5)", "4.1 (5.6)", "4.2 (5.6)")
)





#' ## default
#' 
#' set_my_options(output=FALSE)
#' 
#+ default
dat %>% Output()
dat  %>% Output(output="html")
#dat  %>% Output(output="word")
dat  %>% Output(output="latex")
dat  %>% Output(output="text")
dat  %>% Output(output="pandoc")




End()
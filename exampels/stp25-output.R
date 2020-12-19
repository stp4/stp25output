

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE, warnings = FALSE)
require(stpvers)


Projekt("html", "stp25")

get_my_options()$output
which_output()




#+ data, include = FALSE
dat <- data.frame(
  term = c("A", "B", "C", "D"),
  n = c(23, 14, 56, 2),
  m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA),
  stringsAsFactors = FALSE
)
DF2 <- data.frame(
  term = c("A", "B", "C", "D"),
  G1_k_n = c(23, 14, 56, 2),
  G1_k_m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA),
  G2_n = c(33, 35, 78, 21),
  G2_m = c("4.9 (2.7)", "4.7 (2.5)", "4.1 (5.6)", "4.2 (5.6)")
)




dat %>% Output()



#' ## default
#'
#' set_my_options(output=FALSE)
#'
#+ default
DF2 %>% Output()
#+ plot-1




coplot(
  uptake ~ conc | Plant,
  data = CO2,
  show.given = FALSE,
  type = "b"
)
SavePlot(w = 5, h = 5)



coplot(
  uptake ~ conc | Plant,
  data = CO2,
  show.given = FALSE,
  type = "b"
)
SavePlot(w = 5, h = 5, res=72*2)
 
#  
# Width <- round(7 * 75)
# Height <- round(7 * 75)
# 
# dev.print(
#   device = png,
#   file =  "Fig/GraphFileName",
#   width = Width,
#   height = Height,
#   pointsize = 12,
#   bg =  "white",
#   res = 72  # res liefert unerwartete Ergebnisse 
# )


Stop()
dat  %>% Output(output = "html")
#dat  %>% Output(output="word")
dat  %>% Output(output = "latex")
dat  %>% Output(output = "text")
dat  %>% Output(output = "pandoc")



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

Head("Andere Packete")


#+
my_data <- head(iris)
names(my_data) <- c(letters[1:ncol(iris)])


Text("knitr::kable")
#+ results='asis'
library("knitr")
kable(my_data) %>% HTML_()

Text("xtable::xtable")
#+   results='asis'
library("xtable")
print(
  xtable(my_data),
  type = "html",
  include.rownames = FALSE,
  html.table.attributes = list("border='0' cellpadding='5' ")
) %>% HTML_()

Text("xtable::xtable")
#+  results = 'asis'
library(xtable)
print(xtable(my_data), type = 'html') %>% HTML_()

Text("xtable::xtable")
#+  results = 'asis'
library(xtable)
print(xtable(my_data),
      type = 'html',
      html.table.attributes = '') %>% HTML_()

Text("pander::pandoc.table")
#+  results='asis'
library("pander")
pandoc.table(my_data) %>% HTML_()

Text("pander::pandoc.table")
#+  results='asis'
library("pander")
pandoc.table(my_data, split.cells = 5) %>% HTML_()

Text("pander::pandoc.table")
#+  results = 'asis'
pander::panderOptions('table.split.table', 350)
pander::pandoc.table(my_data, style = "rmarkdown") %>% HTML_()


# #+
# library("ascii")
# print(ascii(my_data), type = 'pandoc')

Text("htmlTable::htmlTable")
#+
library("htmlTable")
htmlTable(my_data, col.rgroup = c("none", "#F7F7F7")) %>% HTML_()

Text("hwriter::hwriter")
#+  results='asis'
library(hwriter)
hwrite(my_data, border = 0) %>% HTML_()

Text("hwriter::hwriter")
#+  results='asis'
library(hwriter)
cat(
  hwrite(
    my_data,
    border = 0,
    center = TRUE,
    table.frame = 'void',
    width = '300px',
    table.style = 'padding: 50px',
    row.names = FALSE,
    row.style = list('font-weight:bold')
  )
) %>% HTML_()



End()
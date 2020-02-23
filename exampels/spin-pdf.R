#' ---
#' title: "Spin PDF"
#' author: "Me"
#' date: "2016-03-11"
#' output:
#'    pdf_document:
#'       toc: true
#' ---
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE, warnings=FALSE)
require(stpvers)
note<- "Auswertung für die Puplikationen in medizinische Fachzeitschriften."

#+ setup2
Projekt("", "spin Pdf")

#set_my_options(output="latex")
which_output()
get_my_options()$output

#' ## Text Output
#' 
#' Hallo Welt!
#' 

#+ text-asis, results='asis', echo=FALSE
Head("H3", style=3)
Text("
Auswertung für die Puplikationen in medizinische Fachzeitschriften.
Online-Befragung mit LimeSurvey.
Consulting-Services bei der Planung und Auswertung von statistischer Erhebungen.
Analyse von Labor und Messwerten aus Versuchsreihen.")


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



#+ default1, results='asis'

dat %>% Output(caption="default", note=note)
DF2 %>% Output(caption="default", note=note)

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

#' ## asis

#+ asis, results='asis'
dat %>% Output()
dat  %>% Output(output="html")
#dat  %>% Output(output="word")
dat  %>% Output(output="latex")
dat  %>% Output(output="text")
dat  %>% Output(output="pandoc")

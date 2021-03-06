#' ---
#' title: "Spin Word"
#' author: "Me"
#' output:
#'    word_document:
#'       toc: true
#' ---
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE, warnings=FALSE)
require(stpvers)
require(lattice)
require(effects)
note<- "Auswertung für die Puplikationen in medizinische Fachzeitschriften."

#+ setup2
Projekt("", "spin Word")

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


#+ plot-1

coplot(uptake ~ conc | Plant, data = CO2, show.given = FALSE, type = "b")
## fit the data for the first plant

 
head(CO2[Cs(uptake, conc, Plant)])

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


#+ APA-Table, results='asis'
fit1 <- glm(gruppe ~ lai, hkarz, family = binomial)
thkarz <- as.data.frame(xtabs(~ gruppe + lai, hkarz))
fit2 <- glm(Freq ~ gruppe * lai, thkarz, family = poisson())

APA_Table(fit1, include.odds = TRUE)



#+
pagebreak()

tab<-APA_Table(fit1, fit2, include.odds = TRUE, output=FALSE)
tab %>% Output()

Tbll_reg(fit1, fit2, include.odds = TRUE) %>% Output( )




#+ plot
plot(1)

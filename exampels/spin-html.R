#' ---
#' title: "Spin HTML"
#' author: "Me"
#' output:
#'    html_document:
#'       toc: true
#' always_allow_html: yes  
#' ---
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE, warnings=FALSE)
require(stpvers)
note<- "Auswertung für die Puplikationen in medizinische Fachzeitschriften."

#+ setup2, include=FALSE
Projekt("", "spin HTML")
#set_my_options(output="latex")
which_output()
get_my_options()$output



#+ text-asis, results='asis', echo=FALSE
Head("H3", style=3)
Text("
Auswertung für die Puplikationen in medizinische Fachzeitschriften.
Online-Befragung mit LimeSurvey.
Consulting-Services bei der Planung und Auswertung von statistischer Erhebungen.
Analyse von Labor und Messwerten aus Versuchsreihen.")

#+ include='FALSE'
require(wakefield)


set.seed(0815)
DF <- r_data_frame(
  n = 120,
  id,
  race,
  age(x = 20:45),
  sex, income,
  education(x = c("low", "med", "hig"),
            prob = c(.2,.45, .35 )),
  hour,
  iq,
  height=height_cm(mean = 50, sd = 10),
  died,
  Smoker = valid,
  m1=age(x = 1:10),
  m2=age(x = 1:10),
  m3=age(x = 1:10),
  m4=age(x = 1:10),
  m5=age(x = 1:10),
  m6=age(x = 1:10),
  date_stamp(prob = probs(12))
) %>% clean_names()

x<- mean(DF$income)
DF<- transform(DF,
               income =
                 round(
                   (income - x*as.numeric(sex) + 
                      x*as.numeric(education) + x*as.numeric(height)/10 + 1)/1000))
 


#+ Tbll-Corr-Table, results='asis'
DF %>%
  Tbll_corr( ~ m1 + m2 + m3 + m4 + m5 + m6) %>% Output()



stop()
#+ Tbll-Table, results='asis'
DF %>%
  Tbll_desc(sex, education,
            age[median], height[mean],
            smoker,
            income[0,mean]) %>%
  Output()

#+ Tbll-Table-gr, results='asis'
DF %>%
  Tbll_desc(sex,# education,
            age[median], height[mean],
            smoker,
            income[0,mean, anova],
            by= ~education,
            include.test = TRUE) %>%
  Output()

#+ Tbll-Table-lm, results='asis' 
Tbll(lm(income ~ sex + age +  education + height, DF),
     lm(income ~ sex + education + height, DF)) %>% Output()


#+ Tbll-Table-lm-2, results='asis' 
Tbll_reg(lm(income ~ sex + age +  education + height, DF),
     lm(income ~ sex + education + height, DF)) %>% Output()



#' ## Text Output
#' 
#' Hallo Welt!
#' 



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



#' ## Tabelle default
#' 
#' set_my_options(output=FALSE)
#' 
#+ default
dat %>% Output(  caption="default")
 dat  %>% Output(output="html", caption="html")
# #dat  %>% Output(output="word")
 dat  %>% Output(output="latex", caption="latex")
 dat  %>% Output(output="text", caption="text")
 dat  %>% Output(output="pandoc", caption="pandoc")
 dat  %>% Output(output="markdown", caption="markdown")
 dat  %>% Output(output="rst", caption="rst")
 
 
#' ## Tabelle asis

#+ asis, results='asis'
 dat %>% Output( caption="default")
 dat  %>% Output(output="html", caption="html")
 #dat  %>% Output(output="word")
 dat  %>% Output(output="latex", caption="latex")
 dat  %>% Output(output="text", caption="text")
 dat  %>% Output(output="pandoc", caption="pandoc")
 dat  %>% Output(output="markdown", caption="markdown")
 dat  %>% Output(output="rst", caption="rst")
 
 
 
 

#+ APA-Table, results='asis'
 fit1 <- glm(gruppe ~ lai, hkarz, family = binomial)
 thkarz <- as.data.frame(xtabs(~ gruppe + lai, hkarz))
 fit2 <- glm(Freq ~ gruppe * lai, thkarz, family = poisson())
 
 APA_Table(fit1, include.odds = TRUE)
 
 
 
 #+
 APA_Table(fit1, include.odds = TRUE)
 
#' Neues Projekt erstellen
#'
#' @description  Erstellt ein neues Projekt mir Ordnerstrucktur
#'  und R-Auswertungs-Files.
#' @param project Name des projektes
#' @param datum Datum
#' @param comment Beschreibung
#' @param path Pfad Dropbox/1_Projekte
#' @export
#' @examples
#' ## Not run:
#'
#'
#' #CreateProjekt( project= "001 Hans Meier",
#' #              datum= date(),
#' #              comment= "Auswertung mit Regressionsanalyse",
#' #              path="C:/Users/wpete/Dropbox/1_Projekte" )
#' ## End(**Not run**)

CreateProjekt <-function(project = "000 Dummy",
                         datum = date(),
                         comment = "Test Dummy",
                         path = "C:/Users/wpete/Dropbox/1_Projekte"
                         ){
  WD <- getwd()
  on.exit(setwd(WD))

  Rdata<- paste0(gsub("[^[:alpha:]]", "", Clean_Umlaute2(project)), ".Rdata")

  if(file.exists(paste0(path, "/", project))){
    cat(paste0("\"", paste0(path, "/", project), "\" already exists:\nDo you want to overwrite?\n\n"))
    ans <- menu(c("Yes", "No"))
    if (ans == "2") {stop("new_project aborted")
    }else {file.remove(paste0(path, "/", project))}
  }
  x <- suppressWarnings(invisible(
           folder(folder.name = paste0(path,"/", project))))
  setwd(x)
  "Processed data" <- "Raw scripts" <- "Raw data"  <-  Results <- R <-Docs <- Fig <- NULL
  invisible(folder("Processed data", "Raw data", Results, R, Docs, Fig))
  myswd<- paste0("setwd(\"", x,"\")")
 #---------------------------------------------------------------------------------


  cat( "#-- Eigene Functionen", file = "R/miscFun.r")
  cat( paste(project, datum, path, comment, sep="\n"),
       file = paste0(project, "(1).docx"))


  cat(
    paste0(
'
require(stp25vers)
require(tidyverse)

# set_my_options(prozent=list(digits=c(1,0), style=2))
  graphics.off()
  MySet()
'
,myswd,
'
Projekt("", "',project,'", "'      ,datum,'")

Arbeitszeit("
Datum  Start   Ende   Task
23-11  8:00    9:00   EinarbeiteninsThema
23-11  9:00    9:48  Auswerten           ")

Methode()
  Materials("Data laden und transformieren")
  Research_Design("Beschreibung des Studiendesigns (Experiment, Kohortenstudie, ...)")
  Measures("Fragebogen und Skalen (Reliabilitaetsanalyse)")
    # load("Processed data/', Rdata,'")
    # N <- nrow(DF)
Results()
  Demographic_Variables()
  Statistic("H1 Korrelation", file="(4) Analyse.R")
  Statistic("H2 Regressionsanalyse", file="(5) Analyse.R")
  Statistic("H3 Korrelation", file="(6) Analyse.R")
  Statistic("H4 Regressionsanalyse", file="(7) Analyse.R")
  Statistic("Weitere Befunde", file="(8) Analyse.R")

  #Anhang()
End()

'), file = "(0) Run All.R")

 cat(
      paste0(
'
#require(stp25vers)
#require(tidyverse)

# source("R/miscFun.r", echo=F)
# -- Load Data ---------------------------------------------
# data <- unzip("Raw data/auswertung.zip", exdir= Folder[1] )
# car::some(DF <- GetData("Raw data/File.R"))
# save(DF, file="Raw data/', Rdata,'")
# -- Tidy Data ---------------------------------------------
# DF %>% Drop_NA(key) %>%
#        mutate(jahr = factor(jahr)) %>%
#        Label(sex=Geschlecht)

#  save(DF, file="Processed data/', Rdata,'")

'), file = "(1) Get Data.R")


 cat(paste0(
   '
# load("Raw data/', Rdata,'")
# -- Tidy Data ---------------------------------------------
# DF %>% Drop_NA(key) %>%
   #        mutate(jahr = factor(jahr)) %>%
   #        Label(sex=Geschlecht)

# fit1<- Principal(DF[c( )], 4, cut=.35, sort=FALSE)
# fit1$Loadings %>% Output()
#
# DF$x1 <- Reliability2(DF[ ])$index
# DF$c2 <- Reliability2(DF ])$index
#   save(DF, file="Processed data/', Rdata,'")
   '), file = "(2) Measures.R")

 cat(paste0(
   '
# load("Processed data/', Rdata,'")
# APA2(~ sex + age, DF, caption="Beschreibung der Untersuchungsgruppe")
#
# DF %>% Tabelle2(sex, caption="Skalen")
'), file = "(3) Demographic.R")


cat(paste0(
'
# load("Processed data/', Rdata,'")
'), file = "(4) Analyse.R")

cat(paste0(
  '
# load("Processed data/', Rdata,'")
  '), file = "(5) Analyse.R")
cat(paste0(
  '
# load("Processed data/', Rdata,'")
  '), file = "(6) Analyse.R")
cat(paste0(
  '
# load("Processed data/', Rdata,'")
  '), file = "(7) Analyse.R")
cat(paste0(
  '
# load("Processed data/', Rdata,'")
  '), file = "(8) Analyse.R")


  cat("\nOk\n\n")
}


#-- Functions stolen from library(report)
scrubber <- function(text.var, rm.quote = TRUE, fix.comma = TRUE, ...){
        x <- reducer(Trim(clean(text.var)))
        if (rm.quote) {
            x  <- gsub('\"', "", x)
        }
        if (fix.comma) {
            x <- gsub(" ,", ",", x)
        }
        ncx <- nchar(x)
        x <- paste0(Trim(substring(x, 1, ncx - 1)), substring(x, ncx))
        x[is.na(text.var)] <- NA
        x
    }
#internal not exported
reducer <- function(x) gsub("\\s+", " ", x)
#internal not exported
Trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#internal not exported
unblanker <- function(x) subset(x, nchar(x)>0)
#internal not exported
clean <- function(text.var) sub("\\s+", " ", gsub("\r|\n|\t", " ", text.var))
mgsub <- function(pattern, replacement = NULL, text.var, fixed = TRUE, ...){
        key <- data.frame(pat=pattern, rep=replacement,
                          stringsAsFactors = FALSE)
        msubs <-function(K, x, ...){
            sapply(seq_len(nrow(K)), function(i){
                x <<- gsub(K[i, 1], K[i, 2], x, fixed = fixed, ...)
            }
            )
            return(gsub(" +", " ", x))
        }
        x <- Trim(msubs(K=key, x=text.var, ...))
        return(x)
    }

#' Create Folder
#'
#' \code{folder} - Create a folder/directory.
#' @param ... Namen der Folder
#' @param folder.name weis nicht  = NULL
#' @return \code{folder} creates a folder/directory.
#' @rdname file_handling
#' @export
folder <- function(..., folder.name = NULL) {
    if (!is.null(folder.name)) {
        x <- strsplit(folder.name, split = ", ")
    } else {
        x <- substitute(...())
    }
    if (!is.null(x)) {
        x <- unblanker(scrubber(unlist(lapply(x, function(y) {
            as.character(y)}))))
    }
    hfolder <- function(folder.name = NULL) {
        if (is.null(folder.name)) {
            FN <- mgsub(c(":", " "), c(".", "_"),
                        substr(Sys.time(), 1, 19))
        } else {
            FN <-folder.name
        }
        if (length(unlist(strsplit(FN, "/"))) == 1) {
            x <- paste(getwd(), "/", FN, sep = "")
        } else {
            x <- FN
        }
        dir.create(x)
        return(x)
    }
    if (is.null(x)) {
        hfolder()
    } else {
        if (length(x) == 1) {
            hfolder(x)
        } else {
            lapply(x, function(z) {
                hfolder(z)
            })
        }
    }
}



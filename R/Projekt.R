#' @rdname Projekt
#' @title Projekt Funktion
#' @name Projekt
#' @description 
#' Adaption der R2HTML-Funktionen. Und Setzen von Default Einstellungen wie Contraste und set.seed(0815).
#' 
#' Bei ausführen von library(stp25output) werden die Optionen mit default_stp25_opt() gesetzt.
#' 
#' Projekt() Initialisiere ein neues Projekt, und erstellt dabei die Folder Results und Fig 
#' mit End() wird der Report aufgerufen und ein Reset der Einstellungen durchgeführt. Ueber die Funktionen
#' stp25_options werden Formatierungsoptionen gesetzt.
#' \subsection{Projekt und HTML}{
#' Die Funktion \code{Projekt}  ist essentiell fuer alle weiteren Funktionen es werden dabei
#' Variablen fuer die Darstellung gesetzt wie ZB Nachkommastellen
#' \code{digits.prozent = 0, digits.mean = 2, digits.r = 2,  digits.p = 3, digits.Fstat = 2}
#' Trennzeichen \code{OutDec = "."}. Das Aussehen der Grafiken (lattice) wird
#' ueber \code{grafik = "ggplot2like"} gesteuert #' bei \code{grafik = 1} wird die normale
#' Einstellung verwendet. Unter \code{options()$stp25}
#' findet man die Liste mit allen Einstellungen.
#' Die HTML Ausgave wur ?ber \code{Projekt("html")} gesteuert und durch \code{End()} wird die Seite geladen.
#' }
#' \subsection{Tabellen}{
#' APA -Style Tabellen erstellten (HTML oder Console) \code{APA2}.
#' }
#' \subsection{Text}{
#' APA -Style Daten ausgabe als Text kann mit (HTML oder Console)
#'  \code{Result} erstellten.
#' }
#' \subsection{Daten}{
#'  Unterschiedliche Daten-Formate einlesen (SPss, csv usw) mit
#'  \code{GetData}. Schreiben lassen sich die
#'  Daten (auch Grafiken) mit \code{SaveDate}
#'  }
#' \subsection{Messinstrumente}{
#' Fuer spezielle Messinstrumente wie \code{Kano}, \code{Likert},
#' oder \code{Rangreihe} stehen eigene Funktionen zur verfuegung.
#' }
#' \subsection{Aggregieren}{
#' Hilfsfunktionen zum Aggregieren finden sich unter \code{dapply2},
#' eine \code{apply()} Variante.
#' und \code{Melt2} eine Kombination aus \code{melt} und \code{dcast}
#' aus dem Packet \code{reshape2}.
#' Melt2 wird dabei wie APA2 verwendet also zB
#' \code{APA2(Produkt + Online + Preis ~ Group, DF, fun=mean2)}ist
#' fuer APA-Tabelle und wenn die Mittelwerte angefordert werden
#' sollen \code{Melt2(Produkt + Online + Preis ~ Group, DF, fun = mean2)}
#'  die Syntax. Summenindex kann mit \code{Index} oder das Umkodieren
#'  von Data.frames durch \code{Umcodieren}
#' erfolgen. Normskalen wie T-Werte lassen sich ueber \code{Norms} berechnen.
#' }
#' \subsection{Berechnen}{
#' Mittelwerte mit \code{mean2} Formatierte Masszahlen fuer
#' die \code{APA2.formula(~a+d, data, fun=Prozent)} zu finden
#' unter \code{Prozent}
#' }
#' @param myformat  HTML oder Knit oder Console
#' @param Projektname Bezeichnung des Projektes (gilt auch fuer die HTML Seite)
#' @param datum  Datum zur Dokumentation
#' @param fig_folder  Folder wenn ein ander Ort gewuenscht
#' @param html_folder Folderwenn ein ander Ort gewuenscht
#' @param OutDec Komma oder Punkt
#' @param contrasts default wie SPSS
#' @param html_name  intern nicht aendern
#' @param ... weitere Objekte nicht benutzt
#' @return HTML oder Text Output
#' @export
#' @examples
#' # require(stpvers)
#' # Projekt()
#' # APA2(~. , hkarz)
#' set_my_options(prozent=list(digits=c(1,0), style=2))
#' get_my_options()$apa.style$prozent
#'
#' set_my_options(mittelwert=list(digits=c(1,0), plusmin_sign=TRUE))
#' get_my_options()$apa.style$mittelwert
#'
#' # APA2(~. , hkarz)
#'
#' # End()
Projekt <- function (myformat = "",
                    Projektname = "Demo",
                    datum = date(),

                    fig_folder = "Fig",
                    html_folder = "Results",

                    OutDec = NULL,
                    contrasts =  c("contr.Treatment", "contr.poly"), ## SPss
                    html_name = stpvers::Clean_Umlaute2(Projektname),
                    ...)
{
  set.seed(0815)
  if (Projektname=="Demo")
      setwd("C:/Users/wpete/Dropbox/3_Forschung/R-Project")
  #-- Fehler Abfangen
  if (options()$prompt[1] == "HTML> ") {
        options(prompt = "> ")
  }

  myformat <- tolower(myformat)
  myDir <- getwd()
  myURL <-
    paste("file://", myDir,
          "/", html_folder,
          "/", html_name,
          ".",myformat, sep = "")
  output.dir <- paste(myDir, html_folder, sep = "/")
  if(is.null(OutDec)) OutDec <- options()$OutDec
  else options(OutDec = OutDec)

  options(R2HTML.format.decimal.mark = OutDec)
 # opar <<- lattice::trellis.par.get()

  if (!any(list.dirs() == paste("./", html_folder, sep = "")))
    dir.create(
      output.dir,
      showWarnings = TRUE,
      recursive = FALSE,
      mode = "0777"
    )


  if (!(paste0("./", fig_folder) %in% list.dirs() |
      paste0("./", tolower(fig_folder)) %in% list.dirs())) {
        dir.create(
          fig_folder,
          showWarnings = TRUE,
          recursive = FALSE,
          mode = "0777"
        )
    cat("\nFolder ",fig_folder, " wurde erstellt.\n")
  }


  if(fig_folder!="Fig")  set_my_options(fig_folder=paste0(fig_folder, "/"))
  cat("\nSpeichere Abbildungen in ",options()$stp25$fig_folder, "\n")

  if (!is.null(contrasts)) {
    oldc <- getOption("contrasts")
    options(contrasts = contrasts)
    cat(
      "\nKontraste von " ,
      paste(oldc, collapse = ", "),
      "auf ",
      paste(contrasts, collapse = ", "),
      " umgestellt!\n"
    )
  }

  set_default_params(list(Tab_Index = 0, Abb_Index = 0))

  if (myformat == "html") {
    set_default_params(list(
     # param.XLConnect = NULL,
      file.name.index = 0,
      reset = par(no.readonly = TRUE)
    ))

    R2HTML::HTMLStart(
      outdir = output.dir,
      file = html_name,
      extension = myformat,
      echo = FALSE,
      HTMLframe = FALSE
    )

    cat( "\n",output.dir, html_name , myformat, "\n")
   # R2HTML::HTMLChangeCss("gridR2HTML")
    #    options(prompt="HTML> ")
   # set_default_params(list(myURL = myURL))
  } else{
    options(continue = "  ")
  }
  Text(Projektname, "\n Datum: ", datum,
       ", Software: ", R.version.string ,
       ", Link: www.R-project.org/\nFile: ",
       get_scriptpath())


  invisible(NULL)
}




#' Ausfuehrendes File finden
#'
#' In Projekt verwendet um den Namen des Files zu dokumentieren https://stackoverflow.com/questions/18000708/find-location-of-current-r-file
#' @return String
#'
get_scriptpath <- function() {

  # location of script can depend on how it was invoked:
  # source() and knit() put it in sys.calls()
  path <- NULL

  if(!is.null(sys.calls())) {
    # get name of script - hope this is consisitent!
    path <- as.character(sys.call(1))[2]

    # make sure we got a file that ends in .R, .Rmd or .Rnw
    if (grepl("..+\\.[R|Rmd|Rnw]", path, perl=TRUE, ignore.case = TRUE) )  {

      path<-strsplit(path, "/")[[1]]

      return(path[length(path)])
    } else {
      message("Obtained value for path does not end with .R, .Rmd or .Rnw: ", path)
    }
  } else{
    # Rscript and R -f put it in commandArgs
    args <- commandArgs(trailingOnly = FALSE)
  }
  return(path)

  #
}


#' @rdname Projekt
#' @description Methode, Materials, Research_Design, Measures
#' Results, Demographic_Variables und Statistic sind Platzhalter Funktionen um den R-Code in verschiedenen Files aiszulagern.
#' @param x Character
#' @param file auszufuerendes File
#' @export
Methode <- function(x=NULL, file=NULL){
  Head("Methoden", style=2)
  if(!is.null(x)) Text(x)
  if(!is.null(file)) source(file, encoding = "UTF-8") # or "latin1"
}
#' @rdname Projekt
#' @export
Materials <- function(x=NULL,
                      file="(1) Get Data.R"){
  Head("Materialien", style=3)
  if(!is.null(x)) Text(x)
  if(!is.null(file)) source(file, encoding = "UTF-8")
}


#' @rdname Projekt
#' @export
Research_Design <- function(x=NULL, file=NULL){
  Head("Forschungs Design", style=3)
  if(!is.null(x)) Text(x)
  if(!is.null(file)) source(file, encoding = "UTF-8")
}

#' @rdname Projekt
#' @export
Measures <- function(x=NULL,
                     file="(2) Measures.R"){
  Head("Messinstrument", style=3)
  if(!is.null(x)) Text(x)
  if(!is.null(file)) source(file, encoding = "UTF-8")
}
#' @rdname Projekt
#' @export
Results<- function(x=NULL, file=NULL){
  Head("Ergebnisse", style=2)
  if(!is.null(file)) source(file, encoding = "UTF-8")
}

#' @rdname Projekt
#' @export
Demographic_Variables<- function(x=NULL,
                                 file="(3) Demographic.R" ){
  Head("Demographische Variablen", style=3)
  if(!is.null(x)) Text(x)
  if(!is.null(file)) source(file, encoding = "UTF-8")
}
#' @rdname Projekt
#' @export
Statistic<- function(x="Resultate", file="(4) Analyse.R"){
  Head(x, style=3)
  if(!is.null(x)) Text(x)
  if(!is.null(file)) source(file, encoding = "UTF-8")
}







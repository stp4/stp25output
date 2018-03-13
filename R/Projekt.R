#' Start eines neuen Prpjekts
#' 
#' Mit \code{Projekt()} werden die default Einstellungen (Kontraste) gesetzt sowie die Folder erstellt.
#' Weiters wird die Ausgabe als HTML gesteuert.
#' Weitere Optionen fuer die Formatierung koennen mit \code{default_stp25_opt()} gesetzt. 
#' Mit  \code{stp25_options} koennen die  Formatierungs-Optionen geaendert werden.
#' @param myformat  HTML oder Knit oder Console
#' @param Projektname Bezeichnung des Projektes (gilt auch fuer die HTML Seite)
#' @param datum  Datum zur Dokumentation
#' @param fig_folder,html_folder Folder wenn ein ander Ort gewuenscht
#' @param OutDec Komma oder Punkt
#' @param contrasts default wie SPSS
#' @param html_name  intern nicht aendern
#' @param ... weitere Objekte nicht benutzt
#' @return HTML oder Text Output
#' @name Projekt
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' require(stpvers)
#'  
#'  Projekt()
#'  
#'  APA2(~. , hkarz)
#'  
#'  
#' set_my_options(prozent=list(digits=c(1,0), style=2))
#' get_my_options()$apa.style$prozent
#'
#' set_my_options(mittelwert=list(digits=c(1,0), plusmin_sign=TRUE))
#' get_my_options()$apa.style$mittelwert
#'
#' #APA2(~. , hkarz)
#'
#'  End()
#' 
#' }
#' 
Projekt <- function (myformat = "",
                     Projektname = "Demo",
                     datum = date(),
                     fig_folder = "Fig",
                     html_folder = "Results",
                     OutDec = NULL,
                     contrasts =  c("contr.Treatment", "contr.poly"),
                     html_name = stpvers::Clean_Umlaute2(Projektname),
                     ...)
{
  set.seed(0815)
  if (Projektname == "Demo")
    setwd("C:/Users/wpete/Dropbox/3_Forschung/R-Project")
  #-- Fehler Abfangen
  if (options()$prompt[1] == "HTML> ") {
    options(prompt = "> ")
  }
  
  myformat <- tolower(myformat)
  myDir <- getwd()
  myURL <-
    paste("file://",
          myDir,
          "/",
          html_folder,
          "/",
          html_name,
          ".",
          myformat,
          sep = "")
  output.dir <- paste(myDir, html_folder, sep = "/")
  
  if (is.null(OutDec))
    OutDec <- options()$OutDec
  else
    options(OutDec = OutDec)
  
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
    cat("\nFolder ", fig_folder, " wurde erstellt.\n")
  }
  
  
  if (fig_folder != "Fig")
    set_my_options(fig_folder = paste0(fig_folder, "/"))
  
  cat("\nSpeichere Abbildungen in ",
      options()$stp25$fig_folder,
      "\n")
  
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
    
    HTMLStart(
      outdir = output.dir,
      file = html_name,
      extension = myformat,
      echo = FALSE,
      HTMLframe = FALSE
    )
    
    cat("\n", output.dir, html_name , myformat, "\n")
    # R2HTML::HTMLChangeCss("gridR2HTML")
    #    options(prompt="HTML> ")
    # set_default_params(list(myURL = myURL))
  } else{
    options(continue = "  ")
  }
  
  
  pr_string <- paste(
    Projektname,
    "\n Datum: ",
    datum,
    ", Software: ",
    R.version.string ,
    ", Link: www.R-project.org/\nFile: ",
    get_scriptpath()
  )
  
  Text(pr_string)
  
  
  invisible(pr_string)
}


#' @rdname Projekt
#' @description  \subsection{End}{
#' Zuruecksetzen der Einstellungen und Aufruf des Browsers browser = getOption("browser")}
#' @param anhang Ja/Nein
#' @param browser Ie oder Chrome
#' @param output TRUE oder FALSE
#' @export
End <- function(anhang=FALSE,
                browser = "C:/Program Files (x86)/Internet Explorer/iexplore.exe",
                output = options()$prompt[1] == "HTML> ",
                ...) {
  options(contrasts = c("contr.treatment", "contr.poly"))
  #- Reset
  stp25_options()
  if( exists( "opar" )) lattice::trellis.par.set(opar)
  if( exists( "oopt" )) lattice::lattice.options(oopt)
  if( exists( "Tab_Index" )) Tab_Index <<-  0
  if( exists( "Abb_Index" )) Abb_Index <<-  0
  
  file<- try( HTMLGetFile(), silent = TRUE)
  if(output & class(file) !=  "try-error") {
    if (anhang){ Anhang() }
    
    HTMLStop()
    #   print(tmp)
    # getOption("browser")
    browseURL(file, browser=browser)
  } #else {cat("Die Funktion Projekt() mus vorher ausgefuert werden.",  "\n\n")}
  
  
  options(prompt="> ")
  
  if(output) file
  else cat("\nReset Kontraste\n")
}

#' @rdname Projekt
#' @description \subsection{Methode}{
#' Methode, Materials, Research_Design, Measures
#' Results, Demographic_Variables und Statistic sind Platzhalter Funktionen um den R-Code 
#' in verschiedenen Files aiszulagern.}
#' @param x Character
#' @param file auszufuerendes File
#' @export
Methode <- function(x=NULL, file=NULL){
  Head("Methoden", style=3)

  if(!is.null(x)) Text(x)
  if(!is.null(file)){
    HTML_I(paste(file, file.info(file)$ctime))
    source(file, encoding = "UTF-8") # or "latin1"
    }
}

#' @rdname Projekt
#' @export
Materials <- function(x=NULL,
                      file="(1) Get Data.R"){
  Head("Materialien", style=3)
 # HTML_I(paste(file, file.info(file)$ctime))
  if(!is.null(x)) Text(x)
  if(!is.null(file)){
    HTML_I(paste(file, file.info(file)$ctime))
    source(file, encoding = "UTF-8") # or "latin1"
  }
}

#' @rdname Projekt
#' @export
Research_Design <- function(x=NULL, file=NULL){
  Head("Forschungs Design", style=3)
 # HTML_I(paste(file, file.info(file)$ctime))
  if(!is.null(x)) Text(x)
  if(!is.null(file)){
    HTML_I(paste(file, file.info(file)$ctime))
    source(file, encoding = "UTF-8") # or "latin1"
  }
}

#' @rdname Projekt
#' @export
Measures <- function(x=NULL,
                     file="(2) Measures.R"){
  Head("Messinstrument", style=3)
 # HTML_I(paste(file, file.info(file)$ctime))
  if(!is.null(x)) Text(x)
  if(!is.null(file)){
    HTML_I(paste(file, file.info(file)$ctime))
    source(file, encoding = "UTF-8") # or "latin1"
  }
}

#' @rdname Projekt
#' @export
Results<- function(x=NULL, file=NULL){
  Head("Ergebnisse", style=2)
#  HTML_I(paste(file, file.info(file)$ctime))
  if(!is.null(file)){
    HTML_I(paste(file, file.info(file)$ctime))
    source(file, encoding = "UTF-8") # or "latin1"
  }
}

#' @rdname Projekt
#' @export
Demographic_Variables<- function(x=NULL,
                                 file="(3) Demographic.R" ){
  Head("Demographische Variablen", style=3)
 # HTML_I(paste(file, file.info(file)$ctime))
  if(!is.null(x)) Text(x)
  if(!is.null(file)){
    HTML_I(paste(file, file.info(file)$ctime))
    source(file, encoding = "UTF-8") # or "latin1"
  }
}

#' @rdname Projekt
#' @export
Statistic<- function(x="Resultate", file="(4) Analyse.R"){
  
  Head(x, style=3)
 # HTML_I(paste(file, file.info(file)$ctime))
  if(!is.null(x)) Text(x)
  if(!is.null(file)){
    HTML_I(paste(file, file.info(file)$ctime))
    source(file, encoding = "UTF-8") # or "latin1"
  }
}

#' @rdname Projekt
#' @description  \subsection{Interne Funktion}{
#' \code{get_scriptpath()} Ausfuehrendes File finden 
#' Quelle: https://stackoverflow.com/questions/18000708/find-location-of-current-r-file}
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


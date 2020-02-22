#' Projekt
#'
#' Ein R-File ruft die Interne Funktion Projekt_html() auf und ein Rmd-File Projekt_Rmd().
#' 
#' 
#' Start eines neuen Prpjekts, mit \code{Projekt()} werden die default Einstellungen (Kontraste) gesetzt sowie die Folder erstellt.
#' Weiters wird die Ausgabe als HTML gesteuert.
#' Weitere Optionen fuer die Formatierung koennen mit \code{default_stp25_opt()} gesetzt.
#' Mit  \code{stp25_options} koennen die  Formatierungs-Optionen geaendert werden.
#' @param myformat HTML, Spin, Knit, Rpres oder Text. Spin ist knitr wobei die Ausgabe der Tabellen mit html erfolgt
#' @param Projektname Bezeichnung des Projektes (gilt auch fuer die HTML Seite)
#' @param datum Datum zur Dokumentation
#' @param fig_folder,html_folder Folder bei html wenn ein ander Ort gewuenscht
#' @param OutDec  Komma oder Punkt
#' @param contrasts default wie SPSS
#' @param css css Eigense Format
#' @param ... weitere Objekte nicht benutzt
#' @name Projekt
#'
#' @return html-Pfad oder Projektname als Text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # require(stpvers)
#'
#'  Projekt()
#'      # R2HTML::HTMLChangeCss("gridR2HTML")
#'      # options(prompt="HTML> ")
#'      #  set_default_params(list(myURL = myURL))
#'
#'  APA2(~. , hkarz)
#'
#'
#' #-- default options
#' set_my_options()
#' get_my_options()$apa.style$prozent
#'
#' #-- Speicherort aendern
#' get_my_options()$fig_folder
#' set_my_options(fig_folder="Fig2/")
#' get_my_options()$fig_folder
#'
#' #-- Format aendern
#' set_my_options(prozent=list(digits=c(1,0), style=2))
#'
#' set_my_options(mittelwert=list(digits=c(1,0), plusmin_sign=TRUE))
#'
#'
#' #  APA2(~. , hkarz)
#' ###Names2Language(c("Item", "Characteristics", "Statistics"  ), "de")
#'
#'
#' # options()$stp25$bez$f.value
#'  ### Names2Language(c("Pr..Chisq.", "F.value"))
#'
#'
#' #  hkarz2 <-Label(mutate(hkarz,
#' #  Lai=factor(lai),
#' #  x=NA),
#' #  Lai="1-Smoking Habits",
#' #  x =   "2-Supplement Intake",
#' #  tzell = " - to boost your performance" ) 
#' 
#' #  Tabelle2(  ~Lai+x[header]+tzell,  hkarz2  , APA=TRUE )
#' #  Tabelle2(  Lai+x[header]+tzell~gruppe, hkarz2 , test=TRUE, APA=TRUE, include.total=TRUE)
#' #  set_my_options(mittelwert=list(include_name=FALSE))
#' #  Tabelle2(  Lai+x[header]+tzell[median]~gruppe, hkarz2 , test=TRUE, APA=TRUE, include.total=TRUE)
#'
#' #APA2(~. , hkarz)
#'
#'  End()
#'
#' }
#'
Projekt <- function(myformat = "",
                    Projektname = "Demo",
                    datum = date(),
                    fig_folder = "Fig",
                    html_folder = "Results",
                    OutDec = NULL,
                    contrasts =  c("contr.Treatment", "contr.poly"),
                    css = TRUE,
                    ...) {
  set.seed(0815)
  path <- "test.R"
  
  if (!is.null(sys.calls())) {
    path <- tolower(as.character(sys.call(1))[2])
  }
  
  if(is.null(myformat)) {
    path <- "test.txt"
    myformat <- "text"
  }else{ myformat <- tolower(myformat)}
  
  is_r_file <- grepl("\\.r$", path, perl = TRUE, ignore.case = TRUE)
  is_not_knit <- which_output() != "markdown"
  
  
 # cat("\n\n", is_r_file, is_not_knit, "\n\n")
  
  if (is_r_file & is_not_knit) {
    if (myformat == "html") {
      HTML_Start(
        Projektname = Projektname,
        datum = datum,
        fig_folder = fig_folder,
        html_folder = html_folder,
        OutDec = OutDec,
        contrasts = contrasts
      )
    }
    else{
        projekt_settings(contrasts, withprompt="> ", OutDec)
    }

  } else   {
    Projekt_Rmd(
      myformat = myformat,
      Projektname = Projektname,
      datum = datum,
      OutDec = OutDec,
      contrasts =  contrasts
    )
  }
}







#' @rdname Projekt
#' @description Einstellungen fuer .Rmd files hier werden keine Folder erstellt
Projekt_Rmd <- function (myformat,
                                Projektname,
                                datum,
                                OutDec,
                                contrasts)
{
  #cat("\Projekt_Rmd\n")
  if (is.null(OutDec))
    OutDec <- options()$OutDec
  else
    options(OutDec = OutDec)
  
  if (!is.null(contrasts)) {
    oldc <- getOption("contrasts")
    options(contrasts = contrasts)
    cat(
      "\nKontraste von " ,
      paste(oldc, collapse = ", "),
      "auf\n",
      paste(contrasts, collapse = ", "),
      " umgestellt!\n"
    )
  }
  
  set_default_params(list(Tab_Index = 0, Abb_Index = 0))
  set_my_options(output = myformat)
  
  invisible(
    paste(
      Projektname,
      "\n Datum: ",
      datum,
      ", Software: ",
      R.version.string ,
      ", Link: www.R-project.org/\nFile: ",
      get_scriptpath()
    )
  )
}





#' @rdname Projekt
#' @description  \subsection{End}{
#' Zuruecksetzen der Einstellungen und Aufruf des Browsers browser = getOption("browser")}
#' @param anhang Ja/Nein
#' @param browser Ie oder Chrome
#' @param output TRUE oder FALSE
#' @export
End <- function(anhang = FALSE,
                browser = "C:/Program Files (x86)/Internet Explorer/iexplore.exe",
                output = options()$prompt[1] == "HTML> ",
                ...) {
  if (options()$prompt[1] == "HTML> ") {
    HTML_End()
  } else{
    options(contrasts = c("contr.treatment", "contr.poly"))
    #- Reset
    stp25_options()
    if (exists("opar"))
      lattice::trellis.par.set(opar)
    if (exists("oopt"))
      lattice::lattice.options(oopt)
    if (exists("Tab_Index"))
      Tab_Index <<-  0
    if (exists("Abb_Index"))
      Abb_Index <<-  0
    
    file <- try(HTMLGetFile(), silent = TRUE)
    if (output & class(file) !=  "try-error") {
      if (anhang) {
        Anhang()
      }
      
      R2HTML::HTMLStop()
      #   print(tmp)
      # getOption("browser")
      browseURL(file, browser = browser)
    } #else {cat("Die Funktion Projekt() mus vorher ausgefuert werden.",  "\n\n")}
    
    
    options(prompt = "> ")
    
    if (output)
      file
    else
      cat("\nReset Kontraste\n")
  }
}


#' @rdname Projekt
#' @description Rechnung(): setzt ein Datum und Beendet mit Ende und stop die Auswertung
#' @export
Rechnung  <- function(datum = "") {
  Text(paste("Rechnung: ", datum))
  End()
  stop()
  NULL
}

#' @rdname Projekt
#' @description \subsection{Methode}{
#' Methode, Materials, Research_Design, Measures
#' Results, Demographic_Variables und Statistic sind Platzhalter Funktionen um den R-Code
#' in verschiedenen Files aiszulagern.}
#'
#' @param x Character
#' @param h Ueberschrift
#' @param file auszufuerendes File
#' @export
Methode <- function(h = "Methoden",
                    x = NULL,
                    file = NULL) {
  HTML_BR()
  if (!is.null(h))
    Head(h, style = 1)
  if (!is.null(file)) {
    HTML_I(paste(file, file.info(file)$mtime))
    
  }
  if (!is.null(x))
    Text(x)
  if (!is.null(file)) {
    source(file, encoding = "UTF-8")
  }
}

#' @rdname Projekt
#' @export
Materials <- function(h = "Materialien",
                      x = NULL,
                      file = "(1) Get Data.R") {
  HTML_BR()
  if (!is.null(h))
    Head(h, style = 2)
  if (!is.null(file)) {
    HTML_I(paste(file, file.info(file)$mtime))
    
  }
  if (!is.null(x))
    Text(x)
  if (!is.null(file)) {
    source(file, encoding = "UTF-8")
  }
}

#' @rdname Projekt
#' @export
Research_Design <-
  function(h = "Forschungs Design",
           x = NULL,
           file = NULL) {
    HTML_BR()
    if (!is.null(h))
      Head(h, style = 2)
    if (!is.null(file)) {
      HTML_I(paste(file, file.info(file)$mtime))
      
    }
    if (!is.null(x))
      Text(x)
    if (!is.null(file)) {
      source(file, encoding = "UTF-8") # or "latin1"
    }
  }

#' @rdname Projekt
#' @export
Measures <- function(h = "Messinstrument",
                     x = NULL,
                     file = "(2) Measures.R") {
  HTML_BR()
  if (!is.null(h))
    Head(h, style = 2)
  if (!is.null(file)) {
    HTML_I(paste(file, file.info(file)$mtime))
    
  }
  if (!is.null(x))
    Text(x)
  if (!is.null(file)) {
    source(file, encoding = "UTF-8") # or "latin1"
  }
}

#' @rdname Projekt
#' @description Results(), Measures(), Materials(), usw Ueberschrift mit aufruf eies exteren R-Scripts.
#' @export
Results <- function(h = "Ergebnisse",
                    x = NULL,
                    file = NULL) {
  HTML_BR()
  HTML_HR()
  if (!is.null(h))
    Head(h, style = 1)
  if (!is.null(file)) {
    HTML_I(paste(file, file.info(file)$mtime))
  }
  
  if (!is.null(x))
    Text(x)
  if (!is.null(file)) {
    source(file, encoding = "UTF-8") # or "latin1"
  }
}

#' @rdname Projekt
#' @export
Demographic_Variables <- function(h = "Deskriptive Analyse",
                                  x = NULL,
                                  file = "(3) Demographic.R") {
  HTML_BR()
  if (!is.null(h))
    Head(h, style = 2)
  if (!is.null(file)) {
    HTML_I(paste(file, file.info(file)$mtime))
  }
  
  if (!is.null(x))
    Text(x)
  
  if (!is.null(file)) {
    source(file, encoding = "UTF-8") # or "latin1"
  }
}

#' @rdname Projekt
#' @export
Statistic <-
  function(h = "Resultate",
           x = NULL,
           file = "(4) Analyse.R") {
    HTML_BR()
    if (!is.null(h))
      Head(h, style = 2)
    
    if (!is.null(file)) {
      HTML_I(paste(file, file.info(file)$mtime))
    }
    # HTML_I(paste(file, file.info(file)$mtime))
    if (!is.null(x))
      Text(x)
    if (!is.null(file)) {
      source(file, encoding = "UTF-8") # or "latin1"
    }
  }



#' @rdname Projekt
#' @description  \subsection{Interne Funktion}{
#' \code{get_scriptpath()} Ausfuehrendes File finden
#' Quelle: https://stackoverflow.com/questions/18000708/find-location-of-current-r-file}
#' 
get_scriptpath <- function() {
  # location of script can depend on how it was invoked:
  # source() and knit() put it in sys.calls()
  path <- NULL
  
  if (!is.null(sys.calls())) {
    # get name of script - hope this is consisitent!
    path <- as.character(sys.call(1))[2]
    
    # make sure we got a file that ends in .R, .Rmd or .Rnw
    #Achtung grep sit falssch!!!!
    
    if (grepl("..+\\.[R|Rmd|Rnw]",
              path,
              perl = TRUE,
              ignore.case = TRUE))  {
      path <- strsplit(path, "/")[[1]]
      
      return(path[length(path)])
    } else {
    #  message("Obtained value for path does not end with .R, .Rmd or .Rnw: ",
     #         path)
    }
  } else{
    # Rscript and R -f put it in commandArgs
    args <- commandArgs(trailingOnly = FALSE)
  }
  return(path)
}





#' cleansing_umlaute cleansing
#' 
#' Data cleansing 
#' 
#' cleansing_umlaute(): Funktion entfernt stoerende Umlaute,
#' Funktion entfernt stoerende Umlaute, unten stehende Liste ggf. erweitern 
#' sprintf("%X", as.integer(charToRaw("Ae")))
#' @param x string
#' @export
#' 
cleansing_umlaute <- function(x){
  x <- gsub("\u00e4","ae", x)
  x <- gsub("\u00fc","ue", x)
  x <- gsub("\u00f6","oe", x)
  x <- gsub("\u00dc","Ue", x)
  x <- gsub("\u00c4","Ae", x)
  x <- gsub("\u00d6","Oe", x)
  x <- gsub("\u00df","ss", x)
  x <- gsub(" ", "_", x)
  x
}




 
#' @rdname cleansing_umlaute
#' @description Sonderzeichen aus socisurvy
#' @export
cleansing_umlaute2 <-
  function(x) {
    diaeresis <- "\u00A8"
    ae <-  paste0("a", diaeresis)
    ue <-  paste0("u", diaeresis)
    oe <-  paste0("o", diaeresis)
    Ue <-  paste0("U", diaeresis)
    Ae <-  paste0("A", diaeresis)
    Oe <-  paste0("O", diaeresis)
    x <- gsub(ae, "\u00e4", x)
    x <- gsub(oe, "\u00f6", x)
    x <- gsub(ue, "\u00fc", x)
    x <- gsub(Ae, "\u00c4", x)
    x <- gsub(Ue, "\u00dc", x)
    x <- gsub(Oe, "\u00d6", x)
    x
  }


#' clean_space
#' 
#' Leerzeichen entfernen
#'
#' @param x string
#'
#' @noRd
clean_space <- function(x) {
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  sub(",", ".", x)
}
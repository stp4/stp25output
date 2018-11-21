#' Projekt
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
    path<- "test.txt"
    myformat <- "text"
  }
  
  if (grepl("\\.r$", path, perl = TRUE, ignore.case = TRUE)) {
    Projekt_html(
      myformat = tolower(myformat),
      Projektname = Projektname,
      datum = datum,
      fig_folder = fig_folder,
      html_folder = html_folder,
      OutDec = OutDec,
      contrasts =  contrasts,
      css = css,
      ...
    )
  } else   {
    Projekt_Rmd(
      myformat = tolower(myformat),
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
#' @description Einstellungen fuer HTNL mit \code{R2HTML::HTMLStart()}
#' @param html_name umlaute bereinigter Projektname
#' @importFrom R2HTML HTMLGetFile HTMLStart HTMLGetFile HTMLStop
Projekt_html <- function (myformat,
                          Projektname,
                          datum,
                          fig_folder,
                          html_folder,
                          OutDec,
                          contrasts,
                          html_name = stpvers::cleansing_umlaute(Projektname),
                          css = TRUE)
{
  
  #cat("\nProjekt_html\n")
  if (myformat != "rpres") {
    if (Projektname == "Demo")
      setwd("C:/Users/wpete/Dropbox/3_Forschung/R-Project")
    #-- Fehler Abfangen
    if (options()$prompt[1] == "HTML> ") {
      options(prompt = "> ")
    }
    
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
    
    
    if (!dir.exists(output.dir))
      dir.create(
        output.dir,
        showWarnings = TRUE,
        recursive = FALSE,
        mode = "0777"
      )
    
    if (!dir.exists(fig_folder))
      dir.create(
        fig_folder,
        showWarnings = TRUE,
        recursive = FALSE,
        mode = "0777"
      )
    
    if (fig_folder != "Fig")
      set_my_options(fig_folder = paste0(fig_folder, "/"))
    
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
    
    if (myformat == "html") {
      set_default_params(list(
        file.name.index = 0,
        reset = par(no.readonly = TRUE)
      ))
      
      
    #  cat("\n R2HTML::HTMLStart()\n")
      R2HTML::HTMLStart(
        outdir = output.dir,
        file = html_name,
        extension = myformat,
        echo = FALSE,
        HTMLframe = FALSE,
        CSSFile = "layout.css"
         
      )
      
      if (css) {
        myCssFile <- file.path(output.dir, "layout.css")
        cat("\nCSS-File:" , myCssFile, "\n")
        
        
        if (!file.exists(myCssFile))
          cat(MyCss(), file = myCssFile)
      }
    }
    else{
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
    
  }
  else{
    if (!is.null(OutDec))
      options(OutDec = OutDec)
    if (fig_folder != "Fig")
      set_my_options(fig_folder = paste0(fig_folder, "/"))
    
    if (!is.null(contrasts)) {
      oldc <- getOption("contrasts")
      options(contrasts = contrasts)
    }
    
    set_default_params(list(Tab_Index = 0, Abb_Index = 0))
    pr_string <- NULL
  }
  
  set_my_options(output = myformat)
  invisible(pr_string)
}



# Projekt <- function (myformat = "",
#                      Projektname = "Demo",
#                      datum = date(),
#                      fig_folder = "Fig",
#                      html_folder = "Results",
#                      OutDec = NULL,
#                      contrasts =  c("contr.Treatment", "contr.poly"),
#                      html_name = stpvers::Clean_Umlaute2(Projektname),
#                      css = TRUE,
#                      ...)
# {
#   #t1 <- Sys.time()
#   myformat <- tolower(myformat)
#   
#   if (myformat != "rpres") {
#     set.seed(0815)
#     if (Projektname == "Demo")
#       setwd("C:/Users/wpete/Dropbox/3_Forschung/R-Project")
#     #-- Fehler Abfangen
#     if (options()$prompt[1] == "HTML> ") {
#       options(prompt = "> ")
#     }
#     
#     myDir <- getwd()
#     myURL <-
#       paste("file://",
#             myDir,
#             "/",
#             html_folder,
#             "/",
#             html_name,
#             ".",
#             myformat,
#             sep = "")
#     output.dir <- paste(myDir, html_folder, sep = "/")
#     
#     
#     if (is.null(OutDec))
#       OutDec <- options()$OutDec
#     else
#       options(OutDec = OutDec)
#     
#     options(R2HTML.format.decimal.mark = OutDec)
#     
#     
#     if (!dir.exists(output.dir))
#       dir.create(
#         output.dir,
#         showWarnings = TRUE,
#         recursive = FALSE,
#         mode = "0777"
#       )
#     
#     if (!dir.exists(fig_folder))
#       dir.create(
#         fig_folder,
#         showWarnings = TRUE,
#         recursive = FALSE,
#         mode = "0777"
#       )
#     
#     if (fig_folder != "Fig")
#       set_my_options(fig_folder = paste0(fig_folder, "/"))
#     
#     if (!is.null(contrasts)) {
#       oldc <- getOption("contrasts")
#       options(contrasts = contrasts)
#       cat(
#         "\nKontraste von " ,
#         paste(oldc, collapse = ", "),
#         "auf\n",
#         paste(contrasts, collapse = ", "),
#         " umgestellt!\n"
#       )
#     }
#     
#     set_default_params(list(Tab_Index = 0, Abb_Index = 0))
#   
#     if (myformat == "html") {
#       set_default_params(list(
#         file.name.index = 0,
#         reset = par(no.readonly = TRUE)
#       ))
#       
#       R2HTML::HTMLStart(
#         outdir = output.dir,
#         file = html_name,
#         extension = myformat,
#         echo = FALSE,
#         HTMLframe = FALSE
#       )
#       
#       if (css) {
#         myCssFile <- file.path(output.dir, "R2HTML.css")
#         cat("\nCSS-File:" , myCssFile, "\n")
#         if (!file.exists(myCssFile))
#           cat(MyCss(), file = myCssFile)
#       }
#     }   
#     else{
#       options(continue = "  ")
#     }
#     
#     
#     pr_string <- paste(
#       Projektname,
#       "\n Datum: ",
#       datum,
#       ", Software: ",
#       R.version.string ,
#       ", Link: www.R-project.org/\nFile: ",
#       get_scriptpath()
#     )
#     Text(pr_string)
#     
#   }
#   else{
#     #myDir <- getwd()
#     if (!is.null(OutDec))
#       options(OutDec = OutDec)
#     if (fig_folder != "Fig")
#       set_my_options(fig_folder = paste0(fig_folder, "/"))
#     
#     if (!is.null(contrasts)) {
#       oldc <- getOption("contrasts")
#       options(contrasts = contrasts)
#     }
#     
#     set_default_params(list(Tab_Index = 0, Abb_Index = 0))
#     pr_string <- NULL
#   }
#   
#   set_my_options(output = myformat)
#   invisible(pr_string)
# }








MyCss <- function() {
  '
/*
* === MAJOR SECTION HEADING ===
  */


  body {
  background: #FFFFFF;
  color: #000000;
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 10pt;
  font-weight: normal
  line-height: normal;
  }
  
  H1 {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 18pt;
  font-style: normal;
  font-weight: bold;
  line-height: 25pt;
  color: #004080;
  
  }
  
  H2 {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 16pt;
  font-style: normal;
  font-weight: bold;
  line-height: 20pt;
  color: #004080;
  }
  
  H3 {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 14pt;
  font-style: normal;
  font-weight: bold;
  line-height: 16pt;
  color: #004080;
  }
  
  H4 {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 12pt;
  font-style: normal;
  font-weight: bold;
  color: #000000;
  line-height: 10pt;
  }
  
  H5 {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 10pt;
  font-style: normal;
  font-weight: bold;
  color: #000000;
  line-height: 10pt;
  }
  
  
  TABLE, TH, TD  {
  font-family: Arial, Helvetica,  Helvetica, sans-serif;
  font-size: 8pt;
  font-style: normal;
  line-height: normal;
  }
  
  LI {
  font-family: "Times New Roman", Times, serif;
  font-size: 10pt
  }
  
  A {
  font-family: "Times New Roman", Times, serif;
  font-size: 10pt;
  text-decoration: none
  }
  
  
  
  
  P{
  font-family: "Times New Roman", Times, serif;
  font-style: normal;
  font-size: 10pt;
  }
  
  '
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


#' @rdname Projekt
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

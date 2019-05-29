#R2HTML




#R2HTML::HTMLGetFile
HTMLGetFile <- function (){
  if (exists(".HTML.file", .HTMLEnv))
    get(".HTML.file", .HTMLEnv)
  else
    stop("not default HTML output file defined; please call HTMLSetFile2() to set it")
}

GetContrasts <- function (){
  if (exists("old.contrasts", .HTMLEnv))
    get("old.contrasts", .HTMLEnv)
  else
     c("contr.treatment", "contr.poly")
  }


#----------------------------------------------------------------------------------------------------#
###   R2HTML CORE
#----------------------------------------------------------------------------------------------------#
.HTMLEnv <- new.env(parent = emptyenv())

HTMLEndFile2 <-
  function(date = format(Sys.time(), "%a %d %b %Y %H:%M:%S "),
           file = HTMLGetFile())
  {
    cat(
      paste0('<p>', date, '</p>\n</body>\n</html> '),
      sep = "",
      append = TRUE,
      file = file
    )
  }


 





#' @rdname Projekt
#' @export
HTML_End <- function(browser = "iexplore"
                     ) {
  file <-if(browser== "firefox") paste0("file:///", HTMLGetFile() )
  else HTMLGetFile()
  brwsr<- list(  
    chrome= "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    firefox= "C:/Program Files/Mozilla Firefox/firefox.exe",
    iexplore=  "C:/Program Files (x86)/Internet Explorer/iexplore.exe")
  
  browser<-  brwsr[[browser]]

  HTMLEndFile2()
  
  browseURL(file, browser = browser)
  
  options(contrasts = c("contr.treatment", "contr.poly"))  #  options(contrasts = GetContrasts())
  set_default_params(list(Tab_Index = 0, Abb_Index = 0, file.name.index = 0))
  reset_lattice() # assign("old.par", par(no.readonly = TRUE), envir = .HTMLEnv)
  set_my_options(output = "text")
 
  
  options(prompt = "> ")
  
  HTMLGetFile()
}


html_init <- function(title, file, layout.css = "layout.css"){
cat(
  paste0(
'<!DOCTYPE html>
<html>
<head>
<title>',  title, '</title>
<link rel=stylesheet href="', layout.css, '" type=text/css>
</head>
<body>

')
 , file = file, append = FALSE)
   
}






MyCss <- function() {
  '
/*
* === MyCss layout.css MAJOR SECTION HEADING ===
*/


  body {
  background: #FFFFFF;
  color: #000000;
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: 10pt;
  font-weight: normal;
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
  font-size: 10pt;
  }
  
  A {
  font-family: "Times New Roman", Times, serif;
  font-size: 10pt;
  text-decoration: none;
  }
  
  
  
  
  P{
  font-family: "Times New Roman", Times, serif;
  font-style: normal;
  font-size: 10pt;
  }
  
  '
}





creat_folder <- function(output.dir, fig_folder, css.file) {
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
  
  
  if (!file.exists(css.file))
    cat(MyCss(), file = css.file)
}

html_settings <- function(contrasts, withprompt, OutDec, extension, file) {

  options(prompt = withprompt) 
  oldc <- getOption("contrasts") 

  if (!is.null(contrasts)) {
    options(contrasts = contrasts)
    cat(
      "\nKontraste von " ,
      paste(oldc, collapse = ", "),
      "auf\n",
      paste(contrasts, collapse = ", "),
      " umgestellt!\n"
    )
  }

  if (is.null(OutDec))  OutDec <- options()$OutDec
  else  options(OutDec = OutDec)
  
  set_default_params(list(Tab_Index = 0, 
                          Abb_Index = 0, 
                          file.name.index = 0))
  
  set_my_options(output = extension)
 
  assign("old.contrasts", oldc, envir = .HTMLEnv)
  assign("old.par", par(no.readonly = TRUE), envir = .HTMLEnv)
  assign(".HTML.file", file, .HTMLEnv)
  
}


#' @rdname Projekt
#' @export
HTML_Start <- function (Projektname = "Demo",
                        datum = date(),
                        fig_folder = "Fig",
                        html_folder = "Results",
                        OutDec = NULL,
                        contrasts =  c("contr.Treatment", "contr.poly"),
                        withprompt = "HTML> ",
                        .extension = "html",
                        .myDir = getwd(),
                        .filename = cleansing_umlaute(Projektname),
                        .output.dir = file.path(.myDir, html_folder),
                        .HTML.file  = file.path(.output.dir, paste(.filename, ".", .extension, sep = "")),
                        .css.file = file.path(.output.dir, "layout.css")
                         )
{
  if (Projektname == "Demo")
    setwd("C:/Users/wpete/Dropbox/3_Forschung/R-Project")
  #-- Fehler Abfangen
  if (options()$prompt[1] == withprompt) {
    options(prompt = "> ")
        return()
  }

  creat_folder(.output.dir, fig_folder, .css.file)
  html_settings(contrasts, withprompt, OutDec, .extension, .HTML.file)
  html_init(Projektname, .HTML.file)

  
  Text(Projektname, "\n Datum: ", datum,
      ", Software: ", R.version.string,
      ", Link: www.R-project.org/\nFile: ", get_scriptpath())
  
}



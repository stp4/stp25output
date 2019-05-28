#R2HTML




#R2HTML::HTMLGetFile
HTMLGetFile <- function (){
  if (exists(".HTML.file", .HTMLEnv))
    get(".HTML.file", .HTMLEnv)
  else
    stop("not default HTML output file defined; please call HTMLSetFile2() to set it")
}

HTMLSetFile2 <- function(file) {
  assign(".HTML.file", file, .HTMLEnv)
  file
}

#----------------------------------------------------------------------------------------------------#
###   R2HTML CORE
#----------------------------------------------------------------------------------------------------#
#.HTMLEnv <- new.env(parent = emptyenv())
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


MyHlmlStart <- function(title = "Demo", layout.css = "layout.css") {
  paste0(
    '<!DOCTYPE html>
<html>
<head>
<title>',
    title,
    '</title>
<link rel=stylesheet href="',
    layout.css,
    '" type=text/css>
</head>
<body>

'
  )
}





#' @rdname Projekt
#' @export
HTML_End <- function(anhang = FALSE,
                     browser = "C:/Program Files (x86)/Internet Explorer/iexplore.exe",
                     output = options()$prompt[1] == "HTML> ",
                     ...) {
  options(contrasts = c("contr.treatment", "contr.poly"))
  #- Reset
  stp25output:::stp25_options()
  if (exists("opar"))
    lattice::trellis.par.set(opar)
  if (exists("oopt"))
    lattice::lattice.options(oopt)
  if (exists("Tab_Index"))
    Tab_Index <<-  0
  if (exists("Abb_Index"))
    Abb_Index <<-  0
  
  HTMLEndFile2()
  
  
  #?removeTaskCallback("HTML")
  

  browseURL(HTMLGetFile(), browser = browser)
  options(prompt = "> ")
  
  if (output)
    file
  else
    cat("\nReset Kontraste\n")
  
  HTMLGetFile()
}


html_init <-
  function(title, file)
    
  {
    .HTMLTmpEnv <- new.env(parent = .GlobalEnv)
 #   assign(".HTML.outdir", outdir, envir = .HTMLTmpEnv)
    assign(".HTML.file", file, .HTMLTmpEnv)
  #  assign("HTMLtorefresh", file.path(outdir, paste(filename, extension, sep = ".")), envir = .HTMLTmpEnv)
   
    cat(MyHlmlStart(title), file = file, append = FALSE)
    HTMLSetFile2(file)
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
  

  
  if (!dir.exists(.output.dir))
    dir.create(
      .output.dir, showWarnings = TRUE,
      recursive = FALSE,mode = "0777"
    )
  
  if (!dir.exists(fig_folder))
    dir.create(
      fig_folder, showWarnings = TRUE,
      recursive = FALSE, mode = "0777"
    )
  
  if (fig_folder != "Fig")
    set_my_options(fig_folder = paste0(fig_folder, "/"))
  
  if (!is.null(contrasts)) {
    oldc <- getOption("contrasts")
    options(contrasts = contrasts)
    cat(
      "\nKontraste von " ,
      paste(oldc, collapse = ", "),
      "auf\n",paste(contrasts, collapse = ", "),
      " umgestellt!\n"
    )
  }
  options(prompt = withprompt)
  
  if (is.null(OutDec))
    OutDec <- options()$OutDec
  else
    options(OutDec = OutDec)
  set_default_params(list(Tab_Index = 0, Abb_Index = 0))
  set_my_options(output = .extension)
  set_default_params(list(
    file.name.index = 0,
    reset = par(no.readonly = TRUE)
  ))
  
  html_init(Projektname, .HTML.file)
  
  if (!file.exists(.css.file))  cat(MyCss(), file = .css.file)
  
  
  Text(
    paste(
      Projektname,
      "\n Datum: ",  datum,
      ", Software: ",  R.version.string ,
      ", Link: www.R-project.org/\nFile: ",
      get_scriptpath()
    )
  )
  
}



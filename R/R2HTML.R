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
.HTMLEnv <- new.env(parent = emptyenv())


html_init <-
  function(outdir = tempdir(),
           filename = "index",
           extension = "html",
           withprompt = "HTML> ",
           CSSFile = "R2HTML.css",
           Title = "R output",
           .HTML.file  = file.path(outdir, paste(filename, ".", extension, sep = "")))
    
  {
    .HTMLTmpEnv <- new.env(parent = .GlobalEnv)
    assign(".HTML.outdir", outdir, envir = .HTMLTmpEnv)
    assign(".HTML.file", .HTML.file, .HTMLTmpEnv)
    assign("HTMLtorefresh", file.path(outdir, paste(filename, extension, sep = ".")), envir = .HTMLTmpEnv)
    options(prompt = withprompt)
    
    cat(MyHlmlStart(Title), file = .HTML.file, append = FALSE)
    HTMLSetFile2(.HTML.file)
}


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
  
  
  invisible(removeTaskCallback("HTML"))
  
  
  
  
  
  # R2HTML::HTMLStop()
  #   print(tmp)
  # getOption("browser")
  browseURL(HTMLGetFile(), browser = browser)
  #else {cat("Die Funktion Projekt() mus vorher ausgefuert werden.",  "\n\n")}
  
  
  options(prompt = "> ")
  
  if (output)
    file
  else
    cat("\nReset Kontraste\n")
}


#' @rdname Projekt
#' @export
HTML_Start <- function (Projektname = "Demo",
                        myformat = "html",
                        datum = date(),
                        fig_folder = "Fig",
                        html_folder = "Results",
                        OutDec = NULL,
                        contrasts =  c("contr.Treatment", "contr.poly"),
                        
                        html_name = cleansing_umlaute(Projektname),
                        css = "layout.css")
{
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
  
  
  set_default_params(list(
    file.name.index = 0,
    reset = par(no.readonly = TRUE)
  ))
  
  html_init(
    outdir = output.dir,
    filename = html_name,
    extension = myformat,
    CSSFile = css,
    Title = Projektname
    
  )
  
  myCssFile <- file.path(output.dir, "layout.css")
  if (!file.exists(myCssFile))
    cat(MyCss(), file = myCssFile)
  
  
  
  
  
  pr_string <- paste(
    Projektname,
    "\n Datum: ",
    datum,
    ", Software: ",
    R.version.string ,
    ", Link: www.R-project.org/\nFile: ",
    stp25output:::get_scriptpath()
  )
  Text(pr_string)
  
  
  if (fig_folder != "Fig")
    set_my_options(fig_folder = paste0(fig_folder, "/"))
  
  
  
  
  
  set_my_options(output = myformat)
  invisible(pr_string)
}



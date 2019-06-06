#' Speichern von Grafiken
#' 
#' @description  Grafik werden  als PDF oder Windows-Metafile
#' speichern. Die Funktion Abb() beinhaltet den Pfad.
#' Die Funktion arbeitet ueber  R2HTML::HTMLplot()
#' @param caption   Grafik Abbildung  caption=data,
#' @param filename    Daten oder Character fuer die speicherung einer Grafik 
#' @param Width,Height,w,h  an HTMLplot default = 520, Breite der  Grafik  also zb w=8=dev.size("in")[2], h=dev.size("in")[1] Hoehe der Grafik also zb h=8
#' @param GraphBorder  Rand um Grafik = 0
#' @param Align    Centriert
#' @param GraphBackGround  Hintergrund= "white"
#' @param save_plot speichern als file = TRUE,
#' @param ...  zusaetliche Parameter
#' @return NULL
#' @importFrom grDevices dev.copy2pdf dev.off dev.size savePlot rgb2hsv
#' @export
SaveData <- function(caption = "", 
                     filename = "",
                     w = dev.size("in")[1], h = dev.size("in")[2],
                     save_plot= TRUE,
                     output =  which_output()
                  ) {
  
abb <- Abb(filename, caption)

if (output == "html") {
  R2HTML_HTMLplot(
    Caption = abb$HtmlCaption,
    file = HTMLGetFile(),
    GraphDirectory = split_path(HTMLGetFile())[2],
    Width = round(w * 75),
    Height = round(h * 75),
    GraphFileName = abb$GraphFileName,
    GraphBorder = 0,
    Align = options()$stp25$apa.style$Align
  )
}

if(save_plot){
  if (length(grep(".pdf", filename)) > 0) {
     try( dev.copy2pdf(file = abb$Name, width = w, height = h) )
  } else if (length(grep(".emf", filename)) > 0) {
   try( savePlot(abb$Name , type = "emf"))
    dev.off()
  } else{
    abb$Name<- paste0(abb$Name, ".pdf")
    try(dev.copy2pdf(file = abb$Name, width = w, height = h))
  }
  HTML_S(abb$Nam)
  HTML_BR()
  }
}



# https://stackoverflow.com/questions/29214932/split-a-file-path-into-folder-names-vector
split_path <- function(path,
                       mustWork = FALSE,
                       rev = TRUE) {
  output <-
    c(strsplit(dirname(
      normalizePath(path, mustWork = mustWork)
    ),
    "/|\\\\")[[1]], basename(path))
  ifelse(rev, return(rev(output)), return(output))
}






#' Kopie von R2HTML::HTMLGetFile
#' @noRd
R2HTML_HTMLplot<-
  function (Caption = "",
            file = HTMLGetFile(),
            append = TRUE,
            GraphDirectory = ".",
            GraphFileName = "",
            GraphSaveAs = "png",
            GraphBorder = 1,
            Align = "center",
            Width = 500,
            Height = 500,
            WidthHTML = NULL,
            HeightHTML = NULL,
            GraphPointSize = 12,
            GraphBackGround = "white",
            GraphRes = 72,
            plotFunction = NULL,
            ...)
  {
    if (exists(".HTMLTmpEnv", where = .HTMLEnv)) {
      GraphDirectory <- get(".HTML.outdir", envir = get(".HTMLTmpEnv", 
                                                        envir = .HTMLEnv))
    }
    
    #HTML_P(paste("File: ", file))
   # HTML_P(paste("GraphDirectory: ",GraphDirectory))
    cat("\n", file = file, append = append, ...)
    if (GraphFileName == "") {
      nowd <- date()
      GraphFileName <- paste("GRAPH_", substring(nowd, 5, 7), 
                             substring(nowd, 9, 10), "_", 
                             substring(nowd, 12,  13), 
                             substring(nowd, 15, 16), 
                             substring(nowd, 18, 19), sep = "")
    }
    GraphFileName <- paste(GraphFileName, ".", GraphSaveAs, sep = "")
    AbsGraphFileName <- file.path(GraphDirectory, GraphFileName)
  #  HTML_P(paste("GraphFileName: ", GraphFileName))
  #  HTML_P(paste("AbsGraphFileName: ", AbsGraphFileName))
    if (GraphSaveAs == "png") {
      if (is.null(plotFunction)) 
        dev.print(device = png, file = AbsGraphFileName, 
                  width = Width, height = Height, pointsize = GraphPointSize, 
                  bg = GraphBackGround)
      else {
        if (exists("X11", envir = .GlobalEnv) && Sys.info()["sysname"] != 
            "Windows" && Sys.info()["sysname"] != "Darwin") 
          bitmap(file = AbsGraphFileName, bg = GraphBackGround, 
                 res = GraphRes)
        else png(filename = AbsGraphFileName, width = Width, 
                 height = Height, pointsize = GraphPointSize, 
                 bg = GraphBackGround)
        plotFunction()
        dev.off()
      }
    }
    else if (GraphSaveAs %in% c("jpg", "jpeg")) {
      if (is.null(plotFunction)) 
        dev.print(device = jpeg, file = AbsGraphFileName, 
                  width = Width, height = Height, pointsize = GraphPointSize, 
                  bg = GraphBackGround)
      else {
        if (exists("X11", envir = .GlobalEnv) && Sys.info()["sysname"] != 
            "Windows" && Sys.info()["sysname"] != "Darwin") 
          bitmap(filename = AbsGraphFileName, bg = GraphBackGround, 
                 res = GraphRes, type = "jpeg")
        else jpeg(filename = AbsGraphFileName, width = Width, 
                  height = Height, pointsize = GraphPointSize, 
                  bg = GraphBackGround)
        plotFunction()
        dev.off()
      }
    }
    else if (GraphSaveAs == "gif") {
      stop("Gif support was removed from base R because of patent restrictions. Use either jpg or png")
    }
    else stop("GraphSaveAs must be either jpg, png or gif")
    cat(paste("<p align=", Align, "><img src='", GraphFileName, 
              "' border=", GraphBorder, if (!is.null(Width)) 
                paste(" width=", Width, sep = "")
              else "", if (!is.null(HeightHTML)) 
                paste(" height=", HeightHTML, sep = ""), if (!is.null(WidthHTML)) 
                  paste(" width="), ">", sep = "", collapse = ""), 
        file = file, append = TRUE, sep = "")
    if (Caption != "") {
      cat(paste("<br><font class=caption>", Caption, "</font>"), 
          file = file, append = TRUE, sep = "")
    }
    cat("</p>", file = file, append = TRUE, sep = "\n")
    if (substitute(file) == "HTMLGetFile()") 
      try(assign(".HTML.graph", TRUE, envir = .HTMLEnv))
    invisible(return(TRUE))
  }
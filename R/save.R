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
                     
                     Width = 520, Height = 520, 
                     GraphBorder = 0,
                     Align = options()$stp25$apa.style$Align,
                     GraphBackGround = "white",
                     save_plot= TRUE,
                   #  graphics_devices = options()$stp25$graphics_devices,
                     ...) {
  
abb <- Abb(filename, caption)

if (options()$prompt[1] == "HTML> ") {
    R2HTML::HTMLplot(
      Caption = abb$HtmlCaption,
      Width = round(w * Width / 7),
      Height = round(h * Height / 7),
      GraphFileName = abb$GraphFileName,
      GraphBorder = GraphBorder,
      Align = Align,
      #  GraphPointSize = GraphPointSize,
      GraphBackGround = GraphBackGround
      ### geht nicht, GraphPointSize = 18
    )
  }
#-- Speichern ---------------------

if(save_plot){
 # cat("\nSave plot ")
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

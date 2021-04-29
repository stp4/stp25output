#' SavePlot
#' 
#' Speichern von Grafiken
#' Grafik werden  als PDF oder Windows-Metafile
#' speichern. Die Funktion Abb() beinhaltet den Pfad.
#'
#' @param caption TextAbbildung  
#' @param w,h,res  Width, Height, an HTMLplot 
#' default = 520, Breite der  Grafik  also zb w=8=dev.size("in")[2], 
#' h=dev.size("in")[1] Hoehe der Grafik also zb h=8
#' res=72
#' @param filename Text 
#' @param save_plot speichern als file = TRUE,
#' @param output nur wichtig bei html
#' @param out.type	 name of the output device: can 
#' be "pdf", or "cairo" 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' plot(1)
#' SavePlot("Einfache Grafik", w=4, h=4)
#' 
#' 
SavePlot <- function(caption = "",
                     w = dev.size("in")[1],
                     h = dev.size("in")[2],
                     filename = "",
                     save_plot = TRUE,
                     output =  which_output(),
                     res=72,
                     out.type="pdf"
                  
                    ) {
  abb <- Abb(filename, caption)
  
  if (output == "html") {
    file <- HTMLGetFile() 
    GraphFileName <- paste0(abb$GraphFileName, ".png")
    # resulution 
    Width <- round(w * res)
    Height <- round(h * res)
    
    dev.print(
      device = png,
      file = file.path(dirname(file), GraphFileName),
      width = Width,
      height = Height,
      pointsize = 12,
      bg =  "white",
      res = res  # res liefert unerwartete Ergebnisse 
    )
    
    # Width <- round(w * 75)
    # Height <- round(h * 75)
    # 
    # dev.print(
    #   device = png,
    #   file = file.path(dirname(file), GraphFileName),
    #   width = Width,
    #   height = Height,
    #   pointsize = 12,
    #   bg =  "white",
    #   res = 72  # res liefert unerwartete Ergebnisse 
    # )
    cat(
      paste0(
        "\n<div align='center'>",
        "<figure style='font-family: verdana; font-size: 8pt; font-style: italic; padding: 8px; text-align: center'>",
        "<img src='", GraphFileName, "'",
        " border=0",
        " width=", Width, " height=", Height,
        "/><br>",
        "<figcaption>", abb$HtmlCaption, "</figcaption>\n",
        "</figure> </div>"
      ),
      file = file, append = TRUE, sep = ""
    )

    }
  
  if (save_plot) {
      abb$Name <- paste0(abb$Name, ".pdf")
      try(dev.copy2pdf(file = abb$Name,
                       width = w,
                       height = h,
                       out.type=out.type))
    HTML_S(abb$Nam)
    HTML_BR()
  }
}






# cat(
#   paste0(
#     "\n <p align='center'><img src='",GraphFileName, "'",
#     " border=0",
#     " width=", Width, 
#     " height=", Height,
#     "/>",
#     "<br><font class=caption>", abb$HtmlCaption, "</font>",
#     "</p>"
#   ),
#   file = file, append = TRUE, sep = ""
# )
# 
# if (substitute(file) == "HTMLGetFile()")
#   try(assign(".HTML.graph", TRUE, envir = .HTMLEnv))



#   
#   R2HTML_HTMLplot(
#     Caption = abb$HtmlCaption,
#     file = HTMLGetFile(),
#     GraphDirectory = split_path(HTMLGetFile())[2],
#     Width = round(w * 75),
#     Height = round(h * 75),
#     GraphFileName = abb$GraphFileName,
#     GraphBorder = 0,
#     Align = options()$stp25$apa.style$Align)
# 


# Kopie von R2HTML::HTMLGetFile
# @param GraphBorder  Rand um Grafik = 0
# @param Align    Centriert
# @param GraphBackGround  Hintergrund= "white"#'
# @noRd

# R2HTML_HTMLplot <-
#   function (Caption = "",
#             file = HTMLGetFile(),
#             append = TRUE,
#             GraphDirectory = ".",
#             GraphFileName = "",
#             GraphSaveAs = "png",
#             GraphBorder = 1,
#             Align = "center",
#             Width = 500,
#             Height = 500,
#             WidthHTML = NULL,
#             HeightHTML = NULL,
#             GraphPointSize = 12,
#             GraphBackGround = "white",
#             GraphRes = 72,
#             ...)
#   {
#     if (exists(".HTMLTmpEnv", where = .HTMLEnv)) {
#       GraphDirectory <- get(".HTML.outdir",
#                             envir = get(".HTMLTmpEnv", envir = .HTMLEnv))
#     }
#     
#     cat("\n", file = file, append = append, ...)
#     
#     if (GraphFileName == "") {
#       nowd <- date()
#       GraphFileName <- paste(
#         "GRAPH_", substring(nowd, 5, 7), substring(nowd, 9, 10),
#         "_", substring(nowd, 12,  13), substring(nowd, 15, 16), substring(nowd, 18, 19),
#         sep = ""
#       )
#     }
#     GraphFileName <-
#       paste(GraphFileName, ".", GraphSaveAs, sep = "")
#     AbsGraphFileName <- file.path(GraphDirectory, GraphFileName)
#     
#     if (GraphSaveAs == "png") {
#       dev.print(
#         device = png,
#         file = AbsGraphFileName,
#         width = Width,
#         height = Height,
#         pointsize = GraphPointSize,
#         bg = GraphBackGround,
#         res = GraphRes
#       )
#     }
#     # else if (GraphSaveAs %in% c("jpg", "jpeg")) {
#     #   dev.print(
#     #     device = jpeg,
#     #     file = AbsGraphFileName,
#     #     width = Width,
#     #     height = Height,
#     #     pointsize = GraphPointSize,
#     #     bg = GraphBackGround,
#     #     res = GraphRes,
#     #   )
#     # }
#     else
#       stop("GraphSaveAs must be either jpg, png")
#     # else  if (GraphSaveAs == "bmp") {
#     #   dev.print(
#     #     device = bmp,
#     #     file = AbsGraphFileName,
#     #     width = Width,
#     #     height = Height,
#     #     pointsize = GraphPointSize,
#     #     bg = GraphBackGround,
#     #     res = GraphRes
#     #   )
#     # }
#     # else if (GraphSaveAs == "tiff") {
#     #   dev.print(
#     #     device = tiff,
#     #     file = AbsGraphFileName,
#     #     width = Width,
#     #     height = Height,
#     #     pointsize = GraphPointSize,
#     #     bg = GraphBackGround,
#     #     res = GraphRes
#     #   )
#     # }
#     
#     
#     cat(
#       paste(
#         "<p align=",Align,"><img src='",GraphFileName,"' border=",
#         GraphBorder,
#         if (!is.null(Width)) paste(" width=", Width, sep = "") else "",
#         if (!is.null(HeightHTML)) paste(" height=", HeightHTML, sep = ""),
#         if (!is.null(WidthHTML)) paste(" width="), ">", sep = "", collapse = ""
#         ),
#       file = file, append = TRUE, sep = ""
#     )
#     
#     
#     if (Caption != "") 
#       cat(
#         paste("<br><font class=caption>", Caption, "</font>"),
#         file = file, append = TRUE, sep = "")
#     
#     cat("</p>",
#         file = file, append = TRUE, sep = "\n")
#     
#     if (substitute(file) == "HTMLGetFile()")
#       try(assign(".HTML.graph", TRUE, envir = .HTMLEnv))
#     
#     invisible(return(TRUE))
#   }


# split_path
# https://stackoverflow.com/questions/29214932/split-a-file-path-into-folder-names-vector
# gtools::split_
#  
# @param path Pfad
# @param mustWork,rev weitere Einstellungen
# @noRd
# 
# split_path <- function(path,
#                        mustWork = FALSE,
#                        rev = TRUE) {
#   output <-
#     c(strsplit(dirname(
#       normalizePath(path, mustWork = mustWork)
#     ),
#     "/|\\\\")[[1]], basename(path))
#   ifelse(rev, return(rev(output)), return(output))
# }
#' Speichern von Objekten
#'
#' Speichert Grafiken als pdf und data.fames  und table-Objekte als csv.
#' 
#' @param x Objekt
#' @param ... alles weitere
#' @return NULL
#' @importFrom grDevices dev.copy2pdf dev.off dev.size savePlot rgb2hsv
#' @export
SaveData <-   function(x, ...) {
  UseMethod("SaveData")
}

#' @rdname SaveData
#' @export
SaveData.default <-   function(...) {
  SavePlot(...)
}


#' @rdname SaveData
#' @export
SaveData.table <-   function(x, ...) {
  SaveData.data.frame(stp25tools::fix_to_df(x), ...)
}

#' @rdname SaveData
#' @export
SaveData.ftable <-   function(x, ...) {
  SaveData.data.frame(stp25tools::fix_to_df(x), ...)
}

#' @rdname SaveData
#' @export
SaveData.xtable <-   function(x, ...) {
  SaveData.data.frame(stp25tools::fix_to_df(x), ...)
}


#' @rdname SaveData
#' @export
SaveData.list <-   function(x,
                            filename = "",
                            SaveAs = "csv",
                            ...) {
  fls <- NULL
  for (i in names(x)) {
    print(i)
    if (is.data.frame(x[[i]])) {
      if (filename == "")
        filename <- generate_name(SaveAs, i)
      else
        SaveAs <- tolower(tools::file_ext(filename))
      
      fls <- c(fls,
               SaveData.data.frame(x[[i]],
                                   filename, SaveAs))
    }
  }
  fls
}

#' @param x data.frame
#'
#' @param filename optional
#' @param SaveAs  optional
#' @param dir noch nicht implementiert
#'
#' @rdname SaveData
#' @export
#' @examples 
#' df <- data.frame(
#' A = c(1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1),
#' B = c(0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0)
#' )
#' SaveData(df, SaveAs = "sav")
#' 
SaveData.data.frame <- function(x,
                                filename = "",
                                SaveAs = "csv",
                                dir = getwd()) {
 
  if (filename == "")
    filename <- generate_name(SaveAs)
  else
    SaveAs <- tolower(tools::file_ext(filename))
  data <- switch(
    SaveAs,
    csv =
      readr::write_csv(x, filename),
    sav =
      haven::write_sav(x, filename),
    xls =
      readr::write_excel_csv(x, filename),
   
    rdata = save(x, filename),
    stop("Unknown extension '.", ext, "'", call. = FALSE)
  )
  
  paste(dir, filename, sep = "/")
  
}



generate_name<- function(SaveAs, sub=""){
  nowd <- date()
 
    filename <- paste(
      "Tab",
      if (exists("Tab_Index"))
        Tab_Index, sub,
      "_",
      substring(nowd, 5, 7),
      substring(nowd, 9, 10),
      "_",
      substring(nowd, 12,  13),
      substring(nowd, 15, 16),
      substring(nowd, 18, 19),
      ".",
      SaveAs,
      sep = ""
    )
  
}



 



# # orginal
# R2HTML_HTMLplot<-
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
#             plotFunction = NULL,
#             ...)
#   {
#     if (exists(".HTMLTmpEnv", where = .HTMLEnv)) {
#       GraphDirectory <- get(".HTML.outdir", envir = get(".HTMLTmpEnv",
#                                                         envir = .HTMLEnv))
#     }
#
#     #HTML_P(paste("File: ", file))
#    # HTML_P(paste("GraphDirectory: ",GraphDirectory))
#     cat("\n", file = file, append = append, ...)
#     if (GraphFileName == "") {
#       nowd <- date()
#       GraphFileName <- paste("GRAPH_", substring(nowd, 5, 7),
#                              substring(nowd, 9, 10), "_",
#                              substring(nowd, 12,  13),
#                              substring(nowd, 15, 16),
#                              substring(nowd, 18, 19), sep = "")
#     }
#     GraphFileName <- paste(GraphFileName, ".", GraphSaveAs, sep = "")
#     AbsGraphFileName <- file.path(GraphDirectory, GraphFileName)
#   #  HTML_P(paste("GraphFileName: ", GraphFileName))
#   #  HTML_P(paste("AbsGraphFileName: ", AbsGraphFileName))
#     if (GraphSaveAs == "png") {
#       if (is.null(plotFunction))
#         dev.print(device = png, file = AbsGraphFileName,
#                   width = Width, height = Height, pointsize = GraphPointSize,
#                   bg = GraphBackGround)
#       else {
#         if (exists("X11", envir = .GlobalEnv) && Sys.info()["sysname"] !=
#             "Windows" && Sys.info()["sysname"] != "Darwin")
#           bitmap(file = AbsGraphFileName, bg = GraphBackGround,
#                  res = GraphRes)
#         else png(filename = AbsGraphFileName, width = Width,
#                  height = Height, pointsize = GraphPointSize,
#                  bg = GraphBackGround)
#         plotFunction()
#         dev.off()
#       }
#     }
#     else if (GraphSaveAs %in% c("jpg", "jpeg")) {
#       if (is.null(plotFunction))
#         dev.print(device = jpeg, file = AbsGraphFileName,
#                   width = Width, height = Height, pointsize = GraphPointSize,
#                   bg = GraphBackGround)
#       else {
#         if (exists("X11", envir = .GlobalEnv) && Sys.info()["sysname"] !=
#             "Windows" && Sys.info()["sysname"] != "Darwin")
#           bitmap(filename = AbsGraphFileName, bg = GraphBackGround,
#                  res = GraphRes, type = "jpeg")
#         else jpeg(filename = AbsGraphFileName, width = Width,
#                   height = Height, pointsize = GraphPointSize,
#                   bg = GraphBackGround)
#         plotFunction()
#         dev.off()
#       }
#     }
#     else if (GraphSaveAs == "gif") {
#       stop("Gif support was removed from base R because of patent restrictions. Use either jpg or png")
#     }
#     else stop("GraphSaveAs must be either jpg, png or gif")
#     cat(paste("<p align=", Align, "><img src='", GraphFileName,
#               "' border=", GraphBorder, if (!is.null(Width))
#                 paste(" width=", Width, sep = "")
#               else "", if (!is.null(HeightHTML))
#                 paste(" height=", HeightHTML, sep = ""), if (!is.null(WidthHTML))
#                   paste(" width="), ">", sep = "", collapse = ""),
#         file = file, append = TRUE, sep = "")
#     if (Caption != "") {
#       cat(paste("<br><font class=caption>", Caption, "</font>"),
#           file = file, append = TRUE, sep = "")
#     }
#     cat("</p>", file = file, append = TRUE, sep = "\n")
#     if (substitute(file) == "HTMLGetFile()")
#       try(assign(".HTML.graph", TRUE, envir = .HTMLEnv))
#     invisible(return(TRUE))
#   }
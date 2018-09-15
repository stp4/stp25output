#' Tabellen und Abbildungs Nummern
#' 
#' Nummerierte Abbildung default Folder ist fig
#' 
#' @name Tab_Abb
#' 
#' @param caption  Name der Tabelle/Abbildung
#' @return Name plus Nummer
NULL
 
#' @rdname Tab_Abb
#' @export
#' @param caption 
#' 
Tab <- function (caption = "")
{
  if (!exists("Tab_Index"))
    Tab_Index <<- 1
  else
    Tab_Index <<- Tab_Index + 1
  paste0("Tab ", Tab_Index, ": ", caption)
}
 

#' @rdname Tab_Abb
#' @export
#' @param filename  Name des Foles
#' @param folder   Speicherort
#' 
Abb <- function (filename="", 
                 caption="",
                 folder = options()$stp25$fig_folder)
{
  if (!exists("Abb_Index"))
    Abb_Index <<- 1
  else
    Abb_Index <<- Abb_Index + 1

  list(
    HtmlCaption=paste0("Abb", Abb_Index, ": ", caption),
    GraphFileName = paste0("Fig", Abb_Index, filename),
    Name =paste0(folder, "Fig", Abb_Index, filename)
  )
}




#' @rdname Tab_Abb
#' @param atr in Caption: alternativer Text
#' @description  Ueberschrift aus stp-Objekt: Caption(caption, attr(x, "caption"))
#' oder Note(note, attr(x, "note")) mit
Caption <- function(caption = NULL,
                    atr = NULL,
                    output =  which_output()) {
  if (is.null(caption) & (!is.null(atr)))
    caption <-  atr
  else if (is.null(caption))
    caption <- ""
  
  
  if (output == "html" | output == "text" | output == "markdown_html")
    Tab(caption)
  else
    caption
}

#' @rdname Tab_Abb
#' @param x Text
Note<- function(x=NULL, 
                atr=NULL){
  if(is.null(x) & (!is.null(atr))) atr
  else if(!is.null(x)) x
  else ""
}


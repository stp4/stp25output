#' Tabellen-Nummern
#'
#' @description  Nummerierte Tabellen, mit aufruf der Funktion \code{Abb()} eird eine Tabellen Nummerierung
#' fortlaufend erstellt. Die Funktion wird intern in der \code{APA2(...)} verwendet.
#' Die Nummer wird dabei in der Veriable \code{Tab_Index} hinterlegt.
#' @param text  Name der Tabelle
#' @return Name plus Nummer
#' @author Wolfgang Peter
#' @export
Tab <- function (text = "")
{
  if (!exists("Tab_Index"))
    Tab_Index <<- 1
  else
    Tab_Index <<- Tab_Index + 1
  paste0("Tab ", Tab_Index, ": ", text)
}
#' Abbildungs- Nummer
#'
#' @description  Nummerierte Abbildung default Folder ist fig
#' @param filename,caption  Name der Grafik und Titel
#' @param folder   Speicherort
#' @return Name plus Pfad
#' @author Wolfgang Peter
#' @export
Abb <- function (filename="", caption="",
                 folder = options()$stp25$fig_folder)
{
  if (!exists("Abb_Index"))
    Abb_Index <<- 1
  else
    Abb_Index <<- Abb_Index + 1
  # paste0(folder, "(Fig", Abb_Index, ") ", text)
  list(
    HtmlCaption=paste0("Abb", Abb_Index, ": ", caption),
    GraphFileName = paste0("Fig", Abb_Index, filename),
    Name =paste0(folder, "Fig", Abb_Index, filename)
  )
}

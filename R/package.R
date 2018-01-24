#' stp25: A package for APA-tables, and HTML outputs.
#'
#' Diese Sammlung von Scripten beinhaltet nuetzliche Funktionen
#' zur Erstellung von Reports.
#' @section stp25 functions:
#' \subsection{Projekt und HTML}{
#' Die Funktion \code{Projekt}  ist essentiell fuer alle weiteren Funktionen es werden dabei
#' Variablen fuer die Darstellung gesetzt wie ZB Nachkommastellen
#' \code{digits.prozent = 0, digits.mean = 2, digits.r = 2,  digits.p = 3, digits.Fstat = 2}
#' Trennzeichen \code{OutDec = "."}. Das Aussehen der Grafiken (lattice) wird
#' ?ber \code{grafik = "ggplot2like"} gesteuert #' bei \code{grafik = 1} wird die normale
#' Einstellung verwendet. Unter \code{options()$stp25}
#' findet man die Liste mit allen Einstellungen.
#' Die HTML Ausgave wur ?ber \code{Projekt("html")} gesteuert und durch \code{End()} wird die Seite geladen.
#' }
#' \subsection{Tabellen}{
#' APA -Style Tabellen erstellten (HTML oder Console) \code{APA2}.
#' }
#' \subsection{Text}{
#' APA -Style Daten ausgabe als Text kann mit (HTML oder Console)
#'  \code{Result} erstellten.
#' }
#' \subsection{Daten}{
#'  Unterschiedliche Daten-Formate einlesen (SPSS, csv usw) mit
#'  \code{GetData}. Schreiben lassen sich die
#'  Daten (auch Grafiken) mit \code{SaveDate}
#'  }
#' \subsection{Messinstrumente}{
#' Fuer spezielle Messinstrumente wie \code{Kano}, \code{Likert},
#' oder \code{Rangreihe} stehen eigene Funktionen zur verfuegung.
#' }
#' \subsection{Aggregieren}{
#' Hilfsfunktionen zum Aggregieren finden sich unter \code{dapply2},
#' eine \code{apply()} Variante.
#' und \code{Melt2} eine Kombination aus \code{melt} und \code{dcast}
#' aus dem Packet \code{reshape2}.
#' Melt2 wird dabei wie APA2 verwendet also zB
#' \code{APA2(Produkt + Online + Preis ~ Group, DF, fun=mean2)}ist
#' fuer APA-Tabelle und wenn die Mittelwerte angefordert werden
#' sollen \code{Melt2(Produkt + Online + Preis ~ Group, DF, fun = mean2)}
#'  die Syntax. Summenindex kann mit \code{Index} oder das Umkodieren
#'  von Data.frames durch \code{Umcodieren}
#' erfolgen. Normskalen wie T-Werte lassen sich ueber \code{Norms} berechnen.
#' }
#' \subsection{Berechnen}{
#' Mittelwerte mit \code{mean2} Formatierte Masszahlen fuer
#' die \code{APA2.formula(~a+d, data, fun=Prozent)} zu finden
#' unter \code{Prozent}
#' }
#' @author Wolfgang Peter
#' @docType package
#' @name stp25
NULL

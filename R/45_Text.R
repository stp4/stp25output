#' @name Text
#' @rdname Text
#' @title Text Ausgabe
#' @description Ausgabe von \code{Text()} und Ueberschriften. \code{Head} ist dabei eine Kopie von \code{Text} mit
#' dem Parameter \code{style = 2}.
#'
#' \code{Zitat())} ist eine Ausgabe vom Type <blockquote>
#'
#' \code{Anmerkung())} ist farblich mit blauen Text
#'
#' \code{CitationLib(car)} gibt Pakete beschreibungen aus.
#'
#' \code{Anhang()}
#'
#' @param ... one or more R objects, to be converted to character vectors.
#' @param style 0 = p oder normaler Text 2 = h2 also ?berschrift bei Consolen-Ausgabe
#' @param char ist nicht zu Verwenden Text Trennzeichen bei Consolen-Ausgabe
#' @param output Abrage nach   which_output()
#' @return Vector
#' @export
#' @examples
#' #library(stp25)
#' #Projekt("html")
#' Head("Ueberschrift") ## HR=2
#'
#' Text("hallo", "ich bin ein Test")
#' ###plot(sin, -pi, 2*pi,main="Sinus")
#' ###HTMLplot( Caption="Look at this curve!")
#' #End()
Text <- function(...,
                 style = 0,
                 char = "-",
                 output =  which_output()) {
  report_html <- function(msg) {
    if (is.null(msg))
      msg <- ""
    if (is.vector(msg)) {
      msg <- unlist(msg)  #-Zeilenumbrueche
      msg <- gsub("\\n", "<BR>", msg)
    } else
      stop("msg must be of type vector")
    if (style == 0 | style == "p")
      HTML_P(msg)
    else
      R2HTML::HTML.title(msg, HR = style)
  }

  report_txt <- function(msg = NULL) {
    if (is.null(msg))
      msg <- ""
    if (is.vector(msg)) {
      msg <- unlist(msg)
      msg <- strsplit(msg, "\\n")  #-Zeilenumbr?che
      msg <- unlist(msg)
    } else {
      stop("msg must be of type vector")
    }

    char <- substr(char, 1, 1)
    underlined <- function(arg) {
      c(arg, paste(rep(char, max(nchar(
        msg
      ))), collapse = ""))
    }
    border <- function(arg) {
      n <- length(msg)
      ml <- max(nchar(msg))
      space <- paste(rep(" ", ml), collapse = "")
      line <- paste(rep(char, ml + 4), collapse = "")
      msg <-
        paste(msg, substr(rep(space, n), rep(1, n), ml - nchar(msg)), sep = "")
      c(line, paste(char, msg, char), line)
    }
    sfun <- list(underlined = underlined, border = border)
    if (is.numeric(style) &&
        length(style) == 1 && any(style == 1:length(sfun)))
      msg <-
      sfun[[style]](msg)
    else if (is.character(style) &&
             length(style) == 1 && !is.null(sfun[[style]]))
      msg <- sfun[[style]](msg)
    m <- matrix(msg, ncol = 1)
    colnames(m) <- ""
    rownames(m) <- rep("", length(msg))
    print.noquote(m)
  }



  if (output == "html")
    report_html(paste0(...))
  else if (output == "markdown")
    cat(...)
  else
    report_txt(paste0(...))
}

#' @rdname Text
#' @param char Bei Konsolenausgabe die Symbole
#' @export
Head<- function( ...,
                 style=3,
                 char = "-"){
  Text(..., style = style, char = char)}

#' @rdname Text
#' @export
Anmerkung <- function(...)
  Text('<div style="color:#0000FF"><b>Anmerkung:</b><br>', ..., "<br></div>")
#' @rdname Text
#' @param lib Paketname
#'
#' @export
CitationLib<-function(lib){
    x<- citation(lib)
    paste0(x$author[1],", (", x$year, "), ", x$title, ", URL ", x$url)
  }

#' @rdname Text
#' @export
Zitat <- function(...)
  Text('<blockquote>', ..., "<br></blockquote>")


#' @rdname Text
#' @param Lines in Arbeitszeit der Input-String
#' @param sep in Arbeitszeit das Input-String-Trennzeichen
#'
#' @export
Arbeitszeit<- function(Lines,
                       sep = "\\t"
) {
  zeit <- read.table(
    zz <- textConnection(gsub(sep, " ", Lines)),
    header = TRUE)
  close(zz)
  names(zeit) <- c("Datum",  "Start",   "Ende",   "Task")
  zeit$strt <- strptime(zeit$Start, "%H:%M")
  zeit$end <- strptime(zeit$Ende, "%H:%M")
  zeit$Zeit_Stunden<-  round(as.numeric(difftime(zeit$end,zeit$strt, units = "hours")), 2)
  zeit$Zeit_Summe <- cumsum(zeit$Zeit_Stunden)
  Output(zeit[, c("Datum", "Start", "Ende", "Task", "Zeit_Stunden", "Zeit_Summe" )])
  invisible(zeit)
}
#' @rdname Text
#' @param include.literatur Literatur
#' @param include.software Verwendete Software
#' @param include.mean Mittelwerte Prozent
#' @param include.test Signifikanz Test
#' @param include.regression Regression Allgenein
#' @param include.lm Regression linear
#' @param include.binom Regression binomial
#' @param include.lme  Regression Hirarchisch
#' @param include.poisso Regression Poisson
#' @param include.rangreihe Rangreihe
#' @param include.aufbau Uberschriften Aufbau
#' @param include.abkuerzung Abkuerzungen
#'
#' @export
Anhang <- function(include.literatur=TRUE,
                   include.software=TRUE,
                   include.mean=TRUE,
                   include.test=APA_formula,
                   include.regression=FALSE,
                   include.lm=APA_regression,
                   include.binom=APA_regression,
                   include.lme=APA_regression,
                   include.poisso=APA_regression,
                   include.rangreihe=APA_regression,
                   include.aufbau=TRUE,
                   include.abkuerzung=TRUE,
                   ... ){
  txt_abk <- txt_Anhang <- ""
  if(include.lm | include.binom | include.poisso | include.lme)
    include.regression <- TRUE
  include.regression<-

  libs <-search()
  myCitation<-  c(
    "Achim Buehl, (2014), SPss 22 Einfuehrung in die moderne Datenanalyse, 14. aktualisierte Auflage, Pearson",
    "APA, 2009, Publication Manual of the American Psychological Association",
    "Daniel Wollschlaeger (2012), Grundlagen der Datenanalyse mit R: Eine anwendungsorientierte Einfaehrung 2. Aufl., Heidelberg: Springer",
    "Juergen Bortz,  Nicola Doering, (2006), Forschungsmethoden und Evaluation, Heidelberg: Springer",
    "Juergen Bortz, Christof Schuster, (2010), Statistik fuer Human- und Sozialwissenschaftler, 7. Aufl., Heidelberg: Springer",
    "Juergen Bortz, Gustav A. Lienert, (2010), Kurzgefasste Statistik fuer die klinische Forschung, 3. Aufl., Heidelberg: Springer",

    "John Fox (2003), Effect Displays in R for Generalised Linear Models, Vol. Journal of Statistical Software 8(15), http://www.jstatsoft.org/v08/i15/paper",
    "John Fox, Sanford Weisberg, (2011), An R Companion to Applied Regression, Second Edition, Sage",

    "Lothar Sachs, Juergen Hedderich, (2006), Angewandte Statistik, 12.Aufl. Heidelberg: Springer",
    "Torsten Hothorn, Brian S.,  Everitt, (2009), A Handbook of Statistical Analyses Using R, Second Edition, Chapman and Hall/CRC",
    "Thomas A. Lang, Michelle Secic, (2006),  How to Report Statistics in Medicine: Annotated Guidelines for Authors, Editors, and Reviewers, ACP",
    "Andrew Gelman, Jennifer Hill, (2009) Data Analysis Using Regression and Multilevel/Hierarchical Models 11 Edition",

    CitationLib("base"),
    CitationLib("Hmisc"),
    CitationLib("car"),
    CitationLib("lattice")

  )

  myCitation <- paste0("[", 1:length(myCitation), "] ", myCitation)


  n <- 5

  #--------------------------------------------------------
if(include.mean){
txt_Anhang <- c(txt_Anhang,
    "<B>Deskriptive Analyse</B>
     In den Tabellen zur deskriptive Analyse wird bei ordinalen oder metrischen Skalenniveau. Je nach Kontext der Mittelwert oder Median berechnet, dabei wird die Schreibweise <b>M(SD)</b> oder <b>Median[unteres Quartil, oberes Quartil]</b>, verwendet. Bei Faktoren (Nominal- Skala) wird der Anteil in Prozent angegeben, dabei wird die Schreibweise <b>Prozent(Anzahl)</b> verwendet.
    ")
txt_abk <-c(txt_abk,
   "<b>Deskriptive Analyse</b>
    <i>M</i> &hellip; Mittelwert
    <i>SD</i> &hellip; Standardabweichung
    <i>N, n</i> &hellip; Anzahl (aller) erhobenen Messwerte
    <i>NA, n.a.</i> &hellip; Fehlender Wert nicht anwendbar
    <i>%</i>  &hellip; Prozent, v. H.
    ")

if(APA_formula_test) {
txt_Anhang <- c(txt_Anhang,
    "Wenn ein Signifikanz Test angegeben ist handelt es sich um einen nicht-parametrischen Test der abhaengig vom Skalenniveau entweder ein Chi-Quadrat-test bei Nominal-Skala oder Wilcoxon bzw. Kruskal-Wallis-Test bei ordinalen und metrischen-Skalen ist. (Es werden auf Empfehlungen des APA-Styles die F-Werte mit ausgegeben)")
txt_abk <-c(txt_abk,
   "<b>Signifikanz Test</b>
    <i>F</i> &hellip; Wilcoxon or Kruskal-Wallis (approximated by using the t or F distributions) Wilcoxon-Mann-Whitney-Test ist identisch mit dem Mann-Whitney-U-Test
    <i>X, &Chi;</i> &hellip; Pearson chi-square test
    <i>p</i> &hellip; p-Wert, Probability-Wert, Wahrscheinlichkeit der Null-Hypothese
    <i>df</i> &hellip; Freiheitsgrade (Anzahl unabhaengiger Informationen, die zur Schaetzung  herangezogen weird)
    <i>r</i> &hellip; Korrelationskoeffizient (Mass fuer den linearen Zusammenhang)
          ")
}}
  #----------------------------------------------------------------
if(include.regression){
n<- length(myCitation)
txt_Anhang <-c(txt_Anhang,
     "<B>Regressionsanalyse</B>
      Ist eine statistische Analyse, die mehrere Ergebnisvariable gleichzeitig beruecksichtigt. Die Regressionsanalyse dient der Analyse von Beziehungen zwischen einer abhaengigen Variable und mehreren unabhaengigen Variablen."
    )
txt_abk <-c(txt_abk,
   "<b>Regressionsanalyse</b>
    <i>estimate</i> &hellip;  Erwartungswert der Koeffizienten
    <i>std. error</i>  &hellip; Standardfehler
    <i>p</i> &hellip; p-Wert, Probability-Wert Wahrscheinlichkeit der Null-Hypothese
    ")

  }
if(include.lm){
txt_Anhang <-c(txt_Anhang, "")
txt_abk <-c(txt_abk,
  "<b>lineare Regression</b>
  <i>T, t, t-value</i> &hellip; Wert der Teststatistik H0: beta-Gewicht = 0
  <i>R2, adj. R2</i> &hellip; Bestimmtheitsmass
  <i>Obs, Num. obs.</i> &hellip; Beobachtungen
  <i>beta, &beta;</i> &hellip; standartisierter Koeffizient
  <i>F</i> &hellip; F-Wert
  <i>eta. Squared, &eta;</i>  &hellip; Effektstaerke (ANOVA)

Wichtige Voraussetzungen nach Gelman Hill:
Das Modell ist linear in den Parametern.
Die Residuen sind normalverteilt und im Mittel sind die Residuen null.
Die Zahl der Beobachtungen muss groesser sein als als die Zahl der zu schaetzenden Parameter: n > k.
")

  }
if(include.rangreihe){
    txt_Anhang <-c(txt_Anhang,
                   "<br><b>Rangreihen</b>
                   Rangordnungen von Objekten koennen durch eine Transformation der Rangreihen in intervallskalierte Merkmale ueberfuehrt werden. Die Grundidee dieser Methode geht auf Thurstone (1927) nach dem <i>Law of Categorical</i> Judgement zurueck. Dabei werden die kumulierten Haufigkeiten in Normalverteilte z-Werte uebergefuehrt und aus diesen die intervallskalierten Markmalsauspraegungen gebildet. (Vergl Bortz, Doering, 2006 Seite 155) ")
  }

  #------------------------------------------------------------------
if(include.aufbau){
Text("<HR><br>Beispiel fuer den Aufbau einer empirischen Untersuchung (Vergl. Bortz, Doering, 2006 Seite 87). ")
Head("Einleitung / Introduction", style=3)
    Text("Bla bla bla...")
  Head("1.    Forschungsgegenstand und Theorie", style=4)
    Text("Bla bla bla...")
  Head("2.	Methode / Metods", style=3)
  Head("2.1.	Untersuchungsdesign",  style=4)
    Text("Die Studie wurde als bla bla bla...")
  Head("2.2.	Daten / Data Collection", style=4)
    Text("Grundlage der Analyse sind Daten von ... bla bla bla... Studiendurchfuehrung ... bla bla bla...Studienpopulation,
       Einschlusskriterien")
  Head("2.3.	Instrument der Datenerhebung /Instrumente und Messgeraete", style=4)
  Text("Fuer die Erhebung der Daten verwendeten wir ... bla bla bla... Fragebogen. ")
  Head("2.4.	Statistische Verfahren / statistical Modeling", style=4)
  Text("Fuer die Auswertung der Erhebung wird mit deskriptiven und inferenzstatistischen Methoden gearbeitet. Das Signifikanzniveau wird fuer die ganze Untersuchung auf 0.05 festgelegt.")
  Head("3.	Ergebnisse / Results", style=3)
  Head("3.1.	Stichprobenbeschreibung", style=4)
  Text("Insgesamt liegt ein Datensatz mit xx  Faellen... bla bla bla...")
  Head("3.2.	Ergebnisse zu den einzelnen Fragestellungen und Hypothesen", style=4)
  Text("Bla bla bla...")
  Head("3.3.	Weitere Befunde", style=4)
  Text("Bla bla bla...")
  Head("4.	Diskussion", style=3)
  Text("Bla bla bla...")
  Head("5.	Literatur / References", style=3)}

  if(include.literatur)
     Text(paste(myCitation[1:n], collapse="<BR>"))

 # Head("Anhang")
  if(include.software)
  Text("<B>Software</B><BR>",
       R.version.string,
       "",
       "<BR>Verwendete R-Bibliotheken: ",
       paste(gsub("package:", "", libs[grep("package", libs)]), collapse=", "))



 # Text( txt_Anhang )
# if(include.abkuerzung)
 # Head("Abkuerzungen", style=3)
  #Text( txt_abk )

  invisible(NULL)
  }



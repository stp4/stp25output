#' @rdname Text
#' @param include.literatur Literatur
#' @param include.software Verwendete Software
#' @param include.mean Mittelwerte Prozent
#' @param include.test Signifikanz Test
#' @param include.regression Regression Allgenein
#' @param include.lm,include.binom,include.lme,include.poisso Regression linear, binomial, Hirarchisch, Poisson
#' @param include.rangreihe Rangreihe
#' @param include.structure Uberschriften Aufbau
#' @param include.notation Abkuerzungen
#'
#' @export
Anhang <- function(include.literatur = TRUE,
                   include.software = TRUE,
                   include.mean = FALSE,
                   include.test = FALSE,
                   include.regression = FALSE,
                   include.lm = FALSE,
                   include.binom = FALSE,
                   include.lme = FALSE,
                   include.poisso = FALSE,
                   include.rangreihe = FALSE,
                   include.structure = FALSE,
                   include.notation = FALSE,
                   ...) {
  str_notation <- str_appendix <- ""
  libs <- search()
  myCitation <-  c(
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
 
  
  if (include.mean) {
    str_appendix <- c(
      str_appendix,
      "<B>Deskriptive Analyse</B>
      In den Tabellen zur deskriptive Analyse wird bei ordinalen oder metrischen Skalenniveau. Je nach Kontext der Mittelwert oder Median berechnet, dabei wird die Schreibweise <b>M(SD)</b> oder <b>Median[unteres Quartil, oberes Quartil]</b>, verwendet. Bei Faktoren (Nominal- Skala) wird der Anteil in Prozent angegeben, dabei wird die Schreibweise <b>Prozent(Anzahl)</b> verwendet.
      "
    )
    str_notation <- c(
      str_notation,
      "<b>Deskriptive Analyse</b>
      <i>M</i> &hellip; Mittelwert
      <i>SD</i> &hellip; Standardabweichung
      <i>N, n</i> &hellip; Anzahl (aller) erhobenen Messwerte
      <i>NA, n.a.</i> &hellip; Fehlender Wert nicht anwendbar
      <i>%</i>  &hellip; Prozent, v. H.
      "
    )
  }
  
  if (include.test) {
    str_appendix <- c(
      str_appendix,
      "Wenn ein Signifikanz Test angegeben ist handelt es sich um einen nicht-parametrischen Test der abhaengig vom Skalenniveau entweder ein Chi-Quadrat-test bei Nominal-Skala oder Wilcoxon bzw. Kruskal-Wallis-Test bei ordinalen und metrischen-Skalen ist. (Es werden auf Empfehlungen des APA-Styles die F-Werte mit ausgegeben)"
    )
    str_notation <- c(
      str_notation,
      "<b>Signifikanz Test</b>
      <i>F</i> &hellip; Wilcoxon or Kruskal-Wallis (approximated by using the t or F distributions) Wilcoxon-Mann-Whitney-Test ist identisch mit dem Mann-Whitney-U-Test
      <i>X, &Chi;</i> &hellip; Pearson chi-square test
      <i>p</i> &hellip; p-Wert, Probability-Wert, Wahrscheinlichkeit der Null-Hypothese
      <i>df</i> &hellip; Freiheitsgrade (Anzahl unabhaengiger Informationen, die zur Schaetzung  herangezogen weird)
      <i>r</i> &hellip; Korrelationskoeffizient (Mass fuer den linearen Zusammenhang)
      "
    )
  }
   
  if (include.regression) {
  #  n <- length(myCitation)
    str_appendix <- c(
      str_appendix,
      "<B>Regressionsanalyse</B>
      Ist eine statistische Analyse, die mehrere Ergebnisvariable gleichzeitig beruecksichtigt. Die Regressionsanalyse dient der Analyse von Beziehungen zwischen einer abhaengigen Variable und mehreren unabhaengigen Variablen."
    )
    str_notation <- c(
      str_notation,
      "<b>Regressionsanalyse</b>
      <i>estimate</i> &hellip;  Erwartungswert der Koeffizienten
      <i>std. error</i>  &hellip; Standardfehler
      <i>p</i> &hellip; p-Wert, Probability-Wert Wahrscheinlichkeit der Null-Hypothese
      "
    )
  }
  
  if (include.lm) {
    str_appendix <- c(str_appendix, "")
    str_notation <- c(
      str_notation,
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
      "
    )
  }
  
  if (include.rangreihe) {
    str_appendix <- c(
      str_appendix,
      "<br><b>Rangreihen</b>
      Rangordnungen von Objekten koennen durch eine Transformation der Rangreihen in intervallskalierte Merkmale ueberfuehrt werden. Die Grundidee dieser Methode geht auf Thurstone (1927) nach dem <i>Law of Categorical</i> Judgement zurueck. Dabei werden die kumulierten Haufigkeiten in Normalverteilte z-Werte uebergefuehrt und aus diesen die intervallskalierten Markmalsauspraegungen gebildet. (Vergl Bortz, Doering, 2006 Seite 155) "
    )
  }
  
  if (include.literatur) {
    Text("<B>Literatur</B><BR>")
    Text(paste(myCitation, collapse = "<BR>"))
  }
 
  if (include.software)  {
    Text(
      "<B>Software</B><BR>",
      R.version.string,
      "",
      "<BR>Verwendete R-Bibliotheken: ",
      paste(gsub("package:", "", libs[grep("package", libs)]), collapse =
              ", ")
    )
  }
  
  if (include.structure) {
    Text(
      "<HR><br>Beispiel fuer den Aufbau einer empirischen Untersuchung (Vergl. Bortz, Doering, 2006 Seite 87). "
    )
    Head("Einleitung / Introduction", style = 3)
    Text("Bla bla bla...")
    Head("1.    Forschungsgegenstand und Theorie", style = 4)
    Text("Bla bla bla...")
    Head("2.	Methode / Metods", style = 3)
    Head("2.1.	Untersuchungsdesign",  style = 4)
    Text("Die Studie wurde als bla bla bla...")
    Head("2.2.	Daten / Data Collection", style = 4)
    Text(
      "Grundlage der Analyse sind Daten von ... bla bla bla... Studiendurchfuehrung ... bla bla bla...Studienpopulation,
      Einschlusskriterien"
    )
    Head("2.3.	Instrument der Datenerhebung /Instrumente und Messgeraete",
         style = 4)
    Text("Fuer die Erhebung der Daten verwendeten wir ... bla bla bla... Fragebogen. ")
    Head("2.4.	Statistische Verfahren / statistical Modeling", style = 4)
    Text(
      "Fuer die Auswertung der Erhebung wird mit deskriptiven und inferenzstatistischen Methoden gearbeitet. Das Signifikanzniveau wird fuer die ganze Untersuchung auf 0.05 festgelegt."
    )
    Head("3.	Ergebnisse / Results", style = 3)
    Head("3.1.	Stichprobenbeschreibung", style = 4)
    Text("Insgesamt liegt ein Datensatz mit xx  Faellen... bla bla bla...")
    Head("3.2.	Ergebnisse zu den einzelnen Fragestellungen und Hypothesen",
         style = 4)
    Text("Bla bla bla...")
    Head("3.3.	Weitere Befunde", style = 4)
    Text("Bla bla bla...")
    Head("4.	Diskussion", style = 3)
    Text("Bla bla bla...")
    Head("5.	Literatur / References", style = 3)
  }
  
  invisible(NULL)
  }

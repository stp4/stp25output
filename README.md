
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stp25output

Die Scripten dienen der Erstellung von Reports als HTML aber auch von
PDF ueber knit. Zum Teil handelt es sich um modifizerte Funktionen von
Hmisc, car, reshape2, R2HTML, texreg oder htmlTable.

## Overview

  - `Projekt` und `End` Initialisiert ein neues Projekt und aufruf der
    HTML-Seite.
  - `Output` Erstellt Tabellen
  - `Text`, `HTML_P` und `HTML_...` Schreibt HTML Text.
  - `SaveDate` Speichert Bilder in den Arbeitsfolder.
  - `fix_irgendwas` Repariert Levels und Labels und Zahlen fuer die
    Ausgabe.
  - `set_my_options` und `get_my_options` Formatierungs-Optionen

### Usage

``` r
# devtools::install_github("stp4/stp25output")
require(stp25vers)
#> Loading required package: stp25vers
set_my_options(prozent=list(digits=c(1,0), style=2))
get_my_options()$apa.style$prozent
#> $digits
#> [1] 1 0
#> 
#> $lead.zero
#> [1] TRUE TRUE
#> 
#> $style
#> [1] 2
#> 
#> $percentage_sign
#> [1] TRUE
#> 
#> $percentage_str
#> [1] "%"
#> 
#> $null_percent_sign
#> NULL
 
#Projekt("html", "Beispiel Projekt")
Projekt("", "Beispiel Projekt")
#> 
#> Speichere Abbildungen in  Fig/ 
#> 
#> Kontraste von  contr.treatment, contr.poly auf  contr.Treatment, contr.poly  umgestellt!
#> Beispiel Projekt 
#>  Datum:  Tue Jan 30 09:33:18 2018 , Software:  R version 3.4.3 (2017-11-30) , Link: www.R-project.org/
#> File:  README.Rmd
APA2(.~ gruppe , hkarz, caption="Deskriptive Analyse")
#> 
#> 
#> Table: gruppe Deskriptive Analyse
#> 
#> Item    N    krank (n=24)   gesund (n=21) 
#> ------  ---  -------------  --------------
#> tzell   45   63.85 (5.61)   71.27 (4.84)  
#> lai     45   0.12 (0.34)    0.76 (0.44)

End()
#> 
#> Reset Kontraste
```
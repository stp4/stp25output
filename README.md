
<!-- README.md is generated from README.Rmd. Please edit that file -->
stp25output
===========

Laden der Experimentelle Funktionen von Statistik-Peter

Overview
--------

-   `Projekt()` Initialisiert ein neues Projekt
-   `APA2()` Erstellt Tabellen
-   `Text()` Schreibt HTML Text
-   `End()` Aufruf der Seite

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
#>  Datum:  Thu Jan 25 11:37:48 2018 , Software:  R version 3.4.3 (2017-11-30) , Link: www.R-project.org/
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

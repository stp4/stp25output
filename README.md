
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stp25output

Die Scripten dienen der Erstellung von Reports als HTML aber auch von
PDF ueber knit. Zum Teil handelt es sich um modifizerte Funktionen von
Hmisc, car, reshape2, R2HTML, texreg und htmlTable.

## Overview

  - Initialisiert von neuen Projekten
      - `Projekt()` und `End()` Initialisiert ein neues Projekt und
        aufruf der HTML-Seite mit Hilfe von R2HTML.
      - `HTML_Start()` und `HTML_End()` Wie Projekt aber ohne den ganzen
        Überhau von R2HTML.
  - Text und Tabellen-Formatierung
      - `Output` Erstellt Tabellen
      - `Text`, `HTML_P` und `HTML_...` Schreibt HTML Text.
  - Hilfsfunktionen
      - `SaveDate` Speichert Bilder in den Arbeitsfolder.
      - `fix_irgendwas` Repariert Levels und Labels und Zahlen fuer die
        Ausgabe.
      - `set_my_options` und `get_my_options` Formatierungs-Optionen

### Usage

``` r
# devtools::install_github("stp4/stp25output")
require(stpvers)
#> Loading required package: stpvers
Projekt("", "Beispiel Projekt")
#> 
#> Kontraste von  contr.treatment, contr.poly auf
#>  contr.Treatment, contr.poly  umgestellt!
#> 
#> set: output =
set_my_options(prozent=list(digits=c(1,0), style=2))
 

# Optionen format:  HTML, Spin, Knit, Rpres oder Text
APA2(.~ gruppe , hkarz, caption="Deskriptive Analyse")
#> 
#>  Tab 1:  
#>    Item  N krank (n=24) gesund (n=21)
#> 1 tzell 45 63.85 (5.61)  71.27 (4.84)
#> 2   lai 45  0.12 (0.34)   0.76 (0.44)
#> 
#> 

End()
#> 
#> Reset Kontraste
```

``` r
 knitr::kable(
          df1, row.names = FALSE,
          format = "pandoc" 
        )
```

| term |  n | m         |
| :--- | -: | :-------- |
| A    | 23 | 4.7 (2.4) |
| B    | 14 | 4.1 (2.3) |
| C    | 56 | 8.9 (3.6) |
| D    |  2 | NA        |

``` r

knitr::kable(
  df1, row.names = FALSE,
  format = "markdown" 
)
```

| term |  n | m         |
| :--- | -: | :-------- |
| A    | 23 | 4.7 (2.4) |
| B    | 14 | 4.1 (2.3) |
| C    | 56 | 8.9 (3.6) |
| D    |  2 | NA        |

``` r
 knitr::kable(
          df1, row.names = FALSE,
          format = "pandoc" 
        )
```

| term |  n | m         |
| :--- | -: | :-------- |
| A    | 23 | 4.7 (2.4) |
| B    | 14 | 4.1 (2.3) |
| C    | 56 | 8.9 (3.6) |
| D    |  2 | NA        |

``` r
#+ 
df1 %>% Output()
#> 
#>  Tab 1:  
#>   Quelle  n         m
#> 1      A 23 4.7 (2.4)
#> 2      B 14 4.1 (2.3)
#> 3      C 56 8.9 (3.6)
#> 4      D  2      <NA>
#> 
#> 


df1 %>% Output(output=TRUE)
#> 
#>  Tab 2:  
#>   Quelle  n         m
#> 1      A 23 4.7 (2.4)
#> 2      B 14 4.1 (2.3)
#> 3      C 56 8.9 (3.6)
#> 4      D  2      <NA>
#> 
#> 

df1 %>% Output(output=FALSE)
#> NULL
```

``` r

df1 %>% Output(output="html")
```

<table class="gmisc_table" style="border-collapse: collapse; padding-left: .5em; padding-right: .2em;">

<thead>

<tr>

<td colspan="3" style="text-align: left;">

Tab
3:

</td>

</tr>

<tr>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

Quelle

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

n

</th>

<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">

m

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

A

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

23

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

4.7 (2.4)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

B

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

14

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

4.1 (2.3)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

C

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

56

</td>

<td style="padding-left: .5em; padding-right: .2em; text-align: left;">

8.9
(3.6)

</td>

</tr>

<tr>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

D

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

2

</td>

<td style="padding-left: .5em; padding-right: .2em; border-bottom: 2px solid grey; text-align: left;">

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td colspan="3">

</td>

</tr>

</tfoot>

</table>

``` r
#+
df1 %>% Output(output="text")
#> 
#>  Tab 4:  
#>   Quelle  n         m
#> 1      A 23 4.7 (2.4)
#> 2      B 14 4.1 (2.3)
#> 3      C 56 8.9 (3.6)
#> 4      D  2      <NA>
#> 
#> 
```

``` r
 
df1 %>% Output(output="markdown")
```

<table class="table" style="margin-left: auto; margin-right: auto;">

<caption>

Tab 5:

</caption>

<thead>

<tr>

<th style="text-align:left;">

Quelle

</th>

<th style="text-align:right;">

n

</th>

<th style="text-align:left;">

m

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

A

</td>

<td style="text-align:right;">

23

</td>

<td style="text-align:left;">

4.7 (2.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

B

</td>

<td style="text-align:right;">

14

</td>

<td style="text-align:left;">

4.1 (2.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

C

</td>

<td style="text-align:right;">

56

</td>

<td style="text-align:left;">

8.9 (3.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

D

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

</td>

</tr>

</tbody>

</table>

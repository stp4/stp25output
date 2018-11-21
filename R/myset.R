#' MySet
#'
#' Initialisiert Lattice ladet die noetigen Pakete und Formatiert die Lattice Grafiken
#' Einstellen der Farben und Symbole ueber \link{trellis.par.set}
#' @param col Farben brewer.pal(9,"Set1")
#' @param pch  Symbole
#' @param lty Striche und Lienien
#' @param cex Punkt in Boxplots
#' @param col.bar Farbe von einzelnen Balken
#' @param n anzahl an Farben default ist 4
#' @param h.start  Farben = 120,
#' @param theme LaticeExtra ggplot2like(), #theEconomist.theme() custom.theme()
#' @param axis.grid  Referenzlinien = FALSE,
#' @param knit einstellung fuer knitter
#' @param ...  nicht verwendet
#' @return Gibt opar zureuck (Wird fue reset benoetigt)
#' @export
#' @importFrom lattice show.settings  trellis.par.set trellis.par.get lattice.options trellis.device
#' @importFrom latticeExtra ggplot2like ggplot2like.opts
#' @examples
#' #graphics.off()
#'
#'
#'    lattice::trellis.par.set(effects::effectsTheme())
#'
# pch = 15:18
# lty = 1:3
# cex = 1
# axis.grid = FALSE
#
# lattice::trellis.par.set(latticeExtra::ggplot2like(n = 4, h.start = 120))
# col <- lattice::trellis.par.get()$superpose.polygon$col
# col.bar <- lattice::trellis.par.get()$plot.polygon$col
#
# lattice::trellis.par.set(
#   axis.text = list(
#     cex = 0.8,
#     lineheight = 0.9,
#     col = "grey20"
#   ),
#   superpose.symbol = list(col = col, pch = pch),
#   superpose.polygon = list(col = col, border = "transparent"),
#   plot.polygon = list(col = col.bar),
#   superpose.line = list(col = col, lty = lty),
#   box.dot = list(pch = 19, cex = cex),
#   plot.symbol = list(pch = 1)
# )
#
#
# if (axis.grid)
#   lattice::lattice.options(latticeExtra::ggplot2like.opts())
#
# lattice::trellis.par.set(effectsTheme())
#'
#'
#'
#'
#' #library(RColorBrewer)
#' # Set3 rosa-himmelblau
#' # brewer.pal(8,"Set3")[c(3,4)]
#'
#'
#'  #- display.brewer.pal(n = 4, name = 'Set2')
#'  #-
#'  #- brewer.pal.info
#'  #- Diverging palettes
#'  #-          maxcolors category colorblind
#'  #- BrBG            11      div       TRUE  braun-gruen
#'  #- PiYG            11      div       TRUE  violet-gruen
#'  #- PRGn            11      div       TRUE  violet-gruen
#'  #- PuOr            11      div       TRUE  braun-violett
#'  #- RdBu            11      div       TRUE  rot-blau
#'  #- RdGy            11      div      FALSE  rot-grau
#'  #- RdYlBu          11      div       TRUE  rot-blau
#'  #- RdYlGn          11      div      FALSE  rot-gruen
#'  #- Spectral        11      div      FALSE  rot-blau
#'  #- Qualitative palettes
#'  #-          maxcolors category colorblind
#'  #- Accent           8     qual      FALSE
#'  #- Dark2            8     qual       TRUE
#'  #- Paired          12     qual       TRUE
#'  #- Pastel1          9     qual      FALSE
#'  #- Pastel2          8     qual      FALSE
#'  #- Set1             9     qual      FALSE
#'  #- Set2             8     qual       TRUE
#'  #- Set3            12     qual      FALSE
#'  #- Sequential palettes
#'  #-          maxcolors category colorblind
#'  #- Blues            9      seq       TRUE
#'  #- BuGn             9      seq       TRUE
#'  #- BuPu             9      seq       TRUE
#'  #- GnBu             9      seq       TRUE
#'  #- Greens           9      seq       TRUE
#'  #- Greys            9      seq       TRUE
#'  #- Oranges          9      seq       TRUE
#'  #- OrRd             9      seq       TRUE
#'  #- PuBu             9      seq       TRUE
#'  #- PuBuGn           9      seq       TRUE
#'  #- PuRd             9      seq       TRUE
#'  #- Purples          9      seq       TRUE
#'  #- RdPu             9      seq       TRUE
#'  #- Reds             9      seq       TRUE
#'  #- YlGn             9      seq       TRUE
#'  #- YlGnBu           9      seq       TRUE
#'  #- YlOrBr           9      seq       TRUE
#'  #- YlOrRd           9      seq       TRUE
#'
#'  #Projekt( )
#'  #MySet(brewer.pal(9,"Set1")[c(8,2)], col.bar=3 )
#'  #windows(8,8)
#'  #  show.settings()
#'  #windows(7,4)
#'  #  bwplot(yield ~ site|year, barley )
#'  #  End()
#'  
MySet <- function(col = NULL,
                  pch = 15:18,
                  lty = 1:3,
                  cex = 1,
                  col.bar = NULL,
                  n = if (is.numeric(col))
                    col
                  else
                    4,
                  h.start = 120,
                  theme = latticeExtra::ggplot2like(n = n, h.start = h.start),
                  axis.grid = FALSE,
                  knit = stp25output::which_output() == "markdown",
                  ...) {
  require(lattice)
  require(latticeExtra)
  require(RColorBrewer)
  require(effects)
  
  # Speichert die Default einstellungen zum zuruecksetzen der Optionen
  if (exists("opar")) {
    cat(
      "\n  opar existiert und die Einstellungen trellis.par.set() werden zurueckgesetzt.\n"
    )
    lattice::trellis.par.set(opar)
    if (exists("oopt")) {
      cat(
        "oopt existiert und die Einstellungen trellis.par.set() werden zurueckgesetzt.\n"
      )
      lattice::lattice.options(oopt) #Lattice Options fuer Grid
    }
  }
  else{
    cat("\n Erstmaliger Aufruf von trellis.par.set()\n")
    opar <<- lattice::trellis.par.get()
    
    if (!knit) {
      cat("Initialisiert trellis.device",
          getOption("device"),
          "\n")
      lattice::trellis.device() #-- new=FALSE
      print(lattice::show.settings())
    }
    
  }
  
  if (!is.null(theme)) {
    lattice::trellis.par.set(theme)
    cat("Theme set\n")
    if (is.null(col) | is.numeric(col))
      col <- lattice::trellis.par.get()$superpose.polygon$col
    else if (is.character(col) & col[1] == "sex")
      col <- brewer.pal(8, "Set3")[c(3, 4)] # Set3 rosa-himmelblau
    
    cat("Farbe: ", paste(col, collapse = ", "))
    
    if (is.null(col.bar))
      col.bar <- lattice::trellis.par.get()$plot.polygon$col
    
    lattice::trellis.par.set(
      axis.text = list(
        cex = 0.8,
        lineheight = 0.9,
        col = "grey20"
      ),
      superpose.symbol = list(col = col, pch = pch),
      superpose.polygon = list(col = col, border = "transparent"),
      plot.polygon = list(col = col.bar),
      superpose.line = list(col = col, lty = lty),
      box.dot = list(pch = 19, cex = cex),
      plot.symbol = list(pch = 1)
      # box.rectangle =list(),
      # box.umbrella = list(),
    )
    cat("Eigene Param (lineheight...\n")
    
    if (axis.grid) {
      oopt <<-
        lattice::lattice.options(latticeExtra::ggplot2like.opts())
      
      cat("Theme axis.grid set\n")
    }
  }
  
  cat("done\n\n")
  NULL
}

#' @rdname End
#' @title End Funktion
#' @name End
#' @description Diese Funktion Projektet den Browser.
#' \code{browser = getOption("browser")}  Aenderung wegen eines bugs in htmlTable
#  beim Copy&paste gehen die Linien verloren (betrifft Chrome)
#' @param anhang Ja nein
#' @param browser Ie oder Chrome
#' @param output TRUE oder FALSE
#' @param ... weitere Objekte nicht benutzt
#' @return A \code{\link[tibble]{tibble}} with counted tagged NA values.
#' @export
End <- function(anhang=FALSE,
                browser = "C:/Program Files (x86)/Internet Explorer/iexplore.exe",
                output = options()$prompt[1] == "HTML> ",
                ...) {
 # getOption("contrasts")


  options(contrasts = c("contr.treatment", "contr.poly"))
  #- Reset
  stp25_options()
  if( exists( "opar" )) lattice::trellis.par.set(opar)
  if( exists( "oopt" )) lattice::lattice.options(oopt)
  if( exists( "Tab_Index" )) Tab_Index <<-  0
  if( exists( "Abb_Index" )) Abb_Index <<-  0

  file<- try( R2HTML::HTMLGetFile(), silent = TRUE)
  if(output & class(file) !=  "try-error") {
      if (anhang){ Anhang() }

     R2HTML::HTMLStop()
     #   print(tmp)
      # getOption("browser")
       browseURL(file, browser=browser)
  } #else {cat("Die Funktion Projekt() mus vorher ausgefuert werden.",  "\n\n")}


   options(prompt="> ")

  if(output) file
   else cat("\nReset Kontraste\n")
}





















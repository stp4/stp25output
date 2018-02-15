#' @rdname Projekt
#' @description  End: Zuruecksetzen der Einstellungen und Aufruf des Browsers browser = getOption("browser") 
#' @param anhang Ja/Nein
#' @param browser Ie oder Chrome
#' @param output TRUE oder FALSE
#' @export
End <- function(anhang=FALSE,
                browser = "C:/Program Files (x86)/Internet Explorer/iexplore.exe",
                output = options()$prompt[1] == "HTML> ",
                ...) {
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





















#' @rdname settings
#' @title Einstellungen
#' @name settings
#' @description Einstellungen stp25, die Einstellungen betreffen vor allem die Formatierung von Zahlen in den Tabellen.
NULL


#' @rdname settings
#' @description set_my_options:  Optionen erstellen
#' @param ... zu aendernde Einstellungen
#' @export

set_my_options <- function(...) {
  
  #Alternative modifyList(x, list(fig_folder="fig3"))
  my_canges <- list(...)

  if (length(my_canges) == 0) {
    cat("\n***\nSet Default Options\n***\n")
    stp25_options()    #stp25output:::default_stp25_opt()
    return(NULL)
  }
  else{
    old_options <-  get_my_options()  # werden beim Laden Initialisiert
    #print( str(my_canges))
    for (i in names(my_canges)) {
      # cat(class(my_canges[i]), i,"\n")
      if (i == "bez" | i == "language") {
        if ( i == "language" ) {
          old_options$bez <- Sprachdatei(my_canges[i])

        }
        if( i == "bez"  ){  # Sprache kann unabhaengig von den einzelnen Woertern geaendert werden
        if (is.null(my_canges[[i]])){
          old_options$bez  <- NULL}
        else{
          for (j in names(my_canges[[i]])) {
            old_options$bez[[j]] <- my_canges[[i]][[j]]}}
          }
      }
      else if (i == "apa.style") {
        return("apa.style nicht notwendig")
      }
      else{
        if (is.list(my_canges[i])) {
          if (is.list(my_canges[[i]]))
            for (j in names(my_canges[[i]])) {
              old_options$apa.style[[i]][[j]] <- my_canges[[i]][[j]]
            }
          else{
            # erste Ebene
            old_options[i] <- my_canges[[i]]
            cat("\nset:", i, "=", my_canges[[i]])
          }
        } else{
          old_options$apa.style[[i]] <- my_canges[[i]]
        }
      }
    }
    options("stp25" = old_options)


  }
}

#' @rdname settings
#' @description get_my_options: Abruf der Optionen
#' @param x Library = "stp25"
#' @export
get_my_options <- function(x = "stp25")
  getOption(x) ## namespace:base>


#' @rdname settings
#' @description set_default_params: Speichern von Variablen im Global Envirment
#' Set default parameters: Stolen from library(ezknitr).
#'
#' @param params List of parameters.
#' @export
set_default_params <- function(params) {
 # if (!exists("Output_info"))
 #   Output_info <<- list(table = c())

  params <- as.list(params)
  env    <-  .GlobalEnv #   parent.frame(1)
  invisible(
    lapply(names(params),
           function(key) {
            assign(key,
                   params[[key]],
                   envir = env, inherits = FALSE)}
           ))
}


#' @rdname settings
#' @description stp25_options: Speichern die Default-Optionen in options$stp25
stp25_options <- function() {
  options("stp25" = default_stp25_opt())
}

#' @rdname settings
#' @description default_stp25_opt: Liste mit den Optionen
#' @param myformat HTML, spin, oder Text  spin ist knitr wobei die Ausgabe der Tabellen mit html erfolgt 
#' @param fig_folder,temp_dir,data_folder,html_folder   Folder Fig, tempdir(), Raw data, Results
#' @param graphics_devices Grafik als PDF speichern
#' @param OutDec Dezimaltrennzeichen
#' @param Align  Zentrieren
#' @param language Sprache
#' @param digits.prozent,digits.mean,digits.r,digits.p,digits.Fstat Digits fuer Format2
#' @param null_percent_sign,lead.zero,p.values,signif.stars,plusmin_sign,sep_element,brackets Diverse Formatierungen
#' @param digits_type,prozent_style,median.style  digits_type,  #- entwerde Runden auf zwei stellen oder auf Signifikante Stellen prozent_style 2=medizin Anzahl(Prozent)
#' @export
#' @examples
#' options("stp25" = default_stp25_opt(language = "en"))
#' 
#' #-- default options
#' set_my_options()
#' get_my_options()$apa.style$prozent
#'
#' #-- Speicherort aendern
#' get_my_options()$fig_folder
#' #set_my_options(fig_folder="Fig2/")
#' get_my_options()$fig_folder
#'
#' #-- Format aendern
#' #set_my_options(prozent=list(digits=c(1,0), style=2))
#'
#' #set_my_options(mittelwert=list(digits=c(1,0), plusmin_sign=TRUE))
#' #set_my_options(prozent = list(null_percent_sign = " . "))
#'
#' #  APA2(~. , hkarz)
#' ##  #Names2Language(c("Item", "Characteristics", "Statistics"  ), "de")
#'
#'
#' # options()$stp25$bez$f.value
#' 
#' 
#' # Impliziet (also nicht als default vorhanen)
#' #set_my_options(mittelwert=list(include_name=FALSE)))
#' 
#' 
default_stp25_opt <-
  function (myformat = "",
            fig_folder = "Fig",
            graphics_devices = "pdf",
            # c(NULL, BMP, JPEG, PNG and TIFF)
            temp_dir =  tempdir(),
            data_folder = "Raw data",
            html_folder = "Results",
         
            OutDec = ".",
            Align = "center",

            language = "de",
            digits.prozent = 0,
            digits.mean = 2,
            digits.r = 2,
            digits.p = 3,
            digits.Fstat = 2,
            null_percent_sign =  NULL,

            lead.zero = FALSE,
            p.values = c(0.001, 0.01, 0.05, 0.1)[1:3],
            signif.stars = FALSE,
            plusmin_sign = FALSE,
            sep_element = ",",
            brackets = c("[", "]"),
            digits_type = "digits",
            prozent_style = 1,
            
            median.style="IQR"
            
            ){
list(language = language,
      dec = OutDec,
      output = myformat,
      fig_folder = paste0(fig_folder, "/"),
      data_folder = paste0(data_folder, "/"),
      graphics_devices = graphics_devices,

      bez = Sprachdatei(language), # Sprachdatei fuer die Tabellen

      apa.style = list(
        OutDec = OutDec,
        digits_type = digits_type,  #- entwerde Runden auf zwei stellen oder auf Signifikante Stellen
        Align = Align,
        center = ifelse(Align == "center", TRUE, FALSE),
        sep_element = sep_element,
        brackets = brackets,

        prozent = list(
              digits = c(digits.prozent, 0),
              lead.zero = c(TRUE, TRUE),
              style = prozent_style, # 2=medizin Anzahl(Prozent)
              percentage_sign = TRUE, #- noch nicht Implementiert
              percentage_str = "%",   #- noch nicht Implementiert
              null_percent_sign = null_percent_sign ## geht nicht  null_percent_sign=null_percent_sign
        ),
        mittelwert = list(
              digits = c(digits.mean, digits.mean),
              lead.zero = c(TRUE, TRUE),
              plusmin_sign = plusmin_sign,
              plusmin_str = intToUtf8(177),
              median.style=median.style
        ),
        Fstat = list(digits = digits.Fstat,
                     lead.zero = TRUE),
        r = list(digits = digits.r,
                 lead.zero = lead.zero),
        r2 = list(digits = digits.r,
                  lead.zero = lead.zero),
        p = list(
                digits = digits.p,
                lead.zero = lead.zero,
                stars.value = p.values,
                stars.symbols = c("***", "**", "*", "'")[1:length(p.values)],
                with.stars = signif.stars
        )))
}

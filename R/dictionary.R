#' Sprachdatei 
#' 
#' Liste mit Uebersaetzung.
#' @param language  Sprache default="de"
#' @param words Wort-liste Deutsch Englisch
#' @name dictionary
#' @export
#' 
Sprachdatei <-
  function(language = "de",

           words = list(
             characteristics =  c("Characteristics", "Bezeichnung"),
             
             statistics = c("Test Statistic", "Test Statistik"),
             statistic = c("Test Statistic", "Test Statistik"),
             sig.test = c("Test Statistic", "Test Statistik"),
             
             mean.Rank = c("mean Rank", "mittlerer Rang"),
             skalenwert = c("Characteristic Value", "Merkmalsauspraegung"),
             items  = c("Items", "Items"),
             item  = c("Item", "Item"),
            
             value = c("Value", "Wert"),
             variable = c ("Variable", "Merkmal"),
             mass= c ("Value", "Anpassungsmass"),
             annforderung= c ("Requirement", "Anforderung"),

             source  = c ("Source", "Quelle"),


             N = c("N", "N"),
             n = c("n", "n"),
             mean = c("Mean", "M"),
             sd = c("SD", "SD"),
             ci95.low = c("low 95% CI", "low 95% CI"),
             ci95.hig = c("up 95% CI", "up 95% CI"),

             se = c("SE", "SE"), #Std. Error
             estimate = c("B", "B"), #Estimate
             est.std = c("Beta", "Beta"),  #std. Estimate
             beta = c("Beta", "Beta"),  #std. Estimate

             df=c("Df", "Df"),
             z.value = c("z", "z"),
             f.value = c("F", "F"),
             t.value  = c ("T", "T"),
             p.value = c("p", "p"),
             lr.chisq = c ("LR Chisq", "LR Chisq"),
             eta = c("eta Sq", "eta Sq"),
             adj.r2 = c("adj. R2", "adj. R2"),
             sum.sq = c("Sum Sq", "Sum Sq"),
             r2 = c("R2", "R2")

           )) {
    lngg <- if (language == "de") 2 else 1
    lapply(words, "[[", lngg)
  }





#' @rdname dictionary
#' @description Names2Language() wird von Output benutzt um die De/En Uebersaetzung durchzufuehren.
#' @export
#' @examples
#' Names2Language(c("Pr..Chisq.", "F.value"))
#' 
Names2Language <- function(words) {

  if(is.null(options()$stp25$language)) return(as.character(words))

 # language <- if (options()$stp25$language == "en") 1 else 2

  tippfehler <-
    list(
      Characteristics =  options()$stp25$bez$characteristics,
      characteristics = options()$stp25$bez$characteristics,

      Statistics = options()$stp25$bez$statistics,
      statistics = options()$stp25$bez$statistics,
      summary= options()$stp25$bez$statistics,
      summary2= options()$stp25$bez$statistics,

      Mean = options()$stp25$bez$mean,
      mean = options()$stp25$bez$mean,
      Mittelwert = options()$stp25$bez$mean,
      SD = options()$stp25$bez$SD,
      sd = options()$stp25$bez$sd,
      mittlerer.Rang = options()$stp25$bez$mean.Rank,

      Skalenwert = options()$stp25$bez$skalenwert,

      Items = options()$stp25$bez$item,
      Item  = options()$stp25$bez$item,

      sig.Test = options()$stp25$bez$sig.test,

      value = options()$stp25$bez$value,
      variable = options()$stp25$bez$variable,

      Anpassungsmass = options()$stp25$bez$mass,
      Anforderung = options()$stp25$bez$annforderung,

      Source = options()$stp25$bez$source,
      model= options()$stp25$bez$source,
      term = options()$stp25$bez$source,

      Std..Error = options()$stp25$bez$se,
      Std.Error = options()$stp25$bez$se,
      "Std. Error" = options()$stp25$bez$se,
      se = options()$stp25$bez$se,
      std.error = options()$stp25$bez$se,

      est.std = options()$stp25$bez$est.std,

      Estimate = options()$stp25$bez$estimate,
      estimate = options()$stp25$bez$estimate,

      DF= options()$stp25$bez$df,
      df= options()$stp25$bez$df,
      numDF= options()$stp25$bez$df,
      Chi.DF= options()$stp25$bez$df,

      t.value  = options()$stp25$bez$t.value,

      p.value = options()$stp25$bez$p.value,
      pvalue = options()$stp25$bez$p.value,
      "p value" = options()$stp25$bez$p.value,
      Pr..Chisq. = options()$stp25$bez$p.value,
      Pr..F.= options()$stp25$bez$p.value,

      LR.Chisq = options()$stp25$bez$lr.chisq,
      eta.Squared = options()$stp25$bez$eta,

      adj.R2 = options()$stp25$bez$adj.r2,
      "F value" = options()$stp25$bez$f.value,

      F.value = options()$stp25$bez$f.value,
      Sum.Sq = options()$stp25$bez$sum.sq,

      "z value" = options()$stp25$bez$z.value,

      X2.5.. = options()$stp25$bez$ci95.low,
      X97.5.. = options()$stp25$bez$ci95.hig,

      conf.low = options()$stp25$bez$ci95.low,
      conf.high = options()$stp25$bez$ci95.hig,
      conf.hig = options()$stp25$bez$ci95.hig,

      n = options()$stp25$bez$n
    )

  new_words <- unlist( tippfehler[words] )

  sapply(words,
         function(x){
                  if (x %in% names(new_words)) new_words[x]
                  else x
                  }
      , USE.NAMES = FALSE)
}


# aus broom
# renamers <- c("Df" = "df",
#               "Sum Sq" = "sumsq",
#               "Mean Sq" = "meansq",
#               "F value" = "statistic",
#               "Pr(>F)" = "p.value",
#               "Res.Df" = "res.df",
#               "RSS" = "rss",
#               "Sum of Sq" = "sumsq",
#               "F" = "statistic",
#               "Chisq" = "statistic",
#               "P(>|Chi|)" = "p.value",
#               "Pr(>Chi)" = "p.value",
#               "Pr..Chisq." = "p.value",
#               "Pr..Chi." = "p.value",
#               "p.value" = "p.value",
#               "Chi.sq" = "statistic",
#               "edf" = "edf",
#               "Ref.df" = "ref.df")
# 
# names(renamers) <- make.names(names(renamers))


#ret <- plyr::rename(x, renamers, warn_missing = FALSE)


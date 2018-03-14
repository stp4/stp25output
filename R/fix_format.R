#' @rdname fix_irgendwas
#' @param exclude Position der auszuschliedenden Spalten
#' @param pattern_N N Muster
#' @param digits exakt nach muster Runden
#' @export
fix_format  <- function(x,
                        exclude=NULL, # Position der auszuschliedenden Spalten
                        digits=NULL, # exakt nach muster Runden

                        # pattern Sonderzeichen mit \\ schreiben!
                        pattern_pval =c("Pr\\(\\>","Pr\\.\\.",
                                        "p\\.value", "p value", "pvalue"),
                        pattern_est =c("Estimate", "Std\\. Error",
                                       "est.std", "se"
                        ),
                        pattern_df=c("Df" ),
                        pattern_N=c( "N", "n"),
                        apa.style=options()$stp25$apa.style,
                        ...){
  x<- as.data.frame(x)  # eventuell entfernen
  vars <- tolower(names(x))

  if(is.null(digits)){
    # - Suchen nach Parameter --------
    pattern_pval <- paste(tolower(pattern_pval), collapse = '|')
    pattern_est  <- paste(tolower(pattern_est), collapse = '|')
    pattern_df   <- paste(tolower(pattern_df), collapse = '|')
    pattern_N    <- paste(tolower(pattern_N), collapse = '|')

    pval         <- which(stringr::str_detect(vars, pattern_pval))
    est          <- which(stringr::str_detect(vars, pattern_est))
    dichte       <- which(stringr::str_detect(vars, pattern_df))
    Anzahl       <- which(stringr::str_detect(vars, pattern_N))
    fstat        <- setdiff(1:length(vars), c(est, pval, dichte))

    if(!is.null(exclude)) {
      pval   <- setdiff(pval, exclude)
      est    <- setdiff(est, exclude)
      dichte <- setdiff(dichte, exclude)
      Anzahl <- setdiff(Anzahl, exclude)
      fstat  <- setdiff(fstat, exclude)
    }

    if(length(pval)>0)
      x[,pval] <- ffpvalue(x[,pval],
                           digits    =  apa.style$p$digits,
                           lead.zero =  apa.style$p$lead.zero,
                           with.stars=  apa.style$p$with.stars)

    if(length(est)>0)
      x[,est] <- Format2(x[,est],
                         digits = 2,  # apa.style$est$digits,
                         lead.zero = TRUE,# apa.style$est$lead.zero,
                         type= "signif"# o apa.style$est$type
      )

    if(length(fstat)>0)
      x[,fstat] <- Format2(x[,fstat],
                           digits =   apa.style$Fstat$digits,
                           lead.zero =   apa.style$Fstat$lead.zero)

    if(length(dichte)>0)
      x[,dichte] <- Format2(x[,dichte],
                            digits =  ifelse(countDigits(x[,dichte])>0,2,1)

      )
  } else{
    x<-Format2(x, digits=digits)
  }


  x
}


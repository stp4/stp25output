#' @rdname fix_irgendwas
#' @description Auto-Format von data.frames.
#' 
#' @param x data.frame
#' @param exclude Position der auszuschliedenden Spalten
#' @param pattern_pval,pattern_est,pattern_df,pattern_N # pattern Sonderzeichen mit \\ schreiben!
#' @param apa.style 
#' @param digits exakt nach muster Runden
#'
#' @export
#' 
#' @examples 
#' 
#' 
#'  df2 <- data.frame(
#' term = c("A", "B", "C", "D"),
#' Estimate = c(23.5, .14, 5.6, 2.9876),
#' df1 = c(3.3, 35., 7.8, 2.1),
#' df = c(3, 35, 7, 2),
#' N = c(33, 35, 78, 21),
#' 
#' F.value = c(2.73, 12.444, 14.576, 30.412),
#' pvalue = c(0.73, 0.044, 0.056, 0.042),   stringsAsFactors =FALSE
#' 
#' 
#' )
#' 
#' x1<-fix_format(df2)
#' 
#' 
fix_format  <- function(x,
                        exclude = NULL,
                        digits = NULL,
                        pattern_pval = c("Pr\\(\\>", 
                                         "Pr\\.\\.",
                                         "p\\.value", 
                                         "p value", 
                                         "pvalue"),
                        pattern_est = c("Estimate", 
                                        "Std\\. Error",
                                        "est.std", 
                                        "se"),
                        pattern_df = c("Df"),
                        pattern_N = c("N", 
                                      "n"),
                        apa.style = options()$stp25$apa.style,
                        ...) {
  x <- as.data.frame(x)  # eventuell entfernen
  vars <- tolower(names(x))
  
  if (is.null(digits)) {
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
    
    if (!is.null(exclude)) {
      pval   <- setdiff(pval, exclude)
      est    <- setdiff(est, exclude)
      dichte <- setdiff(dichte, exclude)
      Anzahl <- setdiff(Anzahl, exclude)
      fstat  <- setdiff(fstat, exclude)
    }
    
    
    
    if (length(Anzahl) > 0)
      x[, Anzahl] <- stp25rndr::rndr2(x[, Anzahl],
                                      digits = 0)
    
    if (length(pval) > 0)
      x[, pval] <- stp25rndr::rndr_P(x[, pval],
                                     include.symbol = FALSE)
    
    
    if (length(est) > 0)
      x[, est] <-
      stp25rndr::rndr2(
        x[, est],
        digits = 2,
        type = "signif" #   apa.style$est$type
        )
        
        if (length(fstat) > 0)
          x[, fstat] <-
          stp25rndr::rndr2(
            x[, fstat],
            digits =   apa.style$Fstat$digits,
            lead.zero =   apa.style$Fstat$lead.zero
          )
        
        if (length(dichte) > 0){
          for(i in dichte)
          x[, i] <-
          stp25rndr::rndr2(x[, i],
                           digits =  ifelse(stp25rndr::countDigits(
                             x[, i]) > 0, 1, 0))
          }
  } else{
    x <- stp25rndr::rndr2(x, digits = digits)
  }
  
  
  x
}

#' @rdname Output
#' @description Output.xtable mit xtable lassen sich alternativ r-Objekte ausgeben. 
#'
#' @examples 
#' 
#'  require(xtable)
#' ## Load example dataset
#' data(tli)
#' 
#' ## Demonstrate aov
#' fm1 <- aov(tlimth ~ sex + ethnicty + grade + disadvg, data = tli)
#' fm1.table <- xtable(fm1)
#' 
#' Out(fm1.table)
#' 
#' # ## Demonstrate lm
#' fm2 <- lm(tlimth ~ sex*ethnicty, data = tli)
#' fm2.table <- xtable(fm2)
#' fm2b <- lm(tlimth ~ ethnicty, data = tli)
#' 
#' Out(fm2.table)
#' Out(xtable(anova(fm2)))
#' Out(xtable(anova(fm2b, fm2)))
#' 
#' 
Output.xtable <- function(x,
                          caption = attr(x, "heading"),
                          digits = attr(x, "digits"),
                          ...) {
  
  caption <-  paste(caption, collapse= " ")
  res <- as.data.frame(x)
  
  res <- cbind(Source = row.names(res), res)
  c_names <- names(res)
  res <- fix_format(res, digits = digits)
  names(res) <- c_names
  
  Output.data.frame(res, caption=caption, ...)
  
}


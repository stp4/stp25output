#bland_altman

#' @rdname Output
#' @description Output.bland_altman fuer eigenes Objekt bland_altman
#' @export
Output.bland_altman<- function(x,
                               caption= paste0("Difference (", x$name.diff,
                                               "), Mean (",  x$name,")"),
                               ...){
  Output(x$stat, caption=caption, ...)
}

print.bland_altman <- function(x, ...){ print(x$stat) }

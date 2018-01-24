#' @rdname Output
#' @description Output.Reliability für eigenes Objekt Reliability
#' @export
Output.Reliability <- function(x,
                               caption = NULL ,
                               note = "",
                               ...) {
  mynames <- stp25APA2:::grap_call_name(x) # name Holen interne Funktion
  if (is.null(caption))
    if (length(grep("Reliability" , mynames)) == 0)
      caption <- paste("", mynames)

    item <-
      data.frame(
        Items = paste0(x$labels, ifelse(x$keys < 0, " (-)", "")),
        n = x$item_statistik$n,
        M = Format2(x$item_statistik$m, 2),
        SD = Format2(x$item_statistik$sd, 2),
        "Alpha if Item Deleted" = Format2(x$psych$item.stats$r.drop, 2)
      )

    aplha_statistik <- with(
      x,
      data.frame(
        Items = Items,
        n = n,
        M = Format2(M, 2),
        SD = Format2(SD, 2),
        Alpha = Format2(Alpha, 2),
        Range = paste(Format2(range, 2), collapse = "; "),
        Skew = Format2(Skew, 2),
        Kurtosi = Format2(Kurtosi, 2) ,
        "Shapiro Test" = shapiro
      ))

    Output(item,
           caption = paste("Itemstatistiken", caption),
           note = note)

    Output(aplha_statistik ,
           caption = paste("Item-Mittelwerte", caption),
           note = note)
}




#' @rdname Output
#' @description Output.bland_altman für eigenes Objekt bland_altman
#' @export
Output.bland_altman<- function(x,
                               caption= paste0("Difference (", x$name.diff,
                                               "), Mean (",  x$name,")"),
                               ...){
  Output(x$stat, caption=caption, ...)
}

print.bland_altman <- function(x, ...){ print(x$stat) }

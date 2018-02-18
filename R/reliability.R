#' @rdname Output
#' @description Output.Reliability fuer eigenes Objekt Reliability
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
        M = stp25APA2::Format2(x$item_statistik$m, 2),
        SD = stp25APA2::Format2(x$item_statistik$sd, 2),
        "Alpha if Item Deleted" = stp25APA2::Format2(x$psych$item.stats$r.drop, 2)
      )
    
    aplha_statistik <- with(
      x,
      data.frame(
        Items = Items,
        n = n,
        M = stp25APA2::Format2(M, 2),
        SD = stp25APA2::Format2(SD, 2),
        Alpha = stp25APA2::Format2(Alpha, 2),
        Range = paste(stp25APA2::Format2(range, 2), collapse = "; "),
        Skew = stp25APA2::Format2(Skew, 2),
        Kurtosi = stp25APA2::Format2(Kurtosi, 2) ,
        "Shapiro Test" = shapiro
      ))
    
    Output(item,
           caption = paste("Itemstatistiken", caption),
           note = note)
    
    Output(aplha_statistik ,
           caption = paste("Item-Mittelwerte", caption),
           note = note)
}
#which_output()
#' @rdname Output
#' @description Output.xtabs und Output.tabl und Output.ftable für Tabellen Objekte
#' @export
Output.table<- function(x,
                        caption =  attr(x, "caption") ,
                        note =  attr(x, "note") , ...) {
 # Output(ftable(x), caption=caption, note=note, ...)
  Output(fix_to_data_frame.table(x),
         caption=caption,
         note=note,
         ...)
  }

#' @rdname Output
#' @export
Output.xtabs<- function(x,
                        caption =  attr(x, "caption") ,
                        note =  attr(x, "note") , ...) {
#  Output(ftable(x),caption=caption, note=note, ...)
  Output(fix_to_data_frame.table(x),
         caption=caption,
         note=note,
         ...)
}


#' @rdname Output
#' @export
Output.ftable <- function(x,
                          caption =  attr(x, "caption") ,
                          note =  attr(x, "note") ,
                          # output = options()$prompt[1] == "HTML> "
                        #  output =  which_output(),
                          ...) {
Output(fix_to_data_frame.ftable(x),
        caption = caption,
        note =  note, ...)

}

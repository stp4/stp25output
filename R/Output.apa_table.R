#' @rdname Output
#' @description Output fur apaTables-Objekte wie zB apa.reg.table
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' library(apaTables) 
#'  basic.reg <- lm(sales ~ adverts + airplay, data=album)
#' apa.reg.table(basic.reg) %>% Output()
#' 
#' 
#' lm_output <- lm(libido ~ dose, data=viagra)
#' apa.aov.table(lm_output) %>% Output()
#' 
#' APA_Table(lm_output)
#' 
#' APA_Table( aov(libido ~ dose, data=viagra))
#' 
#' block1 <- lm(sales ~ adverts + airplay, data=album)
#' block2 <- lm(sales ~ adverts*airplay, data=album)
#' apa.reg.table(block1, block2) %>% Output()
#'  } 
#' 
Output.apa_table <- function(x, output=which_output()) {
  names(x$table_body) <- gsub("_", ".", names(x$table_body))
  Output(x$table_body,
         caption = x$table_title,
         note = x$table_note,
         output=output)
  invisible(x$table_body)
}
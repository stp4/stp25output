#' @rdname Output
#' @description Output_word: flextable
#' @export
#' @importFrom flextable regulartable set_header_df merge_h merge_v theme_vanilla
#' 
Output_word <- function(x,
                        caption = NULL,
                        note = NULL,
                        output =  which_output(),
                        print_col = NULL,
                        col_names = NULL,
                        fix_colnames = FALSE,
                        ...) {
  
  
  caption <- Caption(caption, attr(x, "caption"))
  note <- Note(note, attr(x, "note"))
  
  Text(caption)
  tbl <- tbl_header(x, fix_colnames = fix_colnames)
  x <- cleanup_nbsp(x)
  
  if (is.null(tbl$header_above)) {
    ft <- flextable::regulartable(x)
    ft <- flextable::theme_vanilla(ft)
  }
  else {
    tbl$header_above2[1] <- tbl$header[1]
    typology <- data.frame(
      col_keys = names(x),
      what = tbl$header_above2,
      measure = tbl$header,
      stringsAsFactors = FALSE
    )
    ft <- flextable::regulartable(x)
    ft <-
      flextable::set_header_df(ft, mapping = typology, key = "col_keys")
    ft <- flextable::merge_h(ft, part = "header")
    ft <- flextable::merge_v(ft, part = "header")
    ft <- flextable::theme_vanilla(ft)
  }
  ft
}
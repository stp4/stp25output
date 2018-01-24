#' @rdname Output
#' @export
Output.default <- function(x,
                           caption = "", note = "",
                           output = options()$prompt[1] == "HTML> ",
                           # print = TRUE,
                           col_names = NULL,
                           print_col = NULL,
                           ...) {
 cat("\nin Output.default\n")
  print(class(x))
  Caption2 <- function(...) {
    cptn <- gsub(" {2,}", " ", paste(...))
    #  cat("\n", cptn)
    Output_info$table <<-  c(Output_info$table, cptn)
    cptn
  }

  if (output) {
    ##htmlreg2Print2
    if (is.list(x) ) {
      for (i in names(x)) {
        if (!is.null(print_col))
          x[[i]] <- x[[i]][, print_col]
        else if (all(x[[i]][, 2] == ""))  {
          mynames <- names(x[[i]])
          x[[i]] <- x[[i]][, -2]
          names(x[[i]]) <- mynames[-2]
        }
        names(x[[i]]) <-
          find_col_names(col_names, names(x[[i]]), fix_colnames)
        htmlreg2Print2(
          x[[i]],
          caption = Caption2(Tab(), i, caption),
          caption.above = TRUE,
          inline.border = FALSE,
          note = note,
          center = TRUE
        )
      }
    } else if (is.matrix(x)) {
      Text("is.matrix")
      tab <- as.data.frame.matrix(x)
      tab <- cbind(" " = rownames(x), tab)
      htmlreg2Print2(
        tab,
        caption =  Caption2(Tab(), caption),
        caption.above = TRUE,
        inline.border = FALSE,
        note = note,
        center = TRUE
      )
    } else {
      # Text("else")
      if (!is.null(print_col))
        x <- x[, print_col]
      else if (all(x[, 2] == "")) {
        mynames <- names(x)
        x <- x[, -2]
        names(x) <- mynames[-2]
      }## x <- x[,-2] ## Spalte charactristik loeschen
      #  names(x) <- if(!is.null(col_names)) col_names else Names2Language(names(x))
      names(x) <-
        find_col_names(col_names, names(x), fix_colnames)

      htmlreg2Print2(
        x,
        caption = Caption2(Tab(), caption),
        caption.above = TRUE,
        inline.border = FALSE,
        note = note,
        center = TRUE
      )
    }
    x <- caption
    Text("")    #-- Leerzeichen zum besseren kopieren
  } else {
    if (!is.data.frame(x) & is.list(x)) {
      ##  ?
      for (i in names(x)) {
        if (!is.null(print_col))
          x[[i]] <- x[[i]][, print_col]
        else if (all(x[[i]][, 2] == "")) {
          mynames <- names(x[[i]])
          x[[i]] <- x[[i]][, -2]
          names(x[[i]]) <- mynames[-2]
        }
        names(x[[i]]) <-
          find_col_names(col_names, names(x[[i]]), fix_colnames)
      }
    }
    else if (is.data.frame(x)) {
      if (!is.null(print_col)) {
        x <- x[, print_col]
      }
      else if (all(x[, 2] == ""))  {
        mynames <- names(x)
        x <- x[, -2]
        names(x) <- mynames[-2]
      } else {
        cat("\n", class(x), "\n")
      }
      names(x) <-
        find_col_names(col_names, names(x), fix_colnames)
    }
    else {
      cat("\n", class(x), "\n")
    }
  }
  return(x)
}
#-----------------------------------------------------------------



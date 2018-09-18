#' Transformiere zu data.frame
#' 
#'
#' fix_levels ist fuer dta %>% gather(Parameter, Wert, x1:x5)  %>% fix_levels(dta)
#' fix_colnames ist fuer Formatieren in der Funktion \code{Output()}
#' @param x  vektor liste oder matrix
#' @param ... weiter Argumente
#' @return ein data.frame Objekt
#' @export
fix_to_data_frame <- function(x, ...) {
  UseMethod("fix_to_data_frame")
}

#' @rdname fix_to_data_frame
#' @export
fix_to_data_frame.default<- function(x, ...){

  if (is.matrix(x)){
    if(!is.null( rownames(x)))
      cbind(data.frame(Source=rownames(x), stringsAsFactors = FALSE),
            as.data.frame(x, stringsAsFactors = FALSE))
    else  as.data.frame(x, stringsAsFactors = FALSE)
  }else if (is.data.frame(x) ){ return (x)
  }else if(is.vector(x)){
    if(length(names(x))== length(x))
      as.data.frame(matrix(x,
                           nrow=1,
                           dimnames=list("Source", names(x)))
                    , stringsAsFactors = FALSE)
    else x
  }
}

#' @rdname fix_to_data_frame
#' @param  ncol nicht zum aendern
#' @param nrow nicht zum aendern
#' @param nlength nicht zum aendern
#' @export
fix_to_data_frame.ftable <-
  function(x,
           ncol = length(attributes(x)$col.vars),
           nrow = length(attributes(x)$row.vars),
           nlength = ncol + nrow,
           ...) {
    
    #cat("\n",ncol, "/", nrow, "/", nlength, "\n")
    
    if (nlength == 2) {
      res <- stats:::format.ftable(x,
                                   quote = FALSE,
                                   method =  "row.compact")
      col_names <- c(res[1, 1],
                     paste(res[1, 2], res[1, -c(1:2)], sep = "_"))

      res <- res[-1, -2]
      colnames(res) <- gsub(" +", "", col_names)
      res[, -1] <- gsub(" +", "", res[, -1])
    }
    else if (nlength == 3) {
      res <- stats:::format.ftable(x,
                                   quote = FALSE,
                                   method =  "row.compact")
      col_names <- c(res[1, 1], res[1, 2],
                     paste(res[1, 3], res[1, -c(1:3)], sep = "_"))

      res <- res[-1, -3]
      colnames(res) <- gsub(" +", "", col_names)
    
      res[, -c(1:2)] <- gsub(" +", "", res[, -c(1:2)])
    }
    else
      res <- x
  
   as.data.frame(res, stringsAsFactors = FALSE)
  }



#' @rdname fix_to_data_frame
#' @export
fix_to_data_frame.table <- function(x,...){

  if (length(dimnames(x))==2){
    if(any(names(dimnames(x))=="")){
      col.names<-
        c("Source", colnames(x))
    }else{
    col.names<-
      c(names(dimnames(x))[1],
        paste( names(dimnames(x))[2],
               colnames(x), sep="_"))
}
  res <- as.data.frame.matrix(x,
                              stringsAsFactors=FALSE)
  res <- cbind(Sourse=rownames(res), res,
               stringsAsFactors=FALSE)
  names(res) <- col.names
  }
  else if (length(dimnames(x))==1){
    res<- as.data.frame(x, stringsAsFactors = FALSE)
  } else if(length(dimnames(x))==3){
    res<- fix_to_data_frame.ftable(ftable(x))

  } else stop("Kann in fix_to_data_frame.table keine mehrfach tabellen aufdroeseln!")
  res
}



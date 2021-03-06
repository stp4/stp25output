#' Fix Objekt zu Data.Frame
#' 
#' Repariert Levels und labels und Zahlen
#'
#'  
#'
#' fix_levels ist fuer dta %>% gather(Parameter, Wert, x1:x5)  %>% fix_levels(dta)
#' fix_colnames ist fuer Formatieren in der Funktion \code{Output()}
#' @param x  vektor liste oder matrix
#' @param labels  fix_levels labels als String oder data.frame
#' @param which fix_levels Indexnummer der Variable ncol(x)-1
#' @param measure.vars fix_levels welche Variablen
#' @param col_names fuer fix_colnames
#' @param translate  fuer fix_colnames
#' @param pattern_pval fuer fix_format
#' @param pattern_est  fuer fix_format
#' @param pattern_df  fuer fix_format
#' @param apa.style  fuer fix_format
#' @name fix_irgendwas
#' @return gleiches Objekt
#' @examples
#'   df <- data.frame(Item=c("a", "b"),
#' x=1:2,
#' x2=c(1.2,2.3),
#' beta=  c(.22,.13),
#' est=c(2.4234,.03),
#' p.value=c(0.02456,0.0398))
#'
#' # Format2(df , digits=c( NA,0,1,2,3,4))
#'
#' #fix_format(df,  exclude=2)
#' #fix_format(df,  digits=c( NA,0,1,2,3,4))
NULL





#' @rdname fix_irgendwas
#' @export
fix_colnames<- function(x, col_names=NULL, translate= TRUE){
  names(x) <-
    find_col_names(col_names, names(x), translate)
  x
}

#fix_header<- function(){}

# @rdname fix_irgendwas
# @param ... alles weiter an data.frame
# @export
# fix_data_frame2 <- function(...){
# 
#   ## Hmisc::format.df  #Format a Data Frame or Matrix for LaTeX or HTML
#   fix_format(data.frame(...))
# }



#' @rdname fix_irgendwas
#' @export
fix_levels<- function(x,
                      labels=NULL,
                      which=ncol(x)-1,
                      # id.vars=names(x)[1:(ncol(x)-2)],
                      measure.vars= unique(x[,which]),
                      ...
){
  if(is.null(labels)) {
    x[, which] <-  factor(x[, which], measure.vars)
  }
  else {

    if(is.data.frame(labels)){
      value_labels <- stp25aggregate::GetLabelOrName(labels[measure.vars])
    }else{
      value_labels<- labels[measure.vars]
    }
    if (!is.null(value_labels))
      x[, which] <-  factor(x[, which],
                            names(value_labels),
                            as.character(value_labels)
                            )

    # nicht benutzt weil gather() das selbst erledigt
    #   group_labels <- GetLabelOrName(data_o[id.vars])
    #   if (!is.null(group_labels))
    #   x <- upData2(x, labels = group_labels)
  }
  x
}




# -- Ueberaetzt oder aendert die Namen ----------
find_col_names <- function(cl_nms = NULL,
                           nms,
                           translate = FALSE) {

  if (translate)
    nms <- Names2Language(nms)
  
  if (is.null(cl_nms))
    return(nms)
  else if (length(cl_nms) == length(nms))
    return(cl_nms)
  else if (is.null(names(cl_nms)))
    return(nms)
  else{
    nms[cl_nms] <- names(cl_nms)
    return(nms)
  }
}


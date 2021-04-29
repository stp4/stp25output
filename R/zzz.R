.onLoad <- function(libname, pkgname)
{
  options("stp25" = default_stp25_opt() )
}



#' @importFrom graphics par
#' @importFrom plyr llply
#' @importFrom utils browseURL citation modifyList read.table
#' @importFrom stats ftable
#' @importFrom stringr str_split
#' @importFrom htmlTable htmlTable


 
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom Hmisc Cs
#' @export
Hmisc::Cs

#' @importFrom car contr.Treatment
#' @export
car::contr.Treatment
#' @importFrom car contr.Sum
#' @export
car::contr.Sum
#' @importFrom car contr.Helmert
#' @export
car::contr.Helmert


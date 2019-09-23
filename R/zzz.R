


.onLoad <- function(libname, pkgname)
{
  options("stp25" = default_stp25_opt() )
}


#geloescht 



# when_attached <- function(pkg, action) {
#   if (is_attached(pkg)) {
#     action
#   } else {
#      setHook(packageEvent(pkg, "attach"), function(...) action)
#   }
# }
# 
# is_attached <- function(pkg) paste0("package:", pkg) %in% search()
# 


 
# Start a new environment to hold the session key so all other functions can access it
# See http://trestletech.com/2013/04/package-wide-variablescache-in-r-package/

# session_stp <- new.env(parent = emptyenv())



#' updateList
#' 
#' noch nicht benutzt von Lattice
#' @param x old list
#' @param val new value
#' @noRd
updateList <- function(x, val)
{
  if (is.null(x))
    x <- list()
  modifyList(x, val)
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


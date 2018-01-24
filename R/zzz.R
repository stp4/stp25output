.onLoad <- function(libname, pkgname)
{
  options("stp25" = default_stp25_opt())
}





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



# .onAttach <- function(libname, pkgname) {
#   when_attached("stp5", {
#     packageProjektupMessage("You have loaded stp25 after stp5 - this is likely ",
#                           "to cause problems.",
#                           "please load stp5 first, then stp25:\nlibrary(stp5); library(stp25)")
#
#   })
# }

when_attached <- function(pkg, action) {
  if (is_attached(pkg)) {
    action
  } else {
     setHook(packageEvent(pkg, "attach"), function(...) action)
  }
}

is_attached <- function(pkg) paste0("package:", pkg) %in% search()






# .onAttach <- function(libname, pkgname) {
#
# }
#
# when_attached <- function(pkg, action) {
#
# }
#
# is_attached <- function(pkg) paste0("package:", pkg) %in% search()




# Misc --------------------------------------------------------------------

# countDigits<- function(x, sin){
#   x<- strsplit(as.character(signif(x, digits = 4)),"\\.")[[1]][2]
#   if(is.na(x)) 0 else nchar(x)
# }



## update elements of a list recursively. Used in updating trellis or
## lattice settings using trellis.par.set and lattice.options
## respectively
#- noch nicht benutzt von Lattice
updateList <- function(x, val)
{
  if (is.null(x)) x <- list()
  modifyList(x, val)
}

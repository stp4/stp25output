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

when_attached <- function(pkg, action) {
  if (is_attached(pkg)) {
    action
  } else {
     setHook(packageEvent(pkg, "attach"), function(...) action)
  }
}

is_attached <- function(pkg) paste0("package:", pkg) %in% search()


## update elements of a list recursively. Used in updating trellis or
## lattice settings using trellis.par.set and lattice.options
## respectively
#- noch nicht benutzt von Lattice
updateList <- function(x, val)
{
  if (is.null(x)) x <- list()
  modifyList(x, val)
}

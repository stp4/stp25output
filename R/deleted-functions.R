#' deleted Functions
#' ## ## Transformiere zu data.frame
## ## 
## ##
## ## fix_levels ist fuer dta %>% gather(Parameter, Wert, x1:x5)  %>% fix_levels(dta)
## ## fix_colnames ist fuer Formatieren in der Funktion \code{Output()}
## ## @param x  vektor liste oder matrix
## ## @param ... weiter Argumente
## ## @return ein data.frame Objekt
## ## @export
## ## 
## fix_to_data_frame <- function(x, ...) {
##   UseMethod("fix_to_data_frame")
## }
## 
## ## @rdname fix_to_data_frame
## ## @export
## fix_to_data_frame.default <- function(x, ...) {
##   if (is.matrix(x)) {
##     if (!is.null(rownames(x)))
##       x <-   cbind(
##         data.frame(Source = rownames(x), 
##                    stringsAsFactors = FALSE),
##         as.data.frame(x, stringsAsFactors = FALSE)
##       )
##     else
##       x <-  as.data.frame(x, stringsAsFactors = FALSE)
##     # } else if (is.data.frame(x)) {
##     #   x
##   } else if (is.vector(x)) {
##     if (length(names(x)) == length(x))
##       x <- as.data.frame(matrix(x,
##                                 nrow = 1,
##                                 dimnames = list("Source", names(x)))
##                          , stringsAsFactors = FALSE)
##     #else
##     # x
##   }
##   
##   tibble::as_tibble(x)
##   
## }
## 
## ## @rdname fix_to_data_frame
## ## @param  ncol nicht zum aendern
## ## @param nrow nicht zum aendern
## ## @param nlength nicht zum aendern
## ## @export
## fix_to_data_frame.ftable <-
##   function(x,
##            ncol = length(attributes(x)$col.vars),
##            nrow = length(attributes(x)$row.vars),
##            nlength = ncol + nrow,
##            ...) {
##     
##     #cat("\n",ncol, "/", nrow, "/", nlength, "\n")
##     
##     if (nlength == 2) {
##       res <- stats:::format.ftable(x,
##                                    quote = FALSE,
##                                    method =  "row.compact")
##       col_names <- c(res[1, 1],
##                      paste(res[1, 2], res[1, -c(1:2)], sep = "_"))
## 
##       res <- res[-1, -2]
##       colnames(res) <- gsub(" +", "", col_names)
##       res[, -1] <- gsub(" +", "", res[, -1])
##     }
##     else if (nlength == 3) {
##       res <- stats:::format.ftable(x,
##                                    quote = FALSE,
##                                    method =  "row.compact")
##       col_names <- c(res[1, 1], res[1, 2],
##                      paste(res[1, 3], res[1, -c(1:3)], sep = "_"))
## 
##       res <- res[-1, -3]
##       colnames(res) <- gsub(" +", "", col_names)
##     
##       res[, -c(1:2)] <- gsub(" +", "", res[, -c(1:2)])
##     }
##     else
##       res <- x
##   
##  #  as.data.frame(res, stringsAsFactors = FALSE)
##    
##    tibble::as_tibble(res)
##   }
## 
## 
## 
## ## @rdname fix_to_data_frame
## ## @export
## fix_to_data_frame.table <- function(x,...){
## 
##   if (length(dimnames(x))==2){
##     if(any(names(dimnames(x))=="")){
##       col.names<-
##         c("Source", colnames(x))
##     }else{
##     col.names<-
##       c(names(dimnames(x))[1],
##         paste( names(dimnames(x))[2],
##                colnames(x), sep="_"))
## }
##   res <- as.data.frame.matrix(x,
##                               stringsAsFactors=FALSE)
##   res <- cbind(Sourse=rownames(res), res,
##                stringsAsFactors=FALSE)
##   names(res) <- col.names
##   }
##   else if (length(dimnames(x))==1){
##     res<- as.data.frame(x, stringsAsFactors = FALSE)
##   } else if(length(dimnames(x))==3){
##     res<- fix_to_data_frame.ftable(ftable(x))
## 
##   } else stop("Kann in fix_to_data_frame.table keine mehrfach tabellen aufdroeseln!")
##   res
##   
##   tibble::as_tibble(res)
## }
## 
## 



# @rdname Output
# @description Output.matrix: umwandeln in einen data.frame
# @export
# 
# Output.matrix <- function(x,
#                           output =  which_output(),
#                           ...) {
#   Output(fix_to_data_frame(x), output=output, ...)
# }



# @rdname Output
# @description Output.stp25: experimenteller Prototyp
# @export
# 
# Output.stp25 <- function(x,
#                          output =  which_output(),
#                          ...) {
#   # noch nicht getestet
#   if (is.list(x)) {
#     for (i in i:length(x))
#       Output.data.frame(x[[i]], output=output, ...)
#   }
#   else
#     Output.data.frame(x, output=output, ...)
# }

## #which_output()
## ## @rdname Output
## ## @description Output.xtabs und Output.tabl und Output.ftable fuer Tabellen Objekte
## ## @export
## Output.table<- function(x,
##                         caption =  attr(x, "caption") ,
##                         note =  attr(x, "note") , ...) {
##  # Output(ftable(x), caption=caption, note=note, ...)
##   Output(fix_to_data_frame.table(x),
##          caption=caption,
##          note=note,
##          ...)
##   }
## 
## ## @rdname Output
## ## @export
## Output.xtabs<- function(x,
##                         caption =  attr(x, "caption") ,
##                         note =  attr(x, "note") , ...) {
## #  Output(ftable(x),caption=caption, note=note, ...)
##   Output(fix_to_data_frame.table(x),
##          caption=caption,
##          note=note,
##          ...)
## }
## 
## 
## ## @rdname Output
## ## @export
## Output.ftable <- function(x,
##                           caption =  attr(x, "caption") ,
##                           note =  attr(x, "note") ,
##                           # output = options()$prompt[1] == "HTML> "
##                         #  output =  which_output(),
##                           ...) {
## Output(fix_to_data_frame.ftable(x),
##         caption = caption,
##         note =  note, ...)
## 
## }

#' get.1.sub function
#' 
#' check ordinal if they are ordinal
#' @param Click here and there
#' @keywords ordinal
#' @keywords
#' @export
#' @examples 
#' get.1.sub(data,split,cols,whichcol,replace)
get.1.sub<-function(data,split,cols,whichcol,replace) { #get.1.sub(data,split,cols,whichcol,replace)
get.1(sub(replace,split,data),split,cols,whichcol)
}


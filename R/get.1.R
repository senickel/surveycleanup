#' get.1 function
#'
#' check ordinal if they are ordinal
#' @param Click here and there
#' @keywords ordinal
#' @keywords
#' @export
#' @examples 
#' get.1(data,split,cols,whichcol)
get.1<-function(data,split,cols,whichcol) {
  matrix(unlist(strsplit(data,split,fixed=TRUE)),ncol=cols,byrow=TRUE)[,whichcol]
}

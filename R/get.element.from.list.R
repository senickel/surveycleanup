#' get.element.from.list
#'
#' check nominal if they are nominal
#' @param Click here and there
#' @keywords nominal
#' @keywords
#' @export
#' @examples 
#' checking.nominal()
#' @importFrom magrittr %>%
#' 

get.element.from.list<-function(list,el) {
  var<-lapply(list,function(x) return(x[el])) %>% unlist
  return(var)
}
#' test.fun
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


test.fun<-function(ip) {
  ni<-names(ip)
  return(ni[grepl("del",ni)])
}
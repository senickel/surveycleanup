#' Display.old.Data
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


Display.old.Data<-function(idx) {
  itemm1<-responses[responses$Variable%in%make.var.index()[idx],2:3]
  return(itemm1)
}
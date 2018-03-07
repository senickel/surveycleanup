#' give.variablelabel1
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
give.variablelabel1<-function(id) {
  des1<-responses[responses$Variable%in%make.var.index()[id],7]
  des1<-des1 %>% unique
  des1<-des1[!is.na(des1)]
  if (length(des1)==0) des1<-""
  return(des1)
}
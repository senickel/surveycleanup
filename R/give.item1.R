#' give.item1
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
give.item1 <- function(idx) {
  itemm1<-responses[responses$Variable%in%make.var.index()[idx],5:6]
  item1X<<-responses[responses$Variable%in%make.var.index()[idx],c(2:3,5:6)]
  return(itemm1)
}

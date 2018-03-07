#' remove.ui
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
remove.ui<-function() {
  if (!is.null(inserted)) {
    lapply(inserted,function(x1){
      removeUI(
        selector=paste0("#",x1)
      )
    })
    inserted<<-c()
  }
}
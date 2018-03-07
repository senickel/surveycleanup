#' remove.ui2
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
remove.ui2<-function() {
  lapply(1:5,function(x1){
    removeUI(
      selector=paste0("#",paste0("create",x1)))
  })
}

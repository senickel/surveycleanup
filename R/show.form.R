#' show.form
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
show.form<-function(onlyshow=NULL) {
  if (is.null(onlyshow)) {
    if (!exists("inserted")) {
      inserted<<-c() 
    } else if (is.null(inserted)) {
      insert.ui(item1)  
    } else {
      remove.ui()
    }
  }
  if (!is.null(onlyshow)) {
    if (is.null(inserted)) {
      insert.ui(item1)
    }
  }
}


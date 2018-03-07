#' read.csvX
#'
#' check nominal if they are nominal
#' @param Click here and there
#' @keywords nominal
#' @keywords
#' @export
#' @examples 
#' checking.nominal()
#' @importFrom magrittr %>%
read.csvX <- function(filepath) {
  if (detect.if.comma.or.semicolon(filepath)) return(read.csv2(filepath))
  return(read.csv(filepath))
}

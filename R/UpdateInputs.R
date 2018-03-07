#' UpdateInputs
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
UpdateInputs <- function(idx, session) {
  #updateTextInput(session,"id",value=idx)
  updateTextInput(session,"total",value=paste(idx,"/",length(make.var.index()),sep=""))
  updateTextInput(session,"questionid",value=make.var.index()[idx])
  updateTextAreaInput(session,"description",value=give.description1(idx))
  updateTextAreaInput(session,"code",value=give.code1(idx))
  
  updateTextInput(session,"variablelabel",value=give.variablelabel1(idx))
  updateTextInput(session,"newquestionid",value=give.variablename1(idx))
  updateNumericInput(session,"goto",value=idx)
  
  #updateTextInput(session,"question",value=...,2])
}

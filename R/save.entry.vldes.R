#' save.entry.vldes
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
save.entry.vldes<-function(id,input) {
  responses[responses$Variable%in%make.var.index()[id],4]<<-input$newquestionid
  responses[responses$Variable%in%make.var.index()[id],7]<<-input$variablelabel
  responses[responses$Variable%in%make.var.index()[id],8]<<-input$description %>% gsub("\n","$PAR$",.)
  save.csv.version(responses)
}

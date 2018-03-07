#' insert.ui
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

insert.ui<-function(item1) {
  lapply(1:nrow(item1),function(x1) {
    #ich<-ifelse(is.na(item1[x1,2]),1,nchar(item1[x1,2]))
    insertUI (
      selector="#placeholder1",
      ui=tags$div(fluidRow(column(1,checkboxInput(inputId=paste("del",x1,sep=""),label="")),
                           column(2,textInput(inputId=paste("val",x1,sep=""),
                                              label="",
                                              value=item1[x1,1])),
                           column(5,textInput(inputId=paste("lab",x1,sep=""),
                                              label="",
                                              value=item1[x1,2]#,
                                             # width=paste0(ifelse(ich<3&!is.numeric(ich),60,
                                             #                     ich*20),"px")
                                             ))),
                  id=paste0("ins",x1)))
    inserted<<-c(paste0("ins",x1),inserted)
  })
  
}

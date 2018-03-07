#' insert.ui2
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

insert.ui2<-function() {
  lapply(1:5,function(x1) {
    insertUI (
      selector="#placeholder2",
      ui=tags$div(fluidRow(column(1,checkboxInput(inputId=paste("cdel",x1,sep=""),label="")),
                           column(2,textInput(inputId=paste("cval",x1,sep=""),
                                              label="",
                                              value="")),
                           column(5,textInput(inputId=paste("clab",x1,sep=""),
                                              label="",
                                              value=""#,
                                             # width=paste0(ifelse(ich<3&!is.numeric(ich),60,
                                             #                     ich*20),"px")
                                             ))),
                  id=paste0("create",x1)))
    #inserted<<-c(paste0("ins",x1),inserted)
  })
  
}

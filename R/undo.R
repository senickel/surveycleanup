#' undo
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
undo <- function() {
  csvfiles<-list.files("./shinysurvey/data",pattern=".csv")
  if (csvfiles %>% length > 1) {
    csvfiles<-csvfiles[grepl("_date_",csvfiles)]
    load.df<-data.frame(csvfiles,get.1(csvfiles,"_date_",2,2))
    load.df <- load.df[order(load.df[,2],decreasing = TRUE),]
    file.rename(from=paste("shinysurvey/data/",load.df[1,1],sep=""),
                to=paste("shinysurvey/redo/",load.df[1,1],sep=""))
  }
  responses<<-load.csv.version()
}
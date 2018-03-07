#' save.csv.version
#' 
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
#'  
save.csv.version <- function(dat) {
  dateformat<-paste(format(Sys.time(),"%y_%m_%d"),
                    paste((format(Sys.time(), "%X") %>% strsplit(.,":") %>% unlist),collapse="_"),sep="_")
  write.csv2(dat,paste("./shinysurvey/data/data_date_",
                       dateformat,".csv",sep=""),row.names=FALSE)
}
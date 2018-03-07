#' load.csv.version
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
load.csv.version <- function() {
  csvfiles<-list.files("./shinysurvey/data",pattern=".csv")
  if (csvfiles %>% length == 1) {
    dat<-read.csvX(paste("shinysurvey/data/",csvfiles,sep=""))
  } else {
    csvfiles<-csvfiles[grepl("_date_",csvfiles)]
    load.df<-data.frame(csvfiles,get.1(csvfiles,"_date_",2,2))
    load.df <- load.df[order(load.df[,2],decreasing = TRUE),]
    dat<-read.csvX(paste("shinysurvey/data/",load.df[1,1],sep=""))
  }
  varnames<-colnames(dat)
  if(varnames[length(varnames)] == "X") dat<-dat[,1:(ncol(dat)-1)]
  
  #  dat$uniqueIDforthis<-1:nrow(dat)
  return(dat)
}

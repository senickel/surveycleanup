#' give.code1
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
give.code1<-function(id) {
  csvfiles<-list.files("./shinysurvey/data",pattern=".csv")
  if (csvfiles %>% length == 1) {
    dat<-readLines((paste("shinysurvey/data/",csvfiles,sep="")),warn=FALSE)
  } else {
    csvfiles<-csvfiles[grepl("_date_",csvfiles)]
    load.df<-data.frame(csvfiles,get.1(csvfiles,"_date_",2,2))
    load.df <- load.df[order(load.df[,2],decreasing = TRUE),]
    dat<-readLines(paste("shinysurvey/data/",load.df[1,1],sep=""),warn=FALSE)
  }
  cc<-dat[which(responses$Variable%in%make.var.index()[id])+1]
  sst<-gregexpr("\";\"",cc) %>% lapply(.,as.character) %>% lapply(.,function(x1) x1[c(4,6)]) %>% do.call(rbind,.)
  cc1<-substr(cc,sst[,1] %>% as.no %>% +2,sst[,2] %>% as.no) #%>% strsplit(.,"\";\"") %>% do.call(rbind,.)
  return(paste(cc1,collapse="X^x^X"))
}


#' save.code1
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
save.code1_old<-function(c1,id) {
  c1<-strsplit(cc1,"X^x^X") %>% unlist
  csvfiles<-list.files("./shinysurvey/data",pattern=".csv")
  if (csvfiles %>% length == 1) {
    dat<-readLines((paste("shinysurvey/data/",csvfiles,sep="")),warn=FALSE)
  } else {
    csvfiles<-csvfiles[grepl("_date_",csvfiles)]
    load.df<-data.frame(csvfiles,get.1(csvfiles,"_date_",2,2))
    load.df <- load.df[order(load.df[,2],decreasing = TRUE),]
    dat<-readLines(paste("shinysurvey/data/",load.df[1,1],sep=""),warn=FALSE)
  }
  witch<-which(responses$Variable%in%make.var.index()[id])+1
  d1<-dat[(witch[1]):max(witch)]
  sst<-gregexpr("\";\"",d1) %>% lapply(.,as.character) %>% do.call(rbind,.) 
  cc2<-sapply(1:nrow(sst),function(st) {
    p1<-substr(d1[st],1,sst[st,4] %>% as.no %>% +2)
    p3<-substr(d1[st],sst[st,6] %>% as.no,nchar(d1[st]))
    return(paste0(p1,c1[st],p3))
  })
  if (witch[1]>1) before<-dat[1:(witch[1]-1)]
  if (witch[length(witch)]<length(dat)) after<-dat[(witch[length(witch)]+1):length(dat)]
  if (witch[1]>1&witch[length(witch)]<length(dat)) {
    wrt<-c(before,cc2,after)
  } else if (!witch[1]>1&witch[length(witch)]<length(dat)) {
    wrt<-c(cc2,after)
  } else {
    wrt<-c(before,cc2)
  }
  dateformat<-paste(format(Sys.time(),"%y_%m_%d"),
                    paste((format(Sys.time(), "%X") %>% strsplit(.,":") %>% unlist),collapse="_"),sep="_")
  fileConn<-file(paste("./shinysurvey/data/data_date_",
                       dateformat,".csv",sep=""),encoding="UTF-8")
  writeLines(wrt,sep="\n",fileConn)
  close(fileConn)
}
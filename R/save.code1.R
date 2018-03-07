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
#' 
# code<-"1\";\"Very low trust11111X^x^X2\";\"Low trustX^x^X3\";\"Average trustX^x^X4\";\"High trustX^x^X5\";\"Very high trustX^x^X96\";\"Don't knowX^x^X97\";\"Refuse to answerX^x^X99\";\"Not applicable"
# id<-idd
save.code1<-function(id,code) {
  csvfiles<-list.files("./shinysurvey/data",pattern=".csv")
  if (csvfiles %>% length == 1) {
    dat<-readLines((paste("shinysurvey/data/",csvfiles,sep="")),warn=FALSE)
  } else {
    csvfiles<-csvfiles[grepl("_date_",csvfiles)]
    load.df<-data.frame(csvfiles,get.1(csvfiles,"_date_",2,2))
    load.df <- load.df[order(load.df[,2],decreasing = TRUE),]
    dat<-readLines(paste("shinysurvey/data/",load.df[1,1],sep=""),warn=FALSE)
  }
  
  cc<-dat[which(responses$Variable%in%make.var.index()[id])+1] # +1 because first line is var names
  old<-cc %>% strsplit(.,";") %>% get.element.from.list(.,1:4) %>% matrix(.,ncol=4,byrow=TRUE) %>% 
    apply(.,1,function(x) paste0(x,collapse=";"))
  new<-paste0(paste(old,code %>% strsplit(.,"X^x^X",fixed=TRUE) %>% unlist,sep=";"),";",
              cc %>% strsplit(.,";") %>% get.element.from.list(.,7),";",
              cc %>% strsplit(.,";") %>% get.element.from.list(.,8))
  dat[which(responses$Variable%in%make.var.index()[id])+1]<-new
  dat2<-dat %>% strsplit(.,";") %>% do.call(rbind,.) %>% apply(.,2,function(x)   gsub("\"","",x)) %>% as.data.frame
  colnames(dat2)<-colnames(responses)  
  responses<<-dat2[-1,]  
  save.csv.version(responses)
}


#' detect.if.comma.or.semicolon
#'
#' check nominal if they are nominal
#' @param Click here and there
#' @keywords nominal
#' @keywords
#' @export
#' @examples 
#' checking.nominal()
#' @importFrom magrittr %>%

detect.if.comma.or.semicolon<-function(filepath) {
  fl<-readLines(filepath)[1:10]
  sem<-lapply(fl,function(x1)  ifelse(gregexpr(";",x1,fixed=TRUE)[[1]][1]==-1,-1,
                                      length(gregexpr(";",x1,fixed=TRUE)[[1]]))) %>% do.call(c,.)
  
  
  return(lapply(sem,function(x1) sem%in%x1 %>% sum)  %>% do.call(c,.) %>% sum==100&
           as.numeric(sem%in%(-1))%>%sum==0)
}
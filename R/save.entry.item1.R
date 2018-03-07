#' save.entry.item1
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
save.entry.item1<-function(item1,input) {

  if (is.na(item1[1,1])) item1[1,1]<-""
  if (item1[1,1]!=""|item1[1,2]!="") {

  item.all<-data.frame(oldval=item1X[,1],oldlab=item1X[,2],newval=item1[,1],newlab=item1[,2],del="")



  # fill up changed ones
    item.all[1:nrow(item1),3]<-sapply(1:nrow(item1),function(x1) {
      input[[paste0("val",x1)]]
    })
    item.all[1:nrow(item1),4]<-sapply(1:nrow(item1),function(x1) {
      input[[paste0("lab",x1)]]
    })
    item.all[1:nrow(item1),5]<-ifelse(sapply(1:nrow(item1),function(x1) {
      input[[paste0("del",x1)]]
    }),0,1)

    createtrue<-sapply(paste0("cval",c(1:5)),function(x) {
      if (input[[x]]!="") return(1)
      return(0)
    }) %>% sum

    if (createtrue>0) {
      item.new<-data.frame(oldval=rep("",createtrue),oldlab=rep("",createtrue),
                           newval=sapply(1:createtrue,function(x) input[[paste0("cval",x)]]),
                           newlab=sapply(1:createtrue,function(x) input[[paste0("clab",x)]]),
                           del=sapply(1:createtrue,function(x) input[[paste0("cdel",x)]]))
      item.all<-rbind(item.all,item.new)
    }



    edit.data.item1(item.all,idd)
    save.csv.version(responses)
  }
}

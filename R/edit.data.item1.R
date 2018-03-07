#' edit.data.item1
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
edit.data.item1<-function(item.df,idx) {
  apply(item.df,1,function(x1) {
    # Deleting entries
    if (x1[5]==0) { 
      # Deleting old entries
      if (any(responses$Variable%in%make.var.index()[idx]&
              responses$Value%in%x1[1])) {
        responses<<-responses[-which(responses$Variable%in%make.var.index()[idx]&
                                       responses$Value%in%x1[1]),]
        # Deleting new entries
      } else if (any(responses$Variable%in%make.var.index()[idx]&
                     (!responses$Value%in%x1[1]&responses$New.Value%in%x1[1]))) {
        responses<<-responses[-which(responses$Variable%in%make.var.index()[idx]&
                                       responses$New.Value%in%x1[1]),]
      }
      # Editing original entries
    #  idx<-idd
    #x1<-c("no","","2","no")
      } else if (any(responses$Variable%in%make.var.index()[idx]&
                   responses$Value%in%x1[1])) {
      
      
        responses[responses$Variable%in%make.var.index()[idx]&
                  responses$Value%in%x1[1],5]<<-x1[3]
      responses[responses$Variable%in%make.var.index()[idx]&
                  responses$Value%in%x1[1],6]<<-x1[4]
      
      # Editing new entries
    # } else if (any(responses$Variable%in%make.var.index()[idx]&
    #               (responses$Value==""&x1[1]==""&responses$New.Value%in%x1[3]))) {
    #   
    #   responses[responses$Variable%in%make.var.index()[idx]&
    #               (responses$Value==""&x1[1]==""&responses$New.Value%in%x1[3]),5]<<-x1[3]
    #   responses[responses$Variable%in%make.var.index()[idx]&
    #               (responses$Value==""&x1[1]==""&responses$New.Value%in%x1[3]),6]<<-x1[4]
    #   
    } else {
      maxrow<-which(responses$Variable%in%make.var.index()[idx]) %>% max
      first<-responses[1:maxrow,]
      second<-responses[(maxrow+1):nrow(responses),]
      middle<-first[maxrow,]
      middle[2]<-x1[3]
      middle[3]<-x1[4]
      middle[5]<-x1[3]
      middle[6]<-x1[4]
      responses<<-rbind(first,middle,second)
    } 
  })
}


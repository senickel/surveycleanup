#' checking.nominal
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
#' @import shiny
editsurvey2<-function() {

  options(stringsAsFactors = FALSE)
  if (.Platform$OS.type == "windows") {
    setwd("C:/")
  } else if (Sys.info()["sysname"] == "Darwin") {
    setwd("~/Documents")
  } else if (.Platform$OS.type == "unix") {
    #stop("Linux detected -- code stopped -- add setwd command in code")
  } else {
    stop("Unknown OS")
  }

  dir.create.check<-function(dirname){
    if (!dir.exists(dirname)) dir.create(dirname)
  }
  # make datatree
  dir.create.check("shinysurvey")
  dir.create.check("shinysurvey/data")
  dir.create.check("shinysurvey/redo")
  dir.create.check("shinysurvey/packages")



  #### if no file is there put it in
  #tundat<-read.csv2("C:/Users/xnicse/Google Drive/package/editsurvey/inst/extdata/tun2014_check.csv")
  #save.csv.version(tundat)
  if (list.files("./shinysurvey/data/") %>% length == 0) {

    save.csv.version( read.csv2(system.file("extdata", "tun2014_check.csv", package = "editsurvey")))

  }

  responses<<-load.csv.version()
  idd<<-1
  item1<<-give.item1(idd)
  description1<<-give.description1(idd)
  variablelabel1<<-give.variablelabel1(idd)
  variablename1<<-give.variablename1(idd)
  code1<<-give.code1(idd)
  inserted<<-c()
  #
  # rm(inserted)
  # rm(inserted2)
  #idd<-101
  ui <- shiny::fluidPage(
    sidebarLayout(
      mainPanel(
        shinyjs::useShinyjs(),


        #helpText(paste("Number",textOutput(idd),"of",nrow(responses))),
        shiny::tags$br(),
        shiny::tags$br(),
        fluidRow(column(2,shinyjs::disabled(textInput(inputId="total",
                                                      label="Number/Total Entries",
                                                      value=paste(idd,"/",length(make.var.index()),sep=""),
                                                      width="200px"))),
                 column(2,shinyjs::disabled(textInput(inputId="questionid",label="Old Question id",
                                                      value=make.var.index()[idd],width="200px"))),
                 column(2,numericInput(inputId="goto",
                                       label="Go to id",
                                       value=idd,
                                       width="200px"))),
        actionButton("goback", "Go Back (does not save)",onclick ="location.href='#top';"),
        actionButton(inputId="nextentry",label="Next Entry (saves)",onclick ="location.href='#top';"),
        actionButton("undo", "Undo",onclick ="location.href='#top';"),
        actionButton("showform", "Show/Hide form",onclick ="location.href='#top';"),
        shiny::tags$br(),
        shiny::tags$br(),
        tags$div(id = 'placeholder1'),
        #actionButton("create", "Create",onclick ="location.href='#top';"),
        tags$div(id = 'placeholder2'),

        fluidRow(column(4,textInput(inputId="variablelabel",label="Variable Label",
                                    value=variablelabel1,
                                    width="400px")),
                 column(2,textInput(inputId="newquestionid",label="New Question id",
                                    value=variablename1,
                                    width="200px"))),
        textAreaInput(inputId="description",label="Description",
                      value=description1,
                      width="400px",height="200px",resize="both"),
        textAreaInput(inputId="code",label="CSV code",
                      value=code1,
                      width="400px",height="200px",resize="both"),
        h4("Only use this function if length and order of variables are the same."),
        actionButton("codenext", "Update Code (save&next)",onclick ="location.href='#top';")

      ),
      sidebarPanel(DT::dataTableOutput("item1", width = 350),
                   h3("Variable Label:"),
                   textOutput("variablelabel1"),
                   h3("Description:"),
                   textOutput("description1"),
                   textOutput("output1"),
                   textOutput("output2")
                   #,verbatimTextOutput("value")
      )))

  server <- function(input, output, session) {


    shiny::observeEvent(input$showform, {
      show.form()
    })

    shiny::observeEvent(input$nextentry, {
      show.form(1)
      save.entry.item1(item1,input)

      save.entry.vldes(idd,input)

      idd2<- isolate({input$goto})

      if(idd!=idd2) {
        idd<<-ifelse(idd2<=length(make.var.index()),idd2,length(make.var.index()))
      } else {
        idd<<-ifelse(idd<length(make.var.index()),idd+1,idd)
      }

      item1<<-give.item1(idd)
      variablelabel1<<-give.variablelabel1(idd)
      description1<<-give.description1(idd)
      variablename1<<-give.variablename1(idd)
      UpdateInputs(idd,session)
      remove.ui()
      remove.ui2()
      insert.ui(item1)
      insert.ui2()
    }, priority = 1)

    shiny::observeEvent(input$codenext, {
      output$output1<-renderText({ input[["code"]] })
      save.code1(idd,isolate({input[["code"]]}))
      idd<<-ifelse(idd<length(make.var.index()),idd+1,idd)
      item1<<-give.item1(idd)
      variablelabel1<<-give.variablelabel1(idd)
      description1<<-give.description1(idd)
      variablename1<<-give.variablename1(idd)
      UpdateInputs(idd,session)
      remove.ui()
      remove.ui2()
      insert.ui(item1)
      insert.ui2()
    }, priority = 1)

    shiny::observeEvent(input$goback, {
      show.form(1)
      idd<<-ifelse(idd>1,idd-1,idd)
      item1<<-give.item1(idd)
      variablelabel1<<-give.variablelabel1(idd)
      description1<<-give.description1(idd)
      variablename1<<-give.variablename1(idd)
      UpdateInputs(idd,session)
      remove.ui()
      remove.ui2()
      insert.ui(item1)
      insert.ui2()
    }, priority = 1)

    shiny::observeEvent(input$undo, {
      show.form(1)
      undo()
      item1<<-give.item1(idd)
      variablelabel1<<-give.variablelabel1(idd)
      description1<<-give.description1(idd)
      variablename1<<-give.variablename1(idd)

      UpdateInputs(idd,session)
      remove.ui()
      insert.ui(item1)
    })


    output$item1 <- DT::renderDataTable({
      input$nextentry
      input$showform
      input$goback
      input$undo
      colnames(item1)<-c("Values","Labels")
      Display.old.Data(idd)
    }, server = FALSE, selection = "single",options = list(dom = "t"),rownames=FALSE
    )
    output$description1<-renderText({ description1 })
    output$variablelabel1<-renderText({ variablelabel1 })

  }

  shiny::shinyApp(ui=ui,server=server)
}
## start shiny



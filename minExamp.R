library(shiny)


doThingModalUI <- function(id) {
  ns <- NS(id)
  actionButton(ns("openModalBtn"), "Send result to modal")
}

doThingModal <- function(input, output, session, chosenText = NULL) {
  
  # Observe open modal button -- opens modal
  observeEvent(input$openModalBtn, {
    ns <- session$ns
    showModal(ui = shiny::modalDialog(size = "l", {
      actionButton(inputId = ns("doThing"), label = "Analyze")
    }, title = "Do Thing!"), session = session)
  }, ignoreInit = TRUE, autoDestroy = TRUE)
  
  # Observe doThing button -- does the thing 
  observeEvent(input$doThing, {
    ns <- session$ns
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = paste0("Doing thing! -- your input was ", chosenText()), value = .3)
    Sys.sleep(3)
  }, ignoreInit = TRUE, once = TRUE)
}

ui <- fluidPage(
  h1("Minimal example of modal button double-activation"),
  hr(),
  selectInput(inputId = "selectInput", 
              label = "Select option", 
              choices = c("A", "B", "C"), selected = "A"),
  actionButton(inputId = "doMain", label = "Go"),
  textOutput(outputId = "textOut"),
  br(),
  doThingModalUI("modalExamp"),
  hr()
)

server <- function(input, output, session) {
  
  textOut <- eventReactive(input$doMain, {
    input$selectInput
  })
  output$textOut <- renderText({
    textOut()
  })
  
  observeEvent(textOut(), {
    callModule(doThingModal, "modalExamp", chosenText = textOut)
  })
}

shinyApp(ui = ui, server = server)


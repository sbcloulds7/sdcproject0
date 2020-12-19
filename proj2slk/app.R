library(shinydashboard)
library(datasets)

ui <- dashboardPage(
  dashboardHeader(title="Brushing"),
  dashboardSidebar(collapsed = TRUE),
  dashboardBody(
    
    fluidRow(
      
      plotOutput("airquality", brush = "brushing"),
      
      box((verbatimTextOutput("coords")),width = 8))
    
    
  )#body
)#page

server <- function(input,output){
  
  output$airquality <- renderPlot({
    plot(x=airquality$Ozone, y=airquality$Wind,
         xlab="Ozone",ylab="Wind",
         pch=as.character(airquality$Ozone))})
  
  output$coords <- renderPrint({
    
    brushedPoints(airquality, input$brushing, xvar = "Ozone", 
               yvar = "Wind")
    
    })
  
}

shinyApp(ui= ui, server = server)
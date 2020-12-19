#References
#Schmuller, J. (2018). R projects for dummies. Wiley, Kindle Edition.
#install shiny package and check box
#install shinydashboard package and check box

library(shinydashboard)

ui <-dashboardPage(
    dashboardHeader(
        
        title = "Uniform Distribution"
        
    ),
    dashboardSidebar(),
    
    dashboardBody(
        
        fluidRow(
            
           column(width = 6,
            
            box(
                title = "Select a Number",
                solidHeader = TRUE,
                background ="yellow",
                status ="warning",
                width = NULL,
                height = 312,
                sliderInput(inputId = "number",
                            label = "",
                            value = 500, min = 25, max = 1000)),
            
            box(title = "Histogram",
                
                solidHeader = TRUE,
                
                background ="light-blue",
                
                status="primary",
                
                width = NULL,
                
                plotOutput("hist", height = 250))
        ),
           column(width = 6,
        
            valueBoxOutput("meanBox", width = NULL),
            valueBoxOutput("medianBox", width = NULL),
            valueBoxOutput("sdBox", width = NULL)
        ))))

server <- function(input, output) {
    
    histdata <- reactive({runif(input$number,min=0,max=1)})
    
    output$hist <- renderPlot({
        
        hist(histdata(),xlab="Value",
             
             main=paste(input$number,"random values between 0 and 1 "))
    })
    
    output$meanBox <- renderValueBox({
        valueBox(
            round(mean(histdata()),3),"Mean",
                  color= "navy"
                  )
        
    })
    
    output$medianBox <- renderValueBox({
        valueBox(
            round(median(histdata()),3),"Median",
            color = "aqua"
        )
    })
    
    output$sdBox <- renderValueBox({
                
        valueBox(
            round(sd(histdata()),3), "Standard Deviation",
            color = "blue"
        )
    })
    
    }

shinyApp(ui, server)
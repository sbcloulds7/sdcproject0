#Reference: R Projects for Dummies by Joseph Schmuller
library(tidyr)
library(tibble)
library(readxl)
library(tidyverse)
library(dplyr)
library(shiny)
library(devtools)
library(rsconnect)


exceldata <- read_excel("myfoods.xlsx")
dfasmr = data.frame(exceldata)

dfasmr1 <- subset(dfasmr, select = c(NCDayCountSinceFirst, apples, bananas, carrots, dates, eggplants, fish, gummies))

dfasmrnona1 <- drop_na(dfasmr1)

options <- c("Days Since First Food Count" = "NCDayCountSinceFirst",
             "Apples (organic, fresh)" = "apples",
             "Bananas (organic, dried)" = "bananas",
             "Carrots (baby, organic)" = "carrots",
             "Dates (imported)" = "dates",
             "Eggplant (organic" = "eggplants",
             "Fish (wild caught)" = "fish",
             "Fruit gummies (real fruit flavors)" = "gummies"

)



df.options <-data.frame(options)
df.lv <-rownames_to_column(df.options)

colnames(df.lv) <- c("label", "value")

ui <- fluidPage(
    selectInput("X", "X Variable:", options),
    
    selectInput("Y", "Y Variable:", options),
    
    plotOutput("scatter")
    
)

server <- function(input, output) {
    selections <- reactive({
        
        dfasmr[,c(input$X, input$Y)]
        
    })
    
    output$scatter <- renderPlot({
        
        x_column <- selections()[,1]
        y_column <- selections()[,2 ]
        
        correlation <-cor(x_column,y_column)
        regression <- lm(y_column ~ x_column)
        intercept <- regression$coefficients[1]
        slope <- regression$coefficients[2]
        
        X_Label <- df.lv$label[which(df.lv$value == input$X)]
        Y_Label <- df.lv$label[which(df.lv$value == input$Y)]
        
        plot(x=x_column,y=y_column,xlab= X_Label, ylab = Y_Label,
             cex.axis = 1.5, cex.lab = 1.5, pch = 20, cex = 2,
             main = paste(Y_Label, "vs" ,X_Label,
                          "\n r -", round(correlation,3),
                          "Y' =", round(intercept,3), "+", round(slope,3), "X"),
             cex.main=1.8)
        abline(intercept,slope)
        
    })
}

shinyApp(ui = ui, server = server)
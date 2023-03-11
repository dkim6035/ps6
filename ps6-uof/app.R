#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

data <- read_delim("income-height.csv")

ui <- fluidPage(
  
    titlePanel("Marriage, Income, and Height"),
    tabsetPanel(
      tabPanel("Introduction",
          mainPanel(
            h3("There are", nrow(data), "documented individuals in this dataset"),
            HTML("<br>"),
            h4("This dataset has 8 distinct variables for each individual:"),
            h4("1. Income"),
            h4("2. Height"),
            h4("3. Weight"),
            h4("4. Age"),
            h4("5. Marital"),
            h4("6. Sex"),
            h4("7. Education"),
            h4("8. Afqt"),
            HTML("<br>"),
            h4("This dataset was provided through the data portal on Canvas, the source of which is unknown."),
            h4("However, by sifting through and organizing the data we are able to bring about unique and intriguing graphs.")
            
          )
        ),
      tabPanel("Plot",
               sidebarLayout(
                 sidebarPanel(fluidRow(
                   column(6,
                   sliderInput(
                     "cases", "How many individuals do you want?",
                     min = 1, max = 7006, value = 300
                   )),
                   column(6,
                   selectInput("color", "Choose the color of the plot:",
                                choices = c("green", "blue", "purple")))
                   
                 ),
                 ),
                 mainPanel(plotOutput("case"),
                           textOutput("text"))
               )
            ),
      tabPanel("Table",
               sidebarLayout(
                 sidebarPanel(selectInput('marital', 'Select marital status', choices = c(unique(data$marital))),
                              selectInput('sex', 'Select sex', choices = c(unique(data$sex))),
                              selectInput('education', 'Select education level', choices = c(unique(data$education))),
                              textOutput("text1")
                 ),
                 mainPanel(dataTableOutput("table"),
      )
                 
               )
      )
            )
  )



server <- function(input, output) {
  
  set <- reactive({
    data %>% 
      sample_n(input$cases)
  })
  
  output$case <- renderPlot({
    
    set() %>% 
      ggplot(aes(weight, income)) +
      geom_point(col = input$color) +
      labs(title = "Weight vs Income",
           x = "Weight",
           y = "Income")
  })
  
    output$text <- renderText({
      
      set() %>% 
        pull(income) %>% 
        mean() %>% 
        paste("The mean income within this number of individuals is", .)
    })
    
    datas <- reactive({
      datasorted <- data
      
      if (!is.null(input$marital) && input$marital != "") {
        datasorted <- datasorted %>%
          filter(marital == input$marital)
      }
      
      if (!is.null(input$sex) && input$sex != "") {
        datasorted <- datasorted %>% 
          filter(sex == input$sex)
      }
      
      if (!is.null(input$education) && input$education != "") {
        datasorted <- datasorted %>%
          filter(education == input$education)
      }
      
      datasorted
    })
    
    output$table <- renderDataTable({
      datas()
    })
      
    output$text1 <- renderText({
      
      datas() %>% 
        nrow() %>% 
        paste("There are", ., "individuals with this combination")
      
    })
    
  }


shinyApp(ui = ui, server = server)

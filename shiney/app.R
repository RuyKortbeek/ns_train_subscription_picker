library(shiny)
library(tidyverse)

# Load the data

bcl = read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui = fluidPage(titlePanel("BC Liquor Store prices"),
               sidebarLayout( #Here comes all the things related to the left sidebar (input)
                 sidebarPanel(h1("INPUT"),
                              
                 
                 sliderInput("priceInput", "Price", min = 0, max = 100, value = c(25, 40),
                             pre = "$"),
                 uiOutput("countryOutput"),
                 uiOutput("typeOutput"),
                 uiOutput("subtypeOutput")
                 
                 
                 
                 ),
                 
                 
                 
                 mainPanel(# Her comes all the stuff related to the results
                   plotOutput("coolplot"),
                   h4("total number of drinks counted = "),
                   h3(textOutput("counts")),
                   br(),
                  tableOutput("results")
                  
                  )
               )
)
      

server <- function(input, output) {
#filter the data set in a reactive function  
  filtered = reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }  
    
    bcl %>%
    filter(Price >= input$priceInput[1] &
             Price <= input$priceInput[2] &
             Type == input$typeInput &
             Country == input$countryInput
    )
  })
  
  filtered.final = reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }  
    
    bcl %>%
      filter(Price >= input$priceInput[1] &
               Price <= input$priceInput[2] &
               Type == input$typeInput &
               Subtype %in% input$subtypeInput &
               Country == input$countryInput
      )
  })
  
# plot the data
  output$coolplot <- renderPlot({
    if (is.null(input$countryInput)) {
      return(NULL)
    }  
    
    ggplot(filtered.final(), aes(Alcohol_Content)) +
      geom_histogram()
  })
# Make a table
  output$results = renderTable({filtered()
  })
# Make a total counts 
  output$counts = renderText({filtered()
                             nrow(filtered)
  })
  #########################
  #### Dynamic elements ###
  #########################
  
  # Select the country based on what is in the datafile
  output$countryOutput = renderUI({
    selectInput("countryInput", "Coutry", 
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })
  
  # select type of what is in the datafile
  output$typeOutput = renderUI({
    radioButtons("typeInput", "Type",
                choices = sort((unique(bcl$Type))),
                selected = "BEER")
  })
  
  # Select all the subtypes you want to show
  output$subtypeOutput = renderUI({
    checkboxGroupInput("subtypeInput", "Subtype",
                      choices = sort(unique(filtered()$Subtype)),
                      selected = sort(unique(filtered()$Subtype))[1]
                      )
  })
}


shinyApp(ui = ui, server = server)
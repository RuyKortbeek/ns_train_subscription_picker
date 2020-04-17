library(shiny)
library(tidyverse)

# Data gebaseerd op https://www.ns.nl/ns-abonnementen/overzicht-abonnementen/ 


ui = fluidPage(titlePanel("Dé NS abonnement kiezer"),
               sidebarLayout( #Here comes all the things related to the left sidebar (input)
                 sidebarPanel(
                              h4("Hoeveel dagen per maand reis je?"),
                              selectInput("traveldaysInput", "Aantal dagen:", choices = c(1:31)),
                              br(),
                              h4("Hoeveel ritten daarvan reis je buiten de spitsuren"),
                              h6("Spitstijden: ma t/m vr 06:30-09:00 & 16:00-18:30)"),
                              uiOutput("offpeakOutput"),
                              br(),
                              textInput("faircostInput", "Enkele ritprijs (zonder korting)",
                                        value = "10"),
                              br(),
                              textInput("trajectfixedInput", "Prijs traject abonnement (via ns.nl)",
                                        value = "")
                              
                              
                 ),
                 
                 
                 
                 mainPanel(
                   plotOutput("mainplot"),
                   tableOutput("maintable")
                    
               )
               )
)



server = function(input, output) {
  # Number of fairs that are travelled during off peak times 
  # Depends on the number of days the user used as an input
  output$offpeakOutput = renderUI({
    sliderInput("offpeakInput", "Ritten buiten de Spitsuren",
                min = 0, max = as.numeric(input$traveldaysInput)*2, 
                value = 1, step = 1)
  })
  
  ############################################
  # Input variables depending on user input #
  ############################################
  
  # traveldaysInput
  # travelpeakInput
  # faircostInput
  # trajectfixedInput
  

  observe({
   
    if(!is.null(input$offpeakInput)) {
    
    
    df= data.frame(
     dag =  c(1:(as.numeric(input$traveldaysInput)*2)),
    basis =  c(1:(as.numeric(input$traveldaysInput)*2)*as.numeric(input$faircostInput)),
    dal_voordeel = c(
     
     c(1:((as.numeric(input$traveldaysInput)*2)-as.numeric(input$offpeakInput))*as.numeric(input$faircostInput)), # costs during peak
     
     
      c(1:as.numeric(input$offpeakInput)*(as.numeric(input$faircostInput)*0.6) + ((as.numeric(input$traveldaysInput)*2)-as.numeric(input$offpeakInput))*as.numeric(input$faircostInput))  # cost during off peak + cost made during peak 
    ) +5,
    
    altijd_voordeel = c(
      c(1:((as.numeric(input$traveldaysInput)*2)-as.numeric(input$offpeakInput))*(as.numeric(input$faircostInput)*0.8)), # costs during peak
      
      
      c(1:as.numeric(input$offpeakInput)*(as.numeric(input$faircostInput)*0.6) + ((as.numeric(input$traveldaysInput)*2)-as.numeric(input$offpeakInput))*(as.numeric(input$faircostInput)*0.8)) # cost during off peak + cost made during peak 
    ) +23,
    
    traject_vrij = rep(as.numeric(input$trajectfixedInput), each = (as.numeric(input$traveldaysInput)*2)),
    
    altijd_vrij = rep(351, each = (as.numeric(input$traveldaysInput)*2)),
    
    dal_vrij = c(
      c(1:((as.numeric(input$traveldaysInput)*2)-as.numeric(input$offpeakInput))*as.numeric(input$faircostInput)), # costs during peak
    
    
    c(1:as.numeric(input$offpeakInput)*(as.numeric(input$faircostInput)*0) + ((as.numeric(input$traveldaysInput)*2)-as.numeric(input$offpeakInput))*as.numeric(input$faircostInput)) # cost during off peak + cost made during peak 
    )+105
    )
    
    
  #   colnames(df) = c("dag", "kosten")
    

      print(reactiveVal(input$offpeakInput))
   
     
    
    output$maintable = renderTable(df)
    }
  })
  
  

  #################
  # plot the data #
  #################
  
  output$mainplot <- renderPlot({
    if (is.null(input$faircosts)) {
      return(NULL)
    }
   # ggplot(df)+
    #  geom_line()
    
    })
  
 

}
shinyApp(ui = ui, server = server)
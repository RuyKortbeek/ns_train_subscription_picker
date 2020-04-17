library(shiny)
library(tidyverse)


ui = fluidPage(titlePanel("Dé NS abonnement kiezer"),
               sidebarLayout( #Here comes all the things related to the left sidebar (input)
                 sidebarPanel(
                   h4("Hoeveel dagen per maand reis je?"),
                   selectInput("traveldaysInput", "Aantal dagen:", choices = c(1:31)),
                
                   textInput("faircostsInput", "Enkele ritprijs (zonder korting)",
                             value = "10")
                 
                   
                   
                 ),
                 
                 
                 
    
                   mainPanel(
                   plotOutput("mainplot"),
                   tableOutput("maintable")
                   
                 )
               )
)



server = function(input, output) {
  
  ############################################
  # Input variables depending on user input #
  ############################################
  
  # Number of number of fairs a month derived from the numer of days you travel * 2 (->departure and return)
  # Presuming you travel 2 times a day the same route (e.g. to work (1), and back home (2))
  
  
  observe({
  
 
     df= data.frame(
     c(1:(as.numeric(input$traveldaysInput)*2)),
    c(1:(as.numeric(input$traveldaysInput)*2))*as.numeric(input$faircostsInput)
      )
     
    colnames(df) = c("dag", "kosten")
     
    
    output$maintable = renderTable(df)
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
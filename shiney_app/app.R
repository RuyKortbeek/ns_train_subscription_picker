library(shiny)
library(tidyverse)


ui = fluidPage(titlePanel("Dé NS abonnement kiezer"),
               sidebarLayout( #Here comes all the things related to the left sidebar (input)
                 sidebarPanel(
                              h4("Hoeveel dagen per maand reis je?"),
                              selectInput("traveldaysInput", "Aantal dagen:", choices = c(1:31)),
                              br(),
                              h4("Hoeveel ritten daarvan reis je buiten de spitsuren"),
                              h6("Spitstijden: ma t/m vr 06:30-09:00 & 15:30-19:00)"),
                              uiOutput("offpeakOutput"),
                              br(),
                              textInput("faircosts", "Enkele ritprijs (zonder korting)",
                                        value = ""),
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
  
  ############################################
  # Input variables depending on user input #
  ############################################
  
  # Number of number of fairs a month derived from the numer of days you travel * 2 (->departure and return)
  # Presuming you travel 2 times a day the same route (e.g. to work (1), and back home (2))
  
  fairs.i.travel = reactive({as.numeric(input$traveldaysInput)*2
    })
  
 
  
  # Number of fairs that are travelled during off peak times 
  # Depends on the number of days the user used as an input
    output$offpeakOutput = renderUI({
    sliderInput("offpeakInput", "Ritten buiten de Spitsuren",
                min = 0, max = as.numeric(input$traveldaysInput)*2, 
                value = 0, step = 1)
  })
  # create a numeric variable
  fairs.off.peak = reactive({as.numeric(input$offpeakInput[])
    })
    
  
  # Journey price: Replace a comma by a period in the price
  price.single.journey = reactive({as.numeric(gsub(",", ".", input$faircosts))
    })
  
  # Monthly costs of the "Traject abonnement" of the NS
  
  
  # Monthly cost for each subscription
  traject.fixed = reactive({as.numeric(input$trajectfixedInput)}) # this one the user has to give in the input form
  no.subscrition = 0 # You anly pay the price for the fair
  free.anytime = 351 # Traveller can travel throughout the Netherlands withoud additional costs
  fourty.pct = (53/12) # Traveller gets 40% discount (off-peak hours) - costs of the subscription calculated per month
  twenty.fourty.pct = 23 # Traveller gets 20%discounts during peak hours & 40% discount duing off-peak hours
  
  
  #####################################
  # Create dataframe of monthly costs #
  #####################################
  # input is fairs people made
  
  
  output$maintable = renderTable({
 df = data.frame(cbind(as.vector(1:(as.numeric(input$traveldaysInput[])*2))),
                 as.vector((1:((as.numeric(input$traveldaysInput[])*2)))*as.numeric(input$faircosts[])),
                 as.vector(c((1:((as.numeric(input$traveldaysInput[])*2*(1-(as.numeric(input$offpeakInput[])/(as.numeric(input$traveldaysInput[])*2))))))*(as.numeric(input$faircosts[])),
                             (1:((as.numeric(input$traveldaysInput[])*2*(as.numeric(input$offpeakInput[])/(as.numeric(input$traveldaysInput[])*2)))))*(as.numeric(input$faircosts[])*0.6)+
                               ((as.numeric(input$traveldaysInput[])*2*(1-(as.numeric(input$offpeakInput[])/(as.numeric(input$traveldaysInput[])*2))))*(as.numeric(input$faircosts[]))))
                 )
                
                
                 )
})
  
  

  #################
  # plot the data #
  #################
  
  output$mainplot <- renderPlot({
    if (is.null(input$faircosts)) {
      return(NULL)
    }})
  
 

}
shinyApp(ui = ui, server = server)
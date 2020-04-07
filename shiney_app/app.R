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
                              textInput("journeyInput", "Enkele ritprijs (zonder korting)",
                                        value = ""),
                              br(),
                              textInput("trajectfixedInput", "Prijs traject abonnement (via ns.nl",
                                        value = "")
                              
                              
                 ),
                 
                 
                 
                 mainPanel(# Her comes all the stuff related to the results
                    
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
  price.single.journey = reactive({as.numeric(gsub(",", ".", input$journeyInput))
    })
  
  # Monthly costs of the "Traject abonnement" of the NS
  traject.fixed = reactive({as.numeric(input$trajectfixedInput) 
    })
  
  
  
  
  
}
shinyApp(ui = ui, server = server)
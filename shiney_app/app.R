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
                              textInput("farecostInput", "Enkele ritprijs (zonder korting)",
                                        value = ""),
                              br(),
                              textInput("trajectfixedInput", "Prijs traject abonnement (via ns.nl)",
                                        value = "")
                              
                              
                 ),
                 
                 
                 
                 mainPanel(
                   plotOutput("mainplot"),
                   textOutput("bestoption"),
                   tableOutput("maintable")
                    
               )
               )
)



server = function(input, output) {
  # Number of fares that are travelled during off peak times 
  # Depends on the number of days the user used as an input
  output$offpeakOutput = renderUI({
    sliderInput("offpeakInput", "",
                min = 0, max = (as.numeric(input$traveldaysInput)*2), 
                value = 0, step = 1, round = TRUE)
  })
  


# Observe allows us to fetch the input data
  observe({
    
    fare_value =  as.numeric(paste(gsub(",", ".", input$farecostInput)))
    
    traject_fixed_value = as.numeric(paste(gsub(",", ".", input$trajectfixedInput)))
    
    number_days = as.numeric(input$traveldaysInput)
    
    off_peak_fares = as.numeric(input$offpeakInput)
    
   req(input$farecostInput) # makes sure App does not bug when cost inpout is left empty
    
    if(!is.null(input$offpeakInput) & !is.null(input$traveldaysInput))  # makes sure df is not bugging when switching off-peak / travelsdays (during switch values becomes NULL)
      {
    
####################################################
# Create the main dataframe with the subscriptions #
####################################################

    df= data.frame(
     rit =  c(1:(number_days*2)),
     
    basis =  c(1:(number_days*2)*fare_value),
    
    dal_voordeel = 
    
      if(off_peak_fares == ((number_days*2))){
       5+ c(1:off_peak_fares*(fare_value*0.6)) 
      } 
    
    else if(off_peak_fares == 0){
      dal_voordeel =  5+c(1:(number_days*2)*fare_value)
    } 
     
  else if(off_peak_fares != 0 & off_peak_fares != ((number_days*2))){
     5+ c(
     c(1:((number_days*2)-off_peak_fares)*fare_value),  # costs during peak
     
      c(1:off_peak_fares*(fare_value*0.6) + ((number_days*2)-off_peak_fares)*fare_value) # cost during off peak + cost made during peak  
     )  
    
  }
  ,
  altijd_voordeel = 
    
    if(off_peak_fares == ((number_days*2))){
     23+ c(1:off_peak_fares*(fare_value*0.6)) 
    } 
  
  else if(off_peak_fares == 0){
    dal_voordeel = 23+ c(1:(number_days*2)*fare_value*0.8)
  } 
   
  else if(off_peak_fares != 0 & off_peak_fares != ((number_days*2))){
   23+ c(
      c(1:((number_days*2)-off_peak_fares)*fare_value*0.8),  # costs during peak
      
      c(1:off_peak_fares*(fare_value*0.6) + ((number_days*2)-off_peak_fares)*fare_value*0.8) # cost during off peak + cost made during peak  
      )  
    
  }
,
dal_vrij = 
  
  if(off_peak_fares == ((number_days*2))){
    105+c(1:off_peak_fares*(fare_value*0)) 
  } 

else if(off_peak_fares == 0){
  dal_voordeel =  105+c(1:(number_days*2)*fare_value)
} 

else if(off_peak_fares != 0 & off_peak_fares != ((number_days*2))){
  105+c(
    c(1:((number_days*2)-off_peak_fares)*fare_value),  # costs during peak
    
    c(1:off_peak_fares*(fare_value*0) + ((number_days*2)-off_peak_fares)*fare_value) # cost during off peak + cost made during peak  
    )  
  
}
,
    altijd_vrij = rep(351, each = (number_days*2))
,
trajectvrij = rep(traject_fixed_value, each = (number_days*2))

    )
    
########################
# Format the dataframe #
########################
    df.long = df %>% gather(.,
                            key = "subscription",
                            value = "euro",
                            - rit)
  
# Drop the "altijd vrij" subscription when the costs are out of range other subscriptions
    
    if((number_days*2)*fare_value*1.5 < 351){
      df.long = df.long %>% filter(subscription != "altijd_vrij")
      
    }
    
    
#################
# plot the data #
#################
  
    # Create a custom theme for the plot 
    
    my.theme =
      theme_bw()+
      theme(text = element_text(),
            title = element_text(size = 15, colour = "black"),
            axis.text.x = element_text(size = 12, colour = "black"),
            axis.text.y = element_text(size = 12, colour = "black"),
            legend.title = element_text(size = 15, colour = "black"),
            legend.text = element_text(size = 12),
            legend.position = "left"
      )
    
    
   # Create and reender the main plot          
   
  output$mainplot = renderPlot({
    ggplot(df.long, aes(x = 31)) +
    geom_line(aes(x = rit/2, y = euro, colour = subscription))+
      labs(title = "Opbouw van de maandelijkse kosten per abonnement",
           x = "Dagen dat je reist",
           y = "Totale kosten in Euro's")+
      my.theme
  })

###################
# The final table #
###################
    
    df.sub = df.long %>% filter(., rit == (number_days*2)) %>%
      arrange(euro)
      
    
    output$maintable = renderTable(df.sub[,2:3])
    
###################
# Cheapest option #
###################
    
    output$bestoption = renderText({
      paste("Voordeligste abonnement:", df.sub[1,2])
    })
    }
  })
 

}
shinyApp(ui = ui, server = server)
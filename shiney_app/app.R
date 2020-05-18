library(shiny)
library(tidyverse)

# Data gebaseerd op https://www.ns.nl/ns-abonnementen/overzicht-abonnementen/


ui = fluidPage(titlePanel("Dé NS-abonnement calculator", windowTitle = "De NS-abonnement calculator"),
               sidebarLayout( #Here comes all the things related to the left sidebar (input)
                 sidebarPanel(
                              h4("Hoe vaak reis je?"),
                              selectInput("traveldaysInput", "Aantal dagen per maand:", choices = c(1:31)),
                              br(),
                              h4("Aantal ritten", em(strong("buiten")), "de spits"),
                              h6("Spitstijden:",br(), "ma t/m vr 06:30-09:00 & 16:00-18:30"),
                              uiOutput("offpeakOutput"),
                              br(),
                              h5(strong("Enkele ritprijs", HTML("<a href=https://www.ns.nl>via ns.nl</a>"), ":")),
                              textInput("farecostInput", label = NULL,
                                        value = ""),
                              br(),
                              h5(strong(em("Optioneel:"),"Prijs traject abonnement", HTML("<a href=https://www.ns.nl/webshop/nieuwproduct?0&product=TVM&reisklasse=2&contractduur=1MND&returnurl=https://www.ns.nl/abonnementen/traject-vrij.html>
                                   bereken hier (via ns.nl)</a>"),":")),
                              textInput("trajectfixedInput",label = NULL,
                                        value = ""),
                              
                              width = 3
                              
                              
                 ),
              mainPanel(
                fluidRow(includeHTML("app_introduction.html")),
                fluidRow(
                tabsetPanel(
                  tabPanel("Resultaten", icon = icon("chart-line"),
                           span(h2(textOutput("bestoption")),style="color:#336600"),
                           hr(),
                           fixedRow(
                           column(3,tableOutput("maintable")),
                           column(9,plotOutput("mainplot"))
                           )
                           
                  ),
                  tabPanel("HELP / INFO ",icon = icon("question-circle"),
                         source("Help_info_script.R")
                ),
                
                tabPanel("Abonnementen uitleg", icon = icon("list-alt"),
                          source("Abo_uitleg_script.R")
                         )
               )
               )
              )
)
)




server = function(input, output) {
  # Number of fares that are travelled during off peak times 
  # Depends on the number of days the user used as an input

output$info = renderText(help_info)
  
  output$offpeakOutput = renderUI({
    sliderInput("offpeakInput", "",
                min = 0, max = (as.numeric(input$traveldaysInput)*2), 
                value = 0, step = 1, round = TRUE)
  })
  


# Observe allows us to fetch the input data
  observe({

    
#################################
# Fetch user input to variables #
#################################
    
    fare_value =  as.numeric(paste(gsub(",", ".", input$farecostInput))) # includes substitution of user input of a comma to a dot using gsub
    
    traject_fixed_value = as.numeric(paste(gsub(",", ".", input$trajectfixedInput))) # includes substitution of user input of a comma to a dot using gsub
    
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
     
    Basis =  c(1:(number_days*2)*fare_value),
    
    Dal_Voordeel = 
    
      if(off_peak_fares == ((number_days*2))){
       5+ c(1:off_peak_fares*(fare_value*0.6)) 
      } 
    
    else if(off_peak_fares == 0){
       5+c(1:(number_days*2)*fare_value)
    } 
     
  else if(off_peak_fares != 0 & off_peak_fares != ((number_days*2))){
     5+ c(
     c(1:((number_days*2)-off_peak_fares)*fare_value),  # costs during peak
     
      c(1:off_peak_fares*(fare_value*0.6) + ((number_days*2)-off_peak_fares)*fare_value) # cost during off peak + cost made during peak  
     )  
    
  }
  ,
  Altijd_Voordeel = 
    
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
Dal_Vrij = 
  
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
    Altijd_Vrij = rep(351, each = (number_days*2))
,
Traject_Vrij = rep(traject_fixed_value, each = (number_days*2))

    )
     
    
########################
# Format the dataframe #
########################
    df.long = df %>% gather(.,
                            key = "Abonnement",
                            value = "Euro",
                            - rit)
  
# Drop the "altijd vrij" subscription when the costs are out of range other subscriptions
    
    if((number_days*2)*fare_value*1.5 < 351){
      df.long = df.long %>% filter(Abonnement != "Altijd_Vrij")
      
    }
    
    
#################
# plot the data #
#################
  
    # Create a custom theme for the plot 
    
    my.theme =
      theme_bw()+
      theme(text = element_text(),
            title = element_text(size = 15, colour = "black"),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size = 12, colour = "black"),
            axis.text.y = element_text(size = 12, colour = "black"),
            legend.title = element_text(size = 15, colour = "black"),
            legend.text = element_text(size = 12),
            legend.position = c(0.2, 0.6)
      )
    
    
   # Create and reender the main plot          
   
  output$mainplot = renderPlot({
    ggplot(df.long, aes(x = 31)) +
    geom_line(aes(x = rit/2, y = Euro, colour = Abonnement), size = 3)+
      scale_x_continuous(breaks = seq(1, number_days, by = 1)) +
      labs(title = "Opbouw van de maandelijkse kosten per abonnement",
           x = "Dagen dat je reist",
           y = "Totale kosten in Euro's")+
      my.theme
  })

###################
# The final table #
###################
    
    df.sub = df.long %>% filter(., rit == (number_days*2)) %>%
      arrange(Euro) 
      
    
    output$maintable = renderTable(df.sub[,2:3])
    
###################
# Cheapest option #
###################
    
    output$bestoption = 
      renderText({
      paste("Voordeligste abonnement:",gsub("_", " ", df.sub[1,2]))
    })
    }
  })
 

}
shinyApp(ui = ui, server = server)
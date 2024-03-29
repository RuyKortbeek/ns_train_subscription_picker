library(shiny)
library(tidyverse)
library(httr)

# Data based on https://www.ns.nl/ns-abonnementen/overzicht-abonnementen/

stations = read.csv(file = "NS_Stations_2019.csv", header = TRUE, stringsAsFactors = TRUE) %>% 
  filter(Land == "Netherlands") %>% select(Station, Code) %>% distinct() %>% arrange(Station)

ui = fluidPage(
  
  HTML('<meta name="viewport" content="width=1024">'), # Makes the app mobile-phone friendly
  
  titlePanel("Dé NS-abonnement calculator", windowTitle = "De NS-abonnement calculator"),
               sidebarLayout( #Here comes all the things related to the left sidebar (input)
                 sidebarPanel(selectInput("station_A", label = "Ik reis tussen:", 
                                          choices = stations$Station,
                                          selected = "Aalten"),
                              selectInput("station_B", label = "en", 
                                          choices = stations$Station,
                                          selected = "Aalten"),
                              radioButtons("classInput", label = NULL,
                                           choices = c("2e klas", "1e klas"),
                                           selected = "2e klas",
                                           inline = TRUE),
                              br(),
                              h4("Hoe vaak reis je?"),
                              sliderInput("traveldaysInput", "Aantal dagen per maand:", min = 1, max = 31, value = 1, step = 1, round = TRUE),
                              br(),
                              h4("Aantal ritten", em(strong("buiten")), "de spits"),
                              uiOutput("offpeakOutput"),
                              h6("Spitstijden:",br(), "ma t/m vr 06:30-09:00 & 16:00-18:30"),
                              width = 3
                              
                              
                 ),
              mainPanel(
                fluidRow(includeHTML("app_introduction.html")),
                fluidRow(
                tabsetPanel(
                  tabPanel("Resultaten", icon = icon("chart-line"),
                           span(h2(textOutput("bestoption")),style="color:#336600"),
                           span(h5(textOutput("costs")),style="black"),
                           span(h5(textOutput("costs_40")),style="black"),
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
    
     code_station_A = (stations %>% filter(Station == input$station_A))[,2]
     code_station_B = (stations %>% filter(Station == input$station_B))[,2]
    
    # Url to retrive data from
    ns.url = as.character(paste0("https://gateway.apiportal.ns.nl/public-prijsinformatie/prices?fromStation=",code_station_A,"&toStation=",code_station_B, sep = ""))
    
    # Ocp-Apim-Subscription-Key -> should be in header pirmaire sleutel
    sub.key = as.character("92fd805f312b4907840fa436a2af87df")
    
    # Host: gateway.apiportal.ns.nl
    host = as.character("gateway.apiportal.ns.nl")
    
    ################
    # HTTP Resuest #
    ################
    
    # Create a header to send with the request
    ns.header = add_headers(.headers = 
                              c(Host = host,
                                "Ocp-Apim-Subscription-Key" = sub.key))
    
    request.result = content( # conent() is a JSON parser - it will create a list() object
      GET(ns.url,
          ns.header)
    )
    
    ##########################################################
    # Get data you want (price single fare without discount) #
    ##########################################################
    
    if (is.list(request.result) == TRUE){
    # Price single fare
    if (input$classInput == "2e klas"){
      fare_value = request.result$priceOptions[[2]]$totalPrices[[1]]$price/100 # 2nd class
    } else{
      fare_value = as.numeric((request.result$priceOptions[[2]]$totalPrices[[2]]$price)/100) # 1st class
    }
  
    # Traject vrij
    if (input$classInput == "2e klas"){
    traject_fixed_value = as.numeric((request.result$priceOptions[[2]]$totalPrices[[17]]$price)/100) # 2nd class This is the monthly price for a year - subscribtion
    } else{
      traject_fixed_value =  as.numeric((request.result$priceOptions[[2]]$totalPrices[[18]]$price)/100) # 1st class
    }
    }
    
    # Altijd vrij
    if (input$classInput == "2e klas"){
      always_free_value = 362.4
    } else{
      always_free_value = 512.5
    }
    
    # Start costs Dal vrij
    if (input$classInput == "2e klas"){
      dal_vrij_start =  107.9 # start cost are taken from www.ns.nl
    } else{
      dal_vrij_start =  136.4 # 1st class
    } 
    
    number_days = as.numeric(input$traveldaysInput)
    
    off_peak_fares = as.numeric(input$offpeakInput)
   
    
# Set requirements for the input values to stop the App from bugging 
   req(fare_value > 1) # makes sure App does not bug when there is two times the same station as input
    
    if(!is.null(input$offpeakInput) & !is.null(input$traveldaysInput))  # makes sure df is not bugging when switching off-peak / travelsdays (during switch values becomes NULL)
      {

###################################################
# Create the main dataframe with the subscriptions #
###################################################

    df= data.frame(
     rit =  c(1:(number_days*2)),

    Basis =  c(1:(number_days*2)*fare_value)
    ,

    Dal_Voordeel =

      if(off_peak_fares == ((number_days*2))){
       5.1+ c(1:off_peak_fares*(fare_value*0.6))
      }

    else if(off_peak_fares == 0){
       5.1 +c(1:(number_days*2)*fare_value)
    }

  else if(off_peak_fares != 0 & off_peak_fares != ((number_days*2))){
     5.1 + c(
     c(1:((number_days*2)-off_peak_fares)*fare_value),  # costs during peak

      c(1:off_peak_fares*(fare_value*0.6) + ((number_days*2)-off_peak_fares)*fare_value) # cost during off peak + cost made during peak
     )

  }
    ,
  
  Altijd_Voordeel =

    if(off_peak_fares == ((number_days*2))){
     24.2+ c(1:off_peak_fares*(fare_value*0.6))
    }

   else if(off_peak_fares == 0){
     24.2+ c(1:(number_days*2)*fare_value*0.8)
   }

   else if(off_peak_fares != 0 & off_peak_fares != ((number_days*2))){
     24.2+ c(
       c(1:((number_days*2)-off_peak_fares)*fare_value*0.8),  # costs during peak

       c(1:off_peak_fares*(fare_value*0.6) + ((number_days*2)-off_peak_fares)*fare_value*0.8) # cost during off peak + cost made during peak
       )
   }
    
 ,
 Dal_Vrij =

  if(off_peak_fares == ((number_days*2))){
     dal_vrij_start + c(1:off_peak_fares*(fare_value*0))
   }

 else if(off_peak_fares == 0){
     dal_vrij_start + c(1:(number_days*2)*fare_value)
 }

 else if(off_peak_fares != 0 & off_peak_fares != ((number_days*2))){
   dal_vrij_start + c(
     c(1:((number_days*2)-off_peak_fares)*fare_value),  # costs during peak

     c(1:off_peak_fares*(fare_value*0) + ((number_days*2)-off_peak_fares)*fare_value) # cost during off peak + cost made during peak
     )

 }
  
 ,
 Traject_Vrij = rep(traject_fixed_value, each = (number_days*2))
 
 ,
 Altijd_Vrij = rep(always_free_value, each = (number_days*2))

     )


########################
# Format the dataframe #
########################
    df.long = df %>% gather(.,
                            key = "Abonnement",
                            value = "Euro",
                            - rit)

# Drop the "altijd vrij" subscription when the costs are out of range other subscriptions

    if((number_days*2)*fare_value*1.5 < always_free_value){
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
      scale_fill_hue(l =45) +
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
    
    output$costs =
      renderText({
        paste("Enkele rit:", fare_value,"euro")
      })
    output$costs_40 =
      renderText({
        paste("40% korting:", round(fare_value*0.6, digits = 2), "euro")
      })
    
    }
  })
}
shinyApp(ui = ui, server = server)
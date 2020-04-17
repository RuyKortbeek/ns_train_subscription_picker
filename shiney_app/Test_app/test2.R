library(shiny)

ui<-shinyUI(fluidPage(
  titlePanel("Model"),
  
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("Var1",label="Variable 1",min=100,max=1000,value=200,step=1),
      sliderInput("Var2",label="Variable 2",min=500,max=800,value=600,step=1),
      sliderInput("Var3",label="Variable 3",min=0,max=1,value=0.5,step=0.01),
      sliderInput("Var4",label="Variable 4",min=1,max=20,value=5,step=1),
      sliderInput("Var5",label="Variable 5",min=0,max=1,value=0.6,step=0.01),
      actionButton("Run_model", "Run model")
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("model summary", verbatimTextOutput('summary'))
      )
    )
  )))


server<- function(input,output,session){
  set.seed(1234)
  observe({
    # this is how you fetch the input variables from ui component
    
    Var1 <- as.integer(input$Var1)
    Var2 <- as.integer(input$Var2)
    Var3 <- as.numeric(input$Var3)
    Var4 <- as.integer(input$Var4)
    Var5 <- as.numeric(input$Var4)
    
    test <- cbind(Var1, Var2, Var3, Var4, Var5)
    test <- as.data.frame(test)
    
    ## Import dataset
    train <- data.frame(replicate(6,sample(1:100,1000,rep=TRUE)))
    names(train) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
    
    # Var6 is the dependent variable
    test$Var6 <- ""
    
    # Model action button
    observeEvent(input$Run_model, {
      model <- randomForest(Var6 ~ .,  data = train, ntree=500)
      pred <- predict(model, newdata = test)
      output$summary <- renderPrint(model)
      
    })
  })
}

shinyApp(ui=ui, server=server)
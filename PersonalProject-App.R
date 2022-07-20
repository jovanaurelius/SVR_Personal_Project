setwd("C:/Users/User/Documents/Seminar/Data")

install.packages("shinythemes")

library("shinythemes")
library("readxl")
library(e1071)
library(MLmetrics)
library(Metrics)

data <- read_excel("mydata.xlsx")
df <- data.frame(
  Date = data$Date2,
  Price = data$Price,
  
  stringsAsFactors = FALSE
)


normalize <- function(x, min, max) (x-min)/(max-min)
denormalize <- function(x, min, max) x*(max-min)+min

price.norm <- df$Price
price.range <- range(df$Price)

#Apply min-max normalization
df$Price <- normalize(price.norm, price.range[1], price.range[2])

#Data train test splitting
train_indices <- seq_len(length.out = floor(x = 0.7 * nrow(x = df)))
train <- df[train_indices,]
test <- df[-train_indices,]

#tes shiny
ui <- fluidPage(theme = shinytheme("superhero"),
                
                # App title ----
                titlePanel("SVR Model"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    selectInput("select", h3("Kernel Type"), 
                                choices = list("Linear" = 1, "Third Degree Polynomial" = 2,
                                               "RBF" = 3), selected = 1),
                    numericInput("num1", 
                                 h3("Cost"), 
                                 value = 1),
                    numericInput("num2", 
                                 h3("Epsilon"), 
                                 value = 0.1),
                    numericInput("num3", 
                                 h3("Gamma"), 
                                 value = 1)
                    #textOutput("txt")
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # Output: Histogram ----
                    plotOutput("plott"),
                    textOutput("RMSE"),
                    textOutput("MAPE"),
                    textOutput("Prediction1"),
                    textOutput("Prediction2"),
                    textOutput("Prediction3"),
                    textOutput("Prediction4"),
                    textOutput("Prediction5"),
                    textOutput("Prediction6"),
                    textOutput("Prediction7"),
                    textOutput("Prediction8"),
                    textOutput("Prediction9"),
                    textOutput("Prediction10"),
                    textOutput("Prediction11"),
                    textOutput("Prediction12"),
                    textOutput("Prediction13"),
                    textOutput("Prediction14"),
                    textOutput("Prediction15"),
                    textOutput("Prediction16"),
                    textOutput("Prediction17"),
                    textOutput("Prediction18"),
                    textOutput("Prediction19"),
                    textOutput("Prediction20")
                    
                  ),
                  position = c("right"),
                )
)

server <- function(input, output, session) {
  myreactive <- reactive({
    input$select
  })
  
  myreactive2 <- reactive({
    input$num1
  })
  
  myreactive3 <- reactive({
    input$num2
  })
  
  myreactive4 <- reactive({
    input$num3
  })
  
  output$plott <- renderPlot({
    datepred <- data.frame(Date = c(1536:1555))
    xdate = 462:481
    lentest <- 1:length(test$Price)
    testprice.denorm <- denormalize(test$Price, price.range[1], price.range[2])
    plot(lentest, testprice.denorm, xlab = "Day", ylab = "Price", xlim = c(0,500), pch=18, col="black")
    
    if(myreactive() == 1){
      modelsvr = svm(Price~Date,test,type = "eps-regression", kernel = "linear", cost = myreactive2(), epsilon = myreactive3())
      predYsvr = predict(modelsvr, test)
      
      testpred.denorm <- denormalize(predYsvr, price.range[1], price.range[2])
      lines(lentest, testpred.denorm, lwd="2", col="red")
      
      pricepred = predict(modelsvr, newdata = datenew)
      pricepred = denormalize(pricepred, price.range[1], price.range[2])
      points(xdate, pricepred, col = "red", pch=18)
    } else if(myreactive() == 2){
      modelsvr = svm(Price~Date,test,type = "eps-regression", kernel = "polynomial", degree = 3, cost = myreactive2(), epsilon = myreactive3())
      predYsvr = predict(modelsvr, test)
      
      testpred.denorm <- denormalize(predYsvr, price.range[1], price.range[2])
      lines(lentest, testpred.denorm, lwd="2", col="red")
      
      pricepred = predict(modelsvr, newdata = datenew)
      pricepred = denormalize(pricepred, price.range[1], price.range[2])
      points(xdate, pricepred, col = "red", pch=18)
    } else if(myreactive() == 3){
      modelsvr = svm(Price~Date,test,type = "eps-regression", kernel = "radial", cost = myreactive2(), gamma = myreactive4(), epsilon = myreactive3())
      predYsvr = predict(modelsvr, test)
      
      testpred.denorm <- denormalize(predYsvr, price.range[1], price.range[2])
      lines(lentest, testpred.denorm, lwd="2", col="red")
      
      pricepred = predict(modelsvr, newdata = datenew)
      pricepred = denormalize(pricepred, price.range[1], price.range[2])
      points(xdate, pricepred, col = "red", pch=18)
    }
  })
  
  myreactive5 <- reactive({
    if(myreactive() == 1){
      modelsvr = svm(Price~Date,test,type = "eps-regression", kernel = "linear", cost = myreactive2(), epsilon = myreactive3())
      predYsvr = predict(modelsvr, test)
    } else if(myreactive() == 2){
      modelsvr = svm(Price~Date,test,type = "eps-regression", kernel = "polynomial", degree = 3, cost = myreactive2(), epsilon = myreactive3())
      predYsvr = predict(modelsvr, test)
    } else if(myreactive() == 3){
      modelsvr = svm(Price~Date,test,type = "eps-regression", kernel = "radial", cost = myreactive2(), gamma = myreactive4(), epsilon = myreactive3())
      predYsvr = predict(modelsvr, test)
    }
    
    RMSEBst=rmse(predYsvr,test$Price)
  })
  
  myreactive6 <- reactive({
    if(myreactive() == 1){
      modelsvr = svm(Price~Date,test,type = "eps-regression", kernel = "linear", cost = myreactive2(), epsilon = myreactive3())
      predYsvr = predict(modelsvr, test)
    } else if(myreactive() == 2){
      modelsvr = svm(Price~Date,test,type = "eps-regression", kernel = "polynomial", degree = 3, cost = myreactive2(), epsilon = myreactive3())
      predYsvr = predict(modelsvr, test)
    } else if(myreactive() == 3){
      modelsvr = svm(Price~Date,test,type = "eps-regression", kernel = "radial", cost = myreactive2(), gamma = myreactive4(), epsilon = myreactive3())
      predYsvr = predict(modelsvr, test)
    }
    
    MAPEBst = MAPE(predYsvr, test$Price)
  })
  
  output$RMSE <- renderText({
    paste0("RMSE : ", myreactive5())
  })
  
  output$MAPE <- renderText({
    paste0("MAPE : ", myreactive6())
  })
  
  myreactive7 <- reactive({
    datepred <- data.frame(Date = c(1536:1555))
    if(myreactive() == 1){
      modelsvr = svm(Price~Date,test,type = "eps-regression", kernel = "linear", cost = myreactive2(), epsilon = myreactive3())
      predYsvr = predict(modelsvr, test)
    } else if(myreactive() == 2){
      modelsvr = svm(Price~Date,test,type = "eps-regression", kernel = "polynomial", degree = 3, cost = myreactive2(), epsilon = myreactive3())
      predYsvr = predict(modelsvr, test)
    } else if(myreactive() == 3){
      modelsvr = svm(Price~Date,test,type = "eps-regression", kernel = "radial", cost = myreactive2(), gamma = myreactive4(), epsilon = myreactive3())
      predYsvr = predict(modelsvr, test)
    }
    
    pricepred = predict(modelsvr, newdata = datenew)
    pricepred = denormalize(pricepred, price.range[1], price.range[2])
  })
  
  output$Prediction1 <- renderText({
    paste0("Day 1 Prediction : ", myreactive7()[1])
  })
  
  output$Prediction2 <- renderText({
    paste0("Day 2 Prediction : ", myreactive7()[2])
  })
  
  output$Prediction3 <- renderText({
    paste0("Day 3 Prediction : ", myreactive7()[3])
  })
  
  output$Prediction4 <- renderText({
    paste0("Day 4 Prediction : ", myreactive7()[4])
  })
  
  output$Prediction5 <- renderText({
    paste0("Day 5 Prediction : ", myreactive7()[5])
  })
  
  output$Prediction6 <- renderText({
    paste0("Day 6 Prediction : ", myreactive7()[6])
  })
  
  output$Prediction7 <- renderText({
    paste0("Day 7 Prediction : ", myreactive7()[7])
  })
  
  output$Prediction8 <- renderText({
    paste0("Day 8 Prediction : ", myreactive7()[8])
  })
  
  output$Prediction9 <- renderText({
    paste0("Day 9 Prediction : ", myreactive7()[9])
  })
  
  output$Prediction10 <- renderText({
    paste0("Day 10 Prediction : ", myreactive7()[10])
  })
  
  output$Prediction11 <- renderText({
    paste0("Day 11 Prediction : ", myreactive7()[11])
  })
  
  output$Prediction12 <- renderText({
    paste0("Day 12 Prediction : ", myreactive7()[12])
  })
  
  output$Prediction13 <- renderText({
    paste0("Day 13 Prediction : ", myreactive7()[13])
  })
  
  output$Prediction14 <- renderText({
    paste0("Day 14 Prediction : ", myreactive7()[14])
  })
  
  output$Prediction15 <- renderText({
    paste0("Day 15 Prediction : ", myreactive7()[15])
  })
  
  output$Prediction16 <- renderText({
    paste0("Day 16 Prediction : ", myreactive7()[16])
  })
  
  output$Prediction17 <- renderText({
    paste0("Day 17 Prediction : ", myreactive7()[17])
  })
  
  output$Prediction18 <- renderText({
    paste0("Day 18 Prediction : ", myreactive7()[18])
  })
  
  output$Prediction19 <- renderText({
    paste0("Day 19 Prediction : ", myreactive7()[19])
  })
  
  output$Prediction20 <- renderText({
    paste0("Day 20 Prediction : ", myreactive7()[20])
  })
}

shinyApp(ui = ui, server = server)
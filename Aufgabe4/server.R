library(shiny)
library(caret)
library(e1071)
library(ggplot2)
library(nnet)
library(NeuralNetTools)

df <- read.csv(file = "wein_fixed.csv")

set.seed(1)
intrain <- createDataPartition(y = df$Weinsorte, p= 0.7, list = FALSE)
training <- df[intrain,]
testing <- df[-intrain,]
training[["Weinsorte"]] = factor(training[["Weinsorte"]])
Weinsorte <- df$Weinsorte

shinyServer(function(input, output) {
  
  output$svm <- renderPlot({
    pca <- preProcess(x = training[-14], method = "pca", pcaComp = 2)
    training <- predict(pca, training)
    training <- training[c(2,3,1)]
    testing <- predict(pca, testing)
    testing <- testing[c(2,3,1)]
    if (input$s1 == "Linear") {
      #u*v
      svm <- svm(Weinsorte ~., data = training, kernel = "linear")
    } else if (input$s1 == "Polynomial") {
      #(u*v+1)^degree
      svm <- svm(Weinsorte ~., data = training, kernel = "polynomial", degree = input$n1, coef0 = 1, gamma = 1)
    } else if (input$s1 == "Radial Basis") {
      #exp(-gamma*|u*v|^2)
      svm <- svm(Weinsorte ~., data = training, kernel = "radial", gamma = input$n2)
    }
    plot(svm, testing)
  })
  
  output$knn <- renderPlot({
    pca <- preProcess(x = training[-14], method = "pca", pcaComp = 2)
    training <- predict(pca, training)
    training <- training[c(2,3,1)]
    testing <- predict(pca, testing)
    testing <- testing[c(2,3,1)]
    nnet <- nnet(Weinsorte ~ ., data = training, size = 2, 
                 decay = 1.0e-5, maxit = 50)
    cm <- table(testing$Weinsorte, predict(nnet, testing, type = "class"))
    print(nnet)
  })
})


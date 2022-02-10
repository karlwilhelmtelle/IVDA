library(shiny)
library(caret)
library(e1071)
library(ggplot2)
library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(matrixStats)
library(clusterGeneration)
library(devtools)

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
    ########  usage of nnet ###########
    # Skalierung der Daten
    training[,-14] <- scale(training[,-14])
    testing[,-14] <- scale(testing[,-14])
    
    # 3d vector mit size, decay, max iterations
    nnetParams = vector("list", length = 3)

    # size: Größe des hidden layers
    # size = 127 * 2/3 = 85
    # alternativ: size = Mittelwert von input und output layer (127 und 3) = 65
    # decay bremst Overfitting auf Trainingsdatensatz
    
    nnetParams[[1]] <- c(4, 0.2, 100)

    nnetParams[[2]] <- c(2, 0.2, 100)

    nnetParams[[3]] <- c(7, 0.2, 100)
    
    #import the function from Github
    source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
    i <- 1
    for (param in nnetParams) {
      writeLines("\n\n")
      set.seed(1)
      nnet <- nnet(Weinsorte ~ ., data = training, size = param[[1]],
                   decay = param[[2]], maxit = param[[3]])
      cm <- table(testing$Weinsorte, predict(nnet, testing, type = "class"))
      print(cm)
      
      pdf(paste('./nn-example',i,'.pdf'), width = 20, height = 7)
      plot.nnet(nnet, alpha.val = 0.5, circle.col = list('lightgray', 'white'), bord.col = 'black')
      dev.off()
      i <- i + 1
    }
    
    # # Skalierung der Daten
    # training[,-14] <- scale(training[,-14])
    # testing[,-14] <- scale(testing[,-14])
    # neuralnets = vector("list", length = 3)
    # set.seed(1)
    # neuralnets[[1]] = neuralnet(Weinsorte ~ ., data = training, hidden = 4)
    # set.seed(1)
    # neuralnets[[2]] = neuralnet(Weinsorte ~ ., data = training, hidden = 2)
    # set.seed(1)
    # neuralnets[[3]] = neuralnet(Weinsorte ~ ., data = training, hidden = 7)
    # 
    # for (nnet in neuralnets) {
    #   # Topologie des Netzes visualisieren
    #   plot(nnet)
    #   
    #   print(nnet$result.matrix[1,1])
    # }
  })
})


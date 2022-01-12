library(shiny)
library(ggplot2)
library(cluster)
library(class)
library(caret)

df <- read.csv(file = "housing.csv")
dfoWS <- df[1:9]

shinyServer(function(input, output) {
  
  output$plotgraphA = renderPlot({
    set.seed(1)
    dfoWS <- scale(dfoWS)
    km <- kmeans(dfoWS,input$n3)
    d <- daisy(dfoWS, metric = "euclidean")
    plot(silhouette(km$cluster, d))
  })
  
  output$plotgraphB = renderPlot({
    pca <- prcomp(dfoWS,scale=T)
    dat <-pca$x[,1:2]
    km <- kmeans(dat,input$n4)
    mydf <- data.frame(PCA1 = pca$x[,1], PCA2 = pca$x[,2], cluster = factor(km$cluster))
    
    ggplot(mydf, aes(x=PCA1, y=PCA2, color = cluster)) + geom_point()
  })

  })
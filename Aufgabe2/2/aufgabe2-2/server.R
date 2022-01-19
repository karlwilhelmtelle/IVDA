library(shiny)
library(cluster)
library(class)
library(caret)
library(factoextra)

df <- read.csv(file = "housing.csv")
df <- df[3:9]
df <- na.omit(df)


shinyServer(function(input, output) {
  
  output$plotKMeans = renderPlot({
     fviz_nbclust(df, kmeans, method = "silhouette", k.max = 15) 
  })
  
  output$plotPCA = renderPlot({
    optimalNumberOfClusters <- kmeans(df, 2, nstart = 25)
    fviz_cluster(optimalNumberOfClusters, data = df)
  })

})
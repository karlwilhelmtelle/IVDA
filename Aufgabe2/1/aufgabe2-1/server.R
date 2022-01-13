library(purrr)
library(dplyr)
library(modelr)
library(ggplot2)
library(gridExtra)
library(caret)
df <- read.csv("titanic.csv")

# Datensätze mit ID über 891 kann man nicht zum Trainieren und Testen verwenden,
# da "Survived" nicht gesetzt ist
df <- subset(df, PassengerId <= 891)

# Zeilen, in den "NA" Werte stehen, können nicht zum Trainieren verwendet werden
df <- na.exclude(df)

# cross validation
set.seed(1)
folds <- createFolds(df$Survived, k = 10)
errorRates = vector(mode = "numeric", length = 10)
confusionMatrixSum = vector(mode = "numeric", length = 4)

i = 1
for (index in folds) {
  train_df <- df[-index,]
  test_df <- df[index,]
  
  train_df$Survived[train_df$Survived==1] <- "alive"
  train_df$Survived[train_df$Survived==0] <- "dead"
  
  test_df$Survived[test_df$Survived==1] <- "alive"
  test_df$Survived[test_df$Survived==0] <- "dead"
  
  train_df$Survived <- as.factor(train_df$Survived)
  test_df$Survived <- as.factor(test_df$Survived)
  
  class(train_df$Survived)
  class(test_df$Survived)
  
  ctrlspecs <- trainControl(method = "cv", number = 10,
                            savePredictions = "all",
                            classProbs = TRUE)
  
  set.seed(1)
  model1 <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch,
                  data = train_df,
                  method = "glm", family = binomial,
                  trControl = ctrlspecs)
  
  predictions <- predict(model1, newdata = test_df)
  
  confMatrix <- confusionMatrix(data = predictions, test_df$Survived)
  
  accuracy <- confMatrix$overall['Accuracy']
  
  errorRate <- unname(1 - accuracy)
  
  errorRates[i] <- errorRate
  confMatrixList <- as.vector(confMatrix$table)
  confusionMatrixSum <- confusionMatrixSum + confMatrixList
  
  i <- i + 1
}

errorMedian = median(errorRates)

# bootstrap
set.seed(1)
# bootstrapped dataset
indexBootsList <- createResample(df$Survived, times = 10, list = FALSE)
errorRatesBoot = vector(mode = "numeric", length = 10)
confusionMatrixSumBoot = vector(mode = "numeric", length = 4)

for (i in 1:10) {
  #print(indexBootsList[,indexBoots])
  train_df <- df[indexBootsList[,i],]
  test_df <- df[-indexBootsList[,i],]
  
  train_df$Survived[train_df$Survived==1] <- "alive"
  train_df$Survived[train_df$Survived==0] <- "dead"
  
  test_df$Survived[test_df$Survived==1] <- "alive"
  test_df$Survived[test_df$Survived==0] <- "dead"
  
  train_df$Survived <- as.factor(train_df$Survived)
  test_df$Survived <- as.factor(test_df$Survived)
  
  ctrlspecs <- trainControl(method = "boot632", number = 1)
  
  set.seed(1)
  model <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch,
                 data = train_df,
                 method = "glm", family = binomial,
                 trControl = ctrlspecs)
  
  predictions <- predict(model, newdata = test_df)
  
  confMatrix <- confusionMatrix(data = predictions, test_df$Survived)
  
  accuracy <- confMatrix$overall['Accuracy']
  
  errorRate <- unname(1 - accuracy)
  errorRatesBoot[i] <- errorRate
  
  confMatrixList <- as.vector(confMatrix$table)
  confusionMatrixSumBoot <- confusionMatrixSumBoot + confMatrixList
}

errorMedianBoot = median(errorRatesBoot)

shinyServer(function(input, output) {
  output$plotgraph <- renderPlot({
  })
})
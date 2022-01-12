shinyServer(function(input, output) {
  df <- read.csv("titanic.csv", sep=",", header=TRUE)
  
  output$plotgraph = renderPlot({
    
  })
})
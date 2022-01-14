shinyUI(fluidPage(
  navbarPage("Aufgabe 2 - Teil 1"),
  mainPanel(plotOutput("plotgraph", "500px", "500px"),
            textOutput("cvMedian"),
            verbatimTextOutput("confusionMatrixCV"),
            plotOutput("plotgraphBoot", "500px", "500px"),
            textOutput("bootMedian"),
            verbatimTextOutput("confusionMatrixBoot"))
))
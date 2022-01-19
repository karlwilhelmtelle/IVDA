library(shiny)

shinyUI(
  fluidPage(
    navbarPage("Aufgabe 2 - Teil 2",
      tabPanel("K-means",
        sidebarLayout(
          sidebarPanel("Clustering using k-means"),
          mainPanel("main panel", column(6,plotOutput(outputId="plotKMeans")))
          )
        ),
      tabPanel("PCA",
        sidebarLayout(
          sidebarPanel("Visualization using Silhoutte Coefficient"),
          mainPanel("main panel", column(6,plotOutput(outputId="plotPCA")))
          )
        )
      )
    )
)
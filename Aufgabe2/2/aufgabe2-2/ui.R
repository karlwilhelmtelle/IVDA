library(shiny)

shinyUI(fluidPage(
  
  navbarPage("Aufgabe 2 - Teil 2",
             
             tabPanel("K-means",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("n3", "Anzahl Cluster: ", value = 2, min = 2, max = 15)
                        ),
                        
                        mainPanel("main panel",
                                  column(6,plotOutput(outputId="plotgraphB", width="1000px",height="900px")))
                      )
             ),
             tabPanel("PCA",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("n4", "Anzahl Cluster: ", value = 2, min = 2, max = 15)
                        ),
                        
                        mainPanel("main panel",
                                  column(6,plotOutput(outputId="plotgraphC", width="1000px",height="900px")))
                      )
             )
             
  )
))
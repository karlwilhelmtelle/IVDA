library(shiny)

shinyUI(fluidPage(
  
  navbarPage("Aufgabe 4",
             tabPanel("SVM",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("s1","Kernel",c("Linear", "Polynomial","Radial Basis")),
                          conditionalPanel(
                            condition = "input.s1 == 'Polynomial'",
                            numericInput("n1", "degree", min = 1, max = 10, step = 1, value = 1),
                          ),
                          conditionalPanel(
                            condition = "input.s1 == 'Radial Basis'",
                            numericInput("n2","gamma", min = 0.1, max = 1, value = 0.1, step = 0.1)
                          )
                        ),
                        
                        mainPanel("main panel",
                                  column(6,plotOutput(outputId="svm", width="900px",height="800px"),
                                         verbatimTextOutput(outputId = "conv")))
                      )
             )
  )
))

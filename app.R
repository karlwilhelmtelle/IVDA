library("readxl")
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gridExtra)
library(scales)

df <- read.csv(file="Aufgabe-1.csv", sep=",", header=TRUE)

Items <- unique(sort(df[,2]))
nationalities <- unique(sort(df[,5]))
clubs <- unique(sort(df[,8]))


ui <- fluidPage(
  titlePanel("Fußballspieler"),
  tags$head(tags$style(HTML("hr {border-top: 1px solid #BEBEBE;}"))),
  sidebarLayout(position = "left",
                sidebarPanel("Filteroptionen",
                             # checkboxInput("a1", "Aufgabe 1", value = F),
                             sliderTextInput( "sliderPlayers",
                                              "Auswahl Spieler die angezeigt werden:",
                                              choices = Items,
                                              selected = Items[c(100, 1800)]),
                             checkboxInput("log1", "Log10 Skalierung", value = F),

                             hr(),

                             checkboxInput("sliderAgeWage", "Gegenüberstellung Age-Wage", value = T),
                             checkboxInput("aliderAgeOverall", "Gegenüberstellung Age-Overall", value = F),
                             hr(),
                             
                             #Teil c: Filtern
                             selectInput("selectFilter", "Filter:",
                                         c("Nationality" = "nationality",
                                           "Club" = "club")),
                             
                             sliderTextInput( "sliderChooseNationality",
                                              "Filter choice nationality",
                                              choices = nationalities),
                             
                             sliderTextInput( "sliderChooseClub",
                                              "Filter choice club",
                                              choices = clubs)
                            




                ),
                mainPanel("main panel",
                          column(6,plotOutput(outputId="plotgraph", width="1000px",height="900px"))
                )
  )
)

server <- function(input, output) {

  pt1 <- reactive({
    if (!input$sliderAgeWage) return (NULL)
    fromPlayer <- match(input$sliderPlayers[1], Items)
    toPlayer <- match(input$sliderPlayers[2], Items)
    itemsInRange <- unique(sort(Items[fromPlayer:toPlayer]))
    
    # if(input$selectFilter == "nationality"){
    #   itemsFilterNationality <- subset(df, Nationality %in% input$sliderChooseNationality)
    #   p1 <- ggplot(subset(df, Name %in% itemsFilterNationality), aes(x=Age, y = Wage)) + geom_boxplot() +
    #     labs(title = "Age-Wage (Filter: Nationality)", x = "Age", y = "Wage")
    # }else if(input$selectFilter == "club"){
    #   itemsFilterClub <- subset(df, Club %in% input$sliderChooseClub)
    #   p1 <- ggplot(subset(df, Name %in% itemsFilterClub), aes(x= Age, y = Wage)) + geom_boxplot() +
    #     labs(title = "Age-Wage (Filter: Club)", x = "Age", y = "Wage")
    # } else {
    #   
    # }
    
    p1 <- ggplot(subset(df, Name %in% itemsInRange), aes(x=Age, y = Wage)) + geom_point() +
           labs(title = "Age-Wage", x = "Age", y = "Wage")
    
    p1
    
  })

  output$plotgraph = renderPlot({
    ptlist <- list(pt1())
    #loesche die Null Plots von der Liste
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    grid.arrange(grobs=ptlist,ncol=length(ptlist))
  })
}

shinyApp(ui = ui, server = server)
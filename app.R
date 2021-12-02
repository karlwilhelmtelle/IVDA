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
                             # I don't think this is needed, but i'll keep it just in case
                             # sliderTextInput( "sliderPlayers",
                             #                  "Range of players to be shown:",
                             #                  choices = Items,
                             #                  selected = Items[c(100, 1800)]),
                             
                             hr(),
                             
                             checkboxInput("checkBoxAgeWage", "Gegenüberstellung Age-Wage", value = T),
                             checkboxInput("checkBoxAgeOverall", "Gegenüberstellung Age-Overall", value = F),
                             hr(),
                             
                             #Teil c: Filter
                             selectInput("selectFilter", "Filter:",
                                         c("None" = "none",
                                           "Nationality" = "nationality",
                                           "Club" = "club")),
                             
                             sliderTextInput( "sliderChooseNationality",
                                              "Filter choice nationality",
                                              choices = nationalities),
                             
                             sliderTextInput( "sliderChooseClub",
                                              "Filter choice club",
                                              choices = clubs),
                             hr(),
                             
                             #Teil d: Database row selection
                             textInput(
                               inputId = "rowSelection1",
                               label = "First row to compare:",
                               value="1",
                               width=  '400px'
                               ),
                             
                             textInput(
                               inputId = "rowSelection2",
                               label = "Second row to compare:",
                               value="10",
                               width=  '400px'
                             ), 
                             
                             checkboxInput("checkBoxCompareRows", "Compare rows?", value = F)
                             
                ),
                mainPanel("main panel",
                          column(6,plotOutput(outputId="plotgraph", width="1000px",height="900px"))
                )
  )
)

server <- function(input, output) {
  
  wageAgeCompare <- reactive({
    if (!input$checkBoxAgeWage) return (NULL)
   
    if(input$checkBoxCompareRows) {
      row1 <- as.integer(input$rowSelection1)
      row2 <- as.integer(input$rowSelection2)
      playerName1 <- df[row1,2]
      playerName2 <- df[row2,2]
      print(playerName1)
      print(playerName2)
      itemsInRange <- c(playerName1, playerName2)
    } else {
      fromPlayer <- match(input$sliderPlayers[1], Items)
      toPlayer <- match(input$sliderPlayers[2], Items)
      itemsInRange <- unique(sort(Items[fromPlayer:toPlayer]))
    }
    
    
    if(input$selectFilter == "nationality"){
      p1 <- ggplot(subset(df, ((Name %in% itemsInRange) & (Nationality %in% input$sliderChooseNationality))), aes(x=Age, y = Wage))  + geom_point(color = "#FF0000") +
        labs(title = "Age-Wage", x = "Age", y = "Wage")
    }else if(input$selectFilter == "club"){
      p1 <- ggplot(subset(df, ((Name %in% itemsInRange) & (Club %in% input$sliderChooseClub))), aes(x= Age, y = Wage)) + geom_point(color = "#FF0000") +
        labs(title = "Age-Wage", x = "Age", y = "Wage")
    } else {
      p1 <- ggplot(subset(df, Name %in% itemsInRange), aes(x=Age, y = Wage)) + geom_point(color = "#FF0000") +
        labs(title = "Age-Wage", x = "Age", y = "Wage")
    }
    
    p1
    
  })
  
  overallAgeCompare <- reactive({
    if (!input$checkBoxAgeOverall) return (NULL)
    
    if(input$checkBoxCompareRows) {
      row1 <- as.integer(input$rowSelection1)
      row2 <- as.integer(input$rowSelection2)
      playerName1 <- df[row1,2]
      playerName2 <- df[row2,2]
      print(playerName1)
      print(playerName2)
      itemsInRange <- c(playerName1, playerName2)
    } else {
      fromPlayer <- match(input$sliderPlayers[1], Items)
      toPlayer <- match(input$sliderPlayers[2], Items)
      itemsInRange <- unique(sort(Items[fromPlayer:toPlayer]))
    }
    
    
    if(input$selectFilter == "nationality"){
      p1 <- ggplot(subset(df, ((Name %in% itemsInRange) & (Nationality %in% input$sliderChooseNationality))), aes(x=Age, y = Overall))  + geom_point(color = "#FF0000") +
        labs(title = "Age-Overall", x = "Age", y = "Overall")
    }else if(input$selectFilter == "club"){
      p1 <- ggplot(subset(df, ((Name %in% itemsInRange) & (Club %in% input$sliderChooseClub))), aes(x= Age, y = Overall)) + geom_point(color = "#FF0000") +
        labs(title = "Age-Overall", x = "Age", y = "Overall")
    } else {
      p1 <- ggplot(subset(df, Name %in% itemsInRange), aes(x=Age, y = Overall)) + geom_point(color = "#FF0000") +
        labs(title = "Age-Overall", x = "Age", y = "Overall")
    }
    
    p1
    
  })
  
  output$plotgraph = renderPlot({
    ptlist <- list(wageAgeCompare(), overallAgeCompare())
    #loesche die Null Plots von der Liste
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    grid.arrange(grobs=ptlist,ncol=length(ptlist))
  })
}

shinyApp(ui = ui, server = server)
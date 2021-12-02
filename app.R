library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gridExtra)
library(scales)
library(stringr)


df <- read.csv(file="Aufgabe-1.csv", sep=",", header=TRUE)

nationalities <- unique(sort(df[,5]))
clubs <- unique(sort(df[,8]))
df$Wage <- str_remove_all(df$Wage, "[€]")
df$Wage <- str_replace_all(df$Wage, "[K]", "000")
df$Wage <- str_replace_all(df$Wage, "[M]", "000000")
df$Wage <- as.integer(df$Wage)


ui <- fluidPage(
  titlePanel("Fußballspieler"),
  tags$head(tags$style(HTML("hr {border-top: 1px solid #BEBEBE;}"))),
  sidebarLayout(position = "left",
                sidebarPanel("Visualizations",
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
      itemsInRange <- df[,2]
    }
    
    
    
    if(input$selectFilter == "nationality"){
      p1 <- ggplot(subset(df, ((Name %in% itemsInRange) & (Nationality %in% input$sliderChooseNationality))), aes(x=Age, y = Wage))  + geom_point(color = "#FF0000") +
        labs(title = "Age-Wage", x = "Age", y = "Wage") + scale_y_continuous(labels = comma)
    }else if(input$selectFilter == "club"){
      p1 <- ggplot(subset(df, ((Name %in% itemsInRange) & (Club %in% input$sliderChooseClub))), aes(x= Age, y = Wage)) + geom_point(color = "#FF0000") +
        labs(title = "Age-Wage", x = "Age", y = "Wage") + scale_y_continuous(labels = comma)
    } else {
      p1 <- ggplot(subset(df, Name %in% itemsInRange), aes(x=Age, y = Wage)) + geom_point(color = "#FF0000") +
        labs(title = "Age-Wage", x = "Age", y = "Wage") + scale_y_continuous(labels = comma)
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
    }else {
      itemsInRange <- df[,2]
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
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
                             
                             #Teil b: Verteilung
                             selectInput("selectVerteilung", "Distribution:",
                                         c("None",
                                                 "Age",
                                                 "Nationality",
                                                 "Overall",
                                                 "Club",
                                                 "Value",
                                                 "Wage",
                                                 "Preferred Foot" = "Preferred.Foot",
                                                 "International Reputation" = "International.Reputation",
                                                 "Weak Foot" = "Weak.Foot",
                                                 "Skill Moves" = "Skill.Moves",
                                                 "Work Rate" = "Work.Rate",
                                                 "Position" = "Position",
                                                 "Jersey Number" = "Jersey.Number"
                                                 #continue...
                                           )),
                             
                             hr(),
                             
                             checkboxInput("checkBoxAgeWage", "Comparison Age-Wage", value = T),
                             checkboxInput("checkBoxLogScaling", "Log10 Scaling", value = F),
                             
                             hr(),
                             
                             checkboxInput("checkBoxAgeOverall", "Comparison Age-Overall", value = F),
                             
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
  compareRowsOrAll <- function () {
    if(input$checkBoxCompareRows) {
      row1 <- as.integer(input$rowSelection1)
      row2 <- as.integer(input$rowSelection2)
      playerName1 <- df[row1,2]
      playerName2 <- df[row2,2]
      print(playerName1)
      print(playerName2)
      return(c(playerName1, playerName2))
    } else {
      return(df[,2])
    }
  }
  
  getSubsetItems <- function () {
    items = compareRowsOrAll()
    if(input$selectFilter == "nationality"){
      subsetItems <- subset(df, ((Name %in% items) & (Nationality %in% input$sliderChooseNationality)))
    }else if(input$selectFilter == "club"){
      subsetItems <- subset(df, ((Name %in% items) & (Club %in% input$sliderChooseClub)))
    } else {
      subsetItems <- subset(df, Name %in% items)
    }
    return(subsetItems)
  }
  
  getGGPlot <- function (title, x, y, aesCustom) {
    p1 <- ggplot(getSubsetItems(), aesCustom) + geom_point(color = "#FF0000") + 
      labs(title = title, x = x, y = y)
    return(p1)
  }
  
  getBarPlot <- function (title, x, y, aesCustom) {
    p1 <- barplot(getSubsetItems(), aesCustom) + geom_point(color = "#FF0000") + 
      labs(title = title, x = x, y = y)
    return(p1)
  }
  
  verteilung <- reactive({
    if (input$selectVerteilung == "None") return (NULL)
    
    p1 = getBarPlot("Distribution", input$selectVerteilung, "Count", aes(x = Age, y = Overall))
    
    p1
  })
  
  wageAgeCompare <- reactive({
    if (!input$checkBoxAgeWage) return (NULL)
   
    p1 = getGGPlot("Age-Wage", "Age", "Wage (€)", aes(x = Age, y = Wage)) + 
      scale_y_continuous(labels = comma)
    
    if(input$checkBoxLogScaling) {
      p1 <- p1 + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                               labels = trans_format("log10", math_format(10^.x))) +
        labs(title = "Age-Wage (log10 scaling)", x = "Age", y = "Wage (€)")
    }
    
    p1
    
  })
  
  overallAgeCompare <- reactive({
    if (!input$checkBoxAgeOverall) return (NULL)
    
    p1 = getGGPlot("Age-Overall", "Age", "Overall", aes(x = Age, y = Overall))
    
    p1
  })
  
  output$plotgraph = renderPlot({
    ptlist <- list(verteilung(), wageAgeCompare(), overallAgeCompare())
    #loesche die Null Plots von der Liste
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    grid.arrange(grobs=ptlist,ncol=length(ptlist))
  })
}

shinyApp(ui = ui, server = server)
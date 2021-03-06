library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gridExtra)
library(scales)
library(stringr)


df <- read.csv(file="Aufgabe-1.csv", sep=",", header=TRUE)

nationalities <- unique(sort(df[,5]))
clubs <- unique(sort(df[,8]))

convertPriceToInteger <- function (priceVar) {
  priceVar <- str_remove_all(priceVar, "[€]")
  priceVar <- str_replace_all(priceVar, "[K]", "000")
  priceVar <- str_replace_all(priceVar, "[M]", "000000")
  priceVar <- as.integer(priceVar)
}

df$Wage <- convertPriceToInteger(df$Wage)
df$Value <- convertPriceToInteger(df$Value)

#contract valid until: convert "23-Dez-19" -> "2019"
df[,21] <- str_replace_all(df[,21], "[0-9]+-[A-Za-z]{3}-([0-9]{2})", "20$1")
df[,21] <- format(as.Date(df[,21], "%Y"), "%Y")

#also for Joined
#df[,19] <- str_replace_all(df[,19], "[0-9]+-[A-Za-z]{3}-([0-9]{2})", "20$1")
#df[,19] <- format(as.Date(df[,19], "%Y"), "%Y")

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
                                           "Jersey Number" = "Jersey.Number",
                                           "Joined",
                                           "Loaned From" = "Loaned.From",
                                           "Contact Valid Until" = "Contract.Valid.Until",
                                           "Height",
                                           "Weight",
                                           "Crossing",
                                           "Finishing",
                                           "Heading Accuracy" = "HeadingAccuracy",
                                           "Short Passing" = "ShortPassing",
                                           "Volleys",
                                           "Dribbling",
                                           "Curve",
                                           "Free Kick Accuracy" = "FKAccuracy",
                                           "Long Passing" = "LongPassing",
                                           "Ball Control" = "BallControl",
                                           "Acceleration",
                                           "Sprint Speed" = "SprintSpeed",
                                           "Agility",
                                           "Reactions",
                                           "Balance",
                                           "Shot Power" = "ShotPower",
                                           "Jumping",
                                           "Stamina",
                                           "Strength",
                                           "Long Shots" = "LongShots",
                                           "Aggression",
                                           "Interceptions",
                                           "Positioning",
                                           "Vision",
                                           "Penalties",
                                           "Composure",
                                           "Marking"
                             ), selected = "Age"),
                             
                             hr(),
                             
                             checkboxInput("checkBoxAgeWage", "Comparison Age-Wage", value = F),
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
  
  getLog10Scaling <- function (p1, title, x, y) {
    if(input$checkBoxLogScaling) {
      p1 <- p1 + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                               labels = trans_format("log10", math_format(10^.x))) +
        labs(title = paste(title,"(log10 scaling)", sep = " "), x = x, y = y)
    }
    return(p1)
  }
  
  getGGPlot <- function (title, x, y, aesCustom, log10Scaling) {
    p1 <- ggplot(getSubsetItems(), aesCustom) + geom_point(color = "#FF0000") + 
      labs(title = title, x = x, y = y) +
      scale_y_continuous(labels = comma)
    if (log10Scaling) {
      p1 = getLog10Scaling(p1, title, x, y)
    }
    return(p1)
  }
  
  getBarPlot <- function (title, x, y, aesCustom) {
    p1 <- ggplot(getSubsetItems(), aesCustom) +
      geom_bar(stat="count", position = "dodge", fill = "#FF0000") +
      labs(title = title, x = x, y = y)
    p1 = getLog10Scaling(p1, title, x, y) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    return(p1)
  }
  
  verteilung <- reactive({
    if (input$selectVerteilung == "None") return (NULL)
    p1 = getBarPlot("Distribution", input$selectVerteilung, "Total", aes(factor(!!as.symbol(input$selectVerteilung))))
    
    p1
  })
  
  wageAgeCompare <- reactive({
    if (!input$checkBoxAgeWage) return (NULL)
   
    p1 = getGGPlot("Age-Wage", "Age", "Wage (€)", aes(x = Age, y = Wage), TRUE)
      
    p1
    
  })
  
  overallAgeCompare <- reactive({
    if (!input$checkBoxAgeOverall) return (NULL)
    
    p1 = getGGPlot("Age-Overall", "Age", "Overall", aes(x = Age, y = Overall), FALSE)
    
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
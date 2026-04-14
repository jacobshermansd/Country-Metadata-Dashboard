library(tidyverse)
library(plotly)
library(maps)
library(DT)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# populationData <- read.csv()
# educationData <- read_csv()

worldData <- ne_countries(scale = "medium", returnclass = "sf")
colnames(worldData)[4] <- "Country"
worldData$Country[grep("Russia", worldData$Country)] <- "Russia"
worldData$Country[grep("United States", worldData$Country)] <- "USA"

unemploymentData <- read_csv("Unemployment.csv",
                             locale=locale(encoding="latin1"),
                             skip = 4)[-c(3, 5)]
colnames(unemploymentData)[1] <- "Country"
unemploymentData$Country[grep("United States", unemploymentData$Country)] <- "USA"
unemploymentData$Country[grep("Russia", unemploymentData$Country)] <= "Russia"

incomeData <- read_csv("Median Income.csv",
                       locale=locale(encoding="latin1"))
colnames(incomeData) <- c("Country", "MedianHousehold", "MedianPerCapita", "Population")
incomeData$Country[grep("United States", incomeData$Country)] <- "USA"

crimeData <- read_csv("Crime.csv",
                      locale=locale(encoding="latin1"),
                      skip = 1)[-c(6, 7)]
colnames(crimeData) <- c("Index", "Country", "Year", "Series", "Value")
crimeData <- crimeData %>% group_by(Country) %>% mutate(MaxYear = max(Year)) %>% select(Country, Series, Value, MaxYear)
crimeData$Country[grep("United States of America", crimeData$Country)] <- "USA"

incarcerationData <- read_csv("Incarceration.csv",
                              locale=locale(encoding="latin1"))
colnames(incarcerationData)[1] <- "Country"
incarcerationData$Country[grep("United States", incarcerationData$Country)] <- "USA"

masterDF <- worldData %>% left_join(incomeData, "Country") %>% 
  left_join(unemploymentData, "Country") %>% 
  left_join(incarcerationData, "Country")

colnames(masterDF)[1:3] <- c("x", "y", "id")


ui <- fluidPage(
   
   titlePanel("Quality of Life Data"),
   
   sidebarLayout(
      sidebarPanel(
         # radioButtons(inputId = "metric", label = "Select a metric",
         #              choices = c(
         #              "income" = "Median household income",
         #              "cost" = "Cost of living",
         #              "wealth" = "Income to expenses ratio",
         #              "unemployment" = "Unemployment rate",
         #              "education" = "Percent of adults with college degree",
         #              "educationCost" = "College tuition",
         #              "life" = "Life expectancy",
         #              "incarceration" = "Incarceration rate",
         #              "reincarceration" = "Rate of repeat criminal offenses",
         #              "healthcareCost" = "Annual cost of healthcare",
         #              "tax" = "Annual taxes paid"))
        radioButtons(inputId = "metric", label = "Select a metric",
                     choiceNames = c(
                       "Population",
                       "Income",
                       "Unemployment",
                       "Incarceration"
                     ),
                     choiceValues = c(
                       "pop",
                       "income",
                       "unemployment",
                       "incarceration")
        ),
        
        conditionalPanel(
          condition = "input.metric == 'income'",
          radioButtons("incomeType", "Income metric",
                       choiceNames = c(
                         "Median household income",
                         "Median per-capita income"
                       ),
                       choiceValues = c(
                         "household",
                         "percap"
                       )
          )
        ),
        
        conditionalPanel(
          condition = "input.metric == 'unemployment'",
          radioButtons("unemploymentGender", "Unemployment by gender",
                       choiceNames = c(
                         "Men",
                         "Women"
                       ),
                       choiceValues = c(
                         "unemploymentMen",
                         "unemploymentWomen"
                       )
          )
        ),
        
        sliderInput("setPop", "Total population (in millions)",
                    min = 0,
                    max = ceiling(max(na.exclude(masterDF$Population)) / 1000000),
                    step = 0.1,
                    value = c(3.0, ceiling(max(na.exclude(masterDF$Population)) / 1000000))
        ), 
        tags$p("This filter slider bar is not yet functional.", style = "color: gray; font-size: 14px;")
      ),
      mainPanel(
        
        plotlyOutput("interactivePlot"),
        
        dataTableOutput("table")
      )
  )
)

server <- function(input, output) {
  
  dataSource <- reactive({
    if(input$metric == "pop") {
      masterDF$Population
    } else if(input$metric == "income" & input$incomeType == "household") {
      masterDF$MedianHousehold
    } else if(input$metric == "income" & input$incomeType == "percap") {
      masterDF$MedianPerCapita
    } else if(input$metric == "unemployment" & input$unemploymentGender == "unemploymentMen") {
      masterDF$Men
    } else if(input$metric == "unemployment" & input$unemploymentGender == "unemploymentWomen") {
      masterDF$Women
    } else if(input$metric == "incarceration") {
      masterDF$prisonersRate
    }
  })
  
  # filteredSource <- reactive({
  #   ifelse(masterDF$Population >= input$setPop[1] & masterDF$Population <= input$setPop[2],
  #          dataSource(), NA)
  # })
  
  title <- reactive({
    if(input$metric == "pop") {
      "Population"
    } else if(input$metric == "income" & input$incomeType == "household") {
      "Median Household Income (US Dollars)"
    } else if(input$metric == "income" & input$incomeType == "percap") {
      "Median Income per-Capita (US Dollars)"
    } else if(input$metric == "unemployment" & input$unemploymentGender == "unemploymentMen") {
      "Unemployment Rate, Men"
    } else if(input$metric == "unemployment" & input$unemploymentGender == "unemploymentWomen") {
      "Unemployment Rate, Women"
    } else if(input$metric == "incarceration") {
      "Incarcerations per 100,000 Populous"
    }
  })
  
  output$interactivePlot <- renderPlotly({
    p <- ggplot(data = masterDF) + 
      geom_sf(aes(fill = dataSource(), group = Country)) +
      labs(title = title(), x = "", y = "", fill = 'Level') +
      scale_fill_continuous(label = comma) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$table <- renderDataTable({
    Table <- na.exclude(data.frame(Country = unique(masterDF$Country),
               Measure = dataSource()[which(!duplicated(masterDF$Country))]))
    Table <- Table[order(Table[, 2], decreasing = TRUE), ]
    colnames(Table) <- c("Country", "Measure")
    Table$Measure <- formatC(Table$Measure, format = "f", big.mark = ",", digits = 0)
    Rank <- c(1:nrow(Table))
    data.frame(Rank, Table)
  })
}

shinyApp(ui = ui, server = server)
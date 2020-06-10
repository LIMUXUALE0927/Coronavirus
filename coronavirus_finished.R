#------------------------------------------------------------------------------------------- Packages

library(jsonlite)
library(httr)
library(rvest)
library(DT)
library(dplyr)
#library(tidycovid19)
library(shiny)
library(tidyverse)
library("RCurl") # for getURLContent function
library("png") # display png image
library(plotly)
library(ggplot2)

#------------------------------------------------------------------------------------------- Coronavirus Data

get_json = function(country){
  r <- RETRY("GET", paste0("https://api.covid19api.com/total/country/", country), pause_min = 1, times = 20)
  json <- content(r, as = "text", encoding = "UTF-8")
  return(fromJSON(json))
}

country_list <-  c("china", "united-states", "canada", "united-kingdom", "brazil", "italy", "russia")
country_code_list <- c("CN", "US", "CA", "UK", "BR", "IT", "RU")
json_list <- lapply(country_list, get_json)

coronavirus_data <- do.call("rbind", json_list)
coronavirus_data$Country_Code <- rep(country_code_list, sapply(json_list, nrow))
coronavirus_data$Country <- as.factor(coronavirus_data$Country)
coronavirus_data$Country_Code <- as.factor(coronavirus_data$Country_Code)
coronavirus_data$Record_Date <- str_extract(coronavirus_data$Date, "(\\d{4}-\\d{2}-\\d{2})")
coronavirus_data$Record_Date <- as.Date(coronavirus_data$Record_Date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
coronavirus_data <- coronavirus_data[,c("Country","Confirmed","Deaths","Recovered","Active","Country_Code","Record_Date")]

# all countries summary
r1 <- RETRY("GET", "https://api.covid19api.com/summary", pause_min = 1, times = 20)
json <- content(r1, as = "text", encoding = "UTF-8")
df_summary <- fromJSON(json)

#------------------------------------------------------------------------------------------- UI

ui <- fluidPage(
  title = "COVID-19 pandemic",
  tabsetPanel(
    # Tab1
    tabPanel(
      title = "Coronavirus pandemic",
      plotOutput("coronavirus_img", width = "800px"),
      textOutput("story")
    ),
    tabPanel(
      title = "Global",
      mainPanel(plotlyOutput("global_treemap", height = "600px", width = "600px"))
    ),
    # Tab2
    tabPanel(
      title = "Country Data",
      DT::dataTableOutput("DT_summary")
    ),
    # Tab3
    tabPanel(
        title = "Time Series Plot",
        sidebarPanel(
          htmlOutput("countries_selector")
        ),
        mainPanel(
          plotOutput("timeSeriesPlot")#, height = "500px")
        )
      )
    )
)

#------------------------------------------------------------------------------------------- Server

server <- function(input, output, session) {
  
  output$story <- renderText({paste0("The COVID-19 pandemic, also known as the coronavirus pandemic, is an ongoing pandemic",
  "of coronavirus disease 2019 (COVID‑19), caused by severe acute respiratory syndrome coronavirus 2 (SARS‑CoV‑2).",
  " The outbreak was first identified in Wuhan, China, in December 2019. ",
  "The World Health Organization declared the outbreak a Public Health Emergency of International Concern on 30 January,",
  "and a pandemic on 11 March. As of 10 June 2020, more than 7.18 million cases of COVID-19 have been reported in more than",
  "188 countries and territories, resulting in more than 408,000 deaths; more than 3.35 million people have recovered.",
  "The virus is primarily spread between people during close contact, most often via small droplets produced by coughing,",
  "sneezing, and talking. The droplets usually fall to the ground or onto surfaces rather than travelling through air ",
  "over long distances. Less commonly, people may become infected by touching a contaminated surface and then touching their face. ",
  "It is most contagious during the first three days after the onset of symptoms, although spread is possible before symptoms appear, ",
  "and from people who do not show symptoms. Common symptoms include fever, cough, fatigue, shortness of breath, and loss of sense of smell.",
  "Complications may include pneumonia and acute respiratory distress syndrome. The time from exposure to onset of symptoms is typically around ",
  "five days but may range from two to fourteen days. There is no known vaccine or specific antiviral treatment. Primary treatment is symptomatic ",
  "and supportive therapy.")})

  # Tab1
  output$DT_summary <- DT::renderDataTable({
    countries_df <- df_summary$Countries[,c("Country", "NewConfirmed", "TotalConfirmed", "NewDeaths", "TotalDeaths", "NewRecovered", "TotalRecovered")]
    names(countries_df) <- c("Country", "New Confirmed", "Total Confirmed", "New Deaths", "Total Deaths", "New Recovered", "Total Recovered")
    countries_df[order(countries_df[,"Total Confirmed"], decreasing = TRUE),]
  })
  
  # Tab2
  output$coronavirus_img <- renderPlot({
    x <- "https://www.minnpost.com/wp-content/uploads/2020/03/coronavirusCDC640.png?w=640&strip=all"
    img <- readPNG(getURLContent(x))
    plot(1:10,
         ty = "n",
         axes = FALSE,
         ann = FALSE)
    rasterImage(img, 0, 0, 10, 10)
  })
  # plotly
  conf_df <- df_summary$Countries %>% arrange(-TotalConfirmed)
  conf_df$parents = "Confirmed"
  output$global_treemap <- renderPlotly({
    plot_ly(data = conf_df,
            type= "treemap",
            values = ~TotalConfirmed,
            labels= ~ Country,
            parents=  ~parents,
            domain = list(column=0),
            name = "Confirmed",
            textinfo="label+value+percent parent")
  })
  # Tab3
  output$countries_selector = renderUI({
    selectInput(inputId = "selected_Variables", 
                label = "Variable:",
                choices = c("Confirmed", "Deaths", "Recovered"),
                selected = "Confirmed") 
  })
  output$timeSeriesPlot <- renderPlot({
    if (!is.null(input$selected_Variables)){
    if (input$selected_Variables == "Confirmed") {
      ggplot(coronavirus_data,
             aes(x = Record_Date, y = Confirmed, col = Country)) + geom_line() + xlab("") +
        xlab("Time") + ylab("counts") +
        ggtitle(paste("Number of confirmed cases"))
    } else if (input$selected_Variables == "Deaths") {
      ggplot(coronavirus_data, aes(x = Record_Date, y = Deaths, col = Country)) + geom_line() + xlab("") +
        xlab("Time") + ylab("counts") +
        ggtitle(paste("Number of death cases"))
    } else if (input$selected_Variables == "Recovered") {
      ggplot(coronavirus_data,
             aes(x = Record_Date, y = Recovered, col = Country)) + geom_line() + xlab("") +
        xlab("Time") + ylab("counts") +
        ggtitle(paste("Number of recovered cases"))      
    }
    }
  })
}

shinyApp(server = server, ui = ui)

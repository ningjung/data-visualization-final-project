library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(datasets)
library(metricsgraphics)
library(dplyr)
library(plotly)
library(data.table)
library(shinydashboard)
library(radarchart)
library(DT)


# Tiffany's Data
cwur_data <- read.csv("https://raw.githubusercontent.com/yeukyul/datasets/master/joint_recode.csv", stringsAsFactors=FALSE)
cwur_data$num_students <- gsub(",", "", cwur_data$num_students)
cwur_data$num_students <- as.numeric(cwur_data$num_students)

selectedID <- 1234

# Isabelle's Data
univ_data <- read.csv("https://raw.githubusercontent.com/yeukyul/datasets/master/cwurData.csv")
univ_data$continent <- ifelse(is.element(univ_data$country, c("Canada",  "Puerto Rico",  "USA")), "North America",
                              ifelse(is.element(univ_data$country, c("Argentina", "Brazil", "Chile", 
                                                                     "Colombia", "Mexico",  "Portugal", "Uruguay")), "South America",
                                     ifelse(is.element(univ_data$country, c("China", "Hong Kong", "India", "Iran", "Israel", 
                                                                            "Japan",  "Lebanon", "Malaysia", "Russia", 
                                                                            "Saudi Arabia", "Singapore", "South Korea", "Taiwan", 
                                                                            "Thailand", "Turkey", "United Arab Emirates")), "Asia",
                                            ifelse(is.element(univ_data$country, c("Austria",  "Belgium",  "Bulgaria", "Croatia", 
                                                                                   "Cyprus", "Czech Republic",  "Denmark", "Estonia", 
                                                                                   "Finland", "France", "Germany", "Greece", "Hungary", 
                                                                                   "Iceland",  "Ireland", "Italy", "Lithuania", 
                                                                                   "Netherlands", "Norway", "Poland",  "Romania", 
                                                                                   "Serbia",  "Slovak Republic", "Slovenia", "Spain",  
                                                                                   "Sweden", "Switzerland", "United Kingdom")), "Europe",
                                                   ifelse(is.element(univ_data$country, c("Australia", "New Zealand")), "Australia", "Africa")))))


# Victor's Data
uni.rank.data <- fread("https://raw.githubusercontent.com/yeukyul/datasets/master/timesData.csv")
precent.f <- list()
for(ratio in uni.rank.data$female_male_ratio){
   if (is.character(ratio) & ratio!=""){
      test <- strsplit(ratio, split = ":")
      test <- unlist(test)
      dig.1 <- as.numeric(test[1])
      print(dig.1)
      precent.f <- c(precent.f, dig.1)
   }
   else precent.f <- c(precent.f, NA)
}
precent.f <- unlist(precent.f)

uni.rank.data$precent.f<- precent.f

uni.rank.data$world_rank <- as.numeric(uni.rank.data$world_rank)

international.precent <- list()
for (str in uni.rank.data$international_students){
   if (str!=""){
      str <- substr(str,0, nchar(str)-1)
      international.precent <- c(international.precent,as.numeric(str))
   }
   else international.precent <- c(international.precent,NA)
}
international.precent <- unlist(international.precent)

uni.rank.data$international_precent <- international.precent

# Serene's Data
scores <- read.csv("https://raw.githubusercontent.com/yeukyul/datasets/master/cwurData.csv")
ranking.joint <- read.csv("https://raw.githubusercontent.com/yeukyul/datasets/master/joint_college.csv", stringsAsFactors=FALSE)


# GGPlot theme
yeukyul_315_theme <- theme(
   plot.title = element_text(margin = margin(0, 0, 10, 0),
                             hjust = 0.5),
   legend.key = element_rect(fill = "white"),
   legend.background = element_rect(fill = "white"),
   panel.grid.major = element_line(colour = "white"),
   panel.grid.minor = element_blank(),
   axis.text.x = element_text(size = rel(0.7)),
   axis.text.y = element_text(size = rel(0.7)),
   axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
   axis.title.x = element_text(margin = margin(10, 0, 0, 0))
)


sidebar <- dashboardSidebar(
   sidebarMenu(
      menuItem("College Maps", tabName = "map", icon = icon("map-o")),
      menuItem("College Profile", tabName = "college", icon = icon("graduation-cap")),
      menuItem("Geographical Profile", tabName = "country", icon = icon("globe")),
      menuItem("Global Trend", tabName = "other", icon = icon("line-chart")),
      menuItem("Dataset", tabName = "datasets", icon = icon("table"))
   )
)


ui <- dashboardPage(
   skin = "black",
   dashboardHeader(title = "Group 13"),
   sidebar,
   dashboardBody(
      tags$head(
         # Include our custom CSS
         includeCSS("www/styles.css"),
         includeScript("www/gomap.js")
      ),
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      tabItems(
         
         tabItem(tabName = "college",
                 h2("University Profile"), tags$br(),
                 fluidRow(
                    
                    box(
                       h4("University Basic Stats"), 
                       helpText("Select a university to view basic statistics."), tags$br(),
                       selectInput(inputId = 'uni', label = 'Select University', 
                                   choices = c(as.character(cwur_data$institution)), 
                                   selected = "Carnegie Mellon University"),
                       tabsetPanel(
                              tabPanel("Ranking",
                                 plotlyOutput(outputId = "rankingChange", height = "400px", width = "100%")
                              ),

                              tabPanel("Research / Citation",
                                       plotlyOutput(outputId = "research_citation", height = "400px", width = "100%"),
                                       helpText("Citation score is calculated by measuring number of citations of given university.
                                             The data are normalised to reflect variations in citation volume between different 
                                          subject areas. This means that institutions with high levels of research activity 
                                          in subjects with traditionally high citation counts do not gain an unfair advantage. 
                                          Times(the source of data set) have blended equal measures of a country-adjusted and 
                                          non-country-adjusted raw measure of citations scores.")
                              )
                       )
                    ),
                    box(
                       h4("Universities Comparison"), 
                       helpText("Select two universities to view score comparisons."), tags$br(),
                       selectInput(inputId = 'uni1', label = 'Select University 1', 
                                   choices = c(as.character(scores$institution)), 
                                   selected = 1),
                       selectInput(inputId = 'uni2', label = 'Select University 2', 
                                   choices = c(as.character(scores$institution)), 
                                   selected = 1),
                       chartJSRadarOutput("radar", width = "450", height = "300"),
                       helpText("How these scores are obtained:"),
                       helpText("Education: measured by the number of a university's alumni who have won major international awards, 
                        prizes, and medals relative to the university's size"),
                       helpText("Patent: measured by the number of international patent filings"),
                       helpText("Citations: measured by the number of highly-cited research papers"),
                       helpText("Influence: measured by the number of research papers appearing in highly-influential journals"),
                       helpText("Publications: measured by the number of research papers appearing in reputable journals"),
                       helpText("Faculty: measured by the number of academics who have won major international awards, prizes, and medals"),
                       helpText("Alumni: measured by the number of a university's alumni who have held CEO positions at the world's top companies relative to the university's size")
                    )
                 )
            ),
         tabItem(tabName = "country", 
                 
                 h2("Geographical Profile"), tags$br(),
                           fluidRow(
                              box(
                                 h4("Top Universities in a Given Country by Year"),
                                 helpText("Select a country to view basic statistics of top universities."),
                                 tags$br(),
                                 selectInput(inputId = 'country', label = 'Select Country', 
                                             choices = c(as.character(cwur_data$country)), 
                                             selected = "United States of America"),
                                 fluidRow(
                                    column(6, 
                                       numericInput("limit", 
                                                label = "Top nth universities", 
                                                value = 10)),
                                    column(6, 
                                           selectInput(inputId = "countryYear", label = 'Select Year',
                                                          choices = c(sort(unique(cwur_data$year), decreasing = TRUE)),
                                                                      selected = 2016)
                                          )
                                 ),
                                 fluidRow(
                                    column(
                                       DT::dataTableOutput("countryTable"), width = 12
                                    )
                              )),
                              
                                 box(
                                    h4("Mean College Score by Continent"),
                                    selectInput(inputId = "continent",
                                                label = "Continent",
                                                choices = c("North America", "South America", "Asia", "Europe", 
                                                            "Australia", "Africa"),
                                                selected = "North America"),
                                    
                                    plotlyOutput(outputId = "bar_plot", height = "300px", width = "100%"),
                                    helpText("To calculate average score for each country, we found the average total score of all of the universities in the country.
The total score was the variable that was used to calculate the ranks of the universities, computed as an agregated sum of quality of education, researches, and citations.")
                                 )
                           ),
                 fluidRow(
                    box(
                     h4("Relationship of Interantional Students and Rankng by Contry"),
                     helpText("Select a country to view relationship"),
                     column(12,
                     checkboxInput(inputId = "show_points",
                                  label = "Show Points",
                                  value = FALSE), 
                     selectInput(inputId = "count_select",
                                label = "Select Country",
                                choices = c("All", unique(uni.rank.data$country)),
                                selected = "All"),
                     plotOutput(outputId = "plotb2", height = "500px")
                     )
                    )
                 )
            ),
         
         tabItem(tabName = "map", 
                 # Have the map span the whole page
                 leafletOutput("map", width = "100%", height = "100%"),
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = 80, left = "auto", right = 30, bottom = "auto",
                               width = 330, height = "auto",
                               
                               h1("College Explorer"),
                               tags$br(),
                               tags$p("Click on a point on map to view details.", style = "align:center;"),
                               sliderInput("rankings", "Rankings", min = min(cwur_data$rank_lo, na.rm = TRUE),
                                           max = max(cwur_data$rank_hi, na.rm = TRUE),
                                           value = c(1, 800), step = 20
                               ),
                               selectInput("year", "Year",
                                           sort(as.numeric(unique(cwur_data$year)))
                               ),
                               checkboxInput("size", "Points adjust to size of student body", FALSE),
                               plotOutput(outputId = "international", height = "200px"),
                               plotOutput(outputId = "income", height = "200px")
                 )
         ),
         tabItem(tabName = "datasets",
                 h2('College World Ranking Raw Data'), tags$br(),
                 dataTableOutput('collegetable')
         ),
         
         tabItem(tabName = "other", 
                 h2("Global Trends"),
                 helpText("Here are some global unversity trends that we noticed."),
                 fluidRow(
                                    box(
                                         h4("Percentage of Female Students vs Ranking"),
                                         helpText("Select upperbound of ranking to view trend."),
                                      selectInput(inputId = "upper_rank",
                                                  label = "Upper Rank Bound",
                                                  choices = c(10, 25, 50, 100, 150, 200),
                                                  selected = 200),
                                      checkboxInput(inputId = "show_trend", 
                                                    label = "Show Trendline", 
                                                    value = FALSE), 
                                      checkboxInput(inputId = "show_lm", 
                                                    label = "Show Linear Trend", 
                                                    value = FALSE), 
                                      plotOutput(outputId = "plotb1", height = "500px")
                                      )
                                     ,
                             
                                    box(
                                       h4("Ranking vs. Influence"),
                                       helpText("Select year to view trend"),
                                       selectInput(inputId = "influence",
                                                   label = "Data Year",
                                                   choices = c(2012, 2013, 2014, 2015),
                                                   selected = 2012),
                                       
                                       metricsgraphicsOutput(outputId = "main_plot", width = "100%")))
                 )
         )
         
      )
   )


shinyUI(ui)
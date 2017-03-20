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

# Tiffany's Reading in data
cwur_data <- read.csv("https://raw.githubusercontent.com/yeukyul/datasets/master/joint_recode.csv", stringsAsFactors=FALSE)
cwur_data$num_students <- gsub(",", "", cwur_data$num_students)
cwur_data$num_students <- as.numeric(cwur_data$num_students)
cwur_data$income <- as.numeric(cwur_data$income)

display_data <- cwur_data[,c("college_year", "world_rank", "country", "teaching", "international_students", 
                            "research", "citations...", "income", "total_score", "num_students", "student_staff_ratio", 
                            "female_male_ratio")]

selectedID <- 1234
isPlotting <- FALSE

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

server <- function(input, output, session) {
   
   # Reactive expression for the data subsetted to what the user selected
   filteredData <- reactive({
      cwur_data[cwur_data$rank_lo >= input$rankings[1] & cwur_data$rank_hi <= input$rankings[2] &
                   cwur_data$year == input$year,]
   })
   
   # College Map: college's geographical location
   output$map <- renderLeaflet({
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      leaflet(cwur_data) %>% addTiles() %>%
         fitBounds(~min(Lon, na.rm = TRUE), ~min(Lat, na.rm = TRUE), 
                   ~max(Lon, na.rm = TRUE), ~max(Lat, na.rm = TRUE))
   })

   plot_international = function(id){
      output$international <- renderPlot({
         selectedCollege <- cwur_data[which(cwur_data$ID == id),]
         df <- cwur_data[which(cwur_data$institution == selectedCollege$institution),]
         df <- df[order(df$year), ]
         p <- ggplot(data = df, aes(x = year, y = rank_lo)) + 
            geom_point(color = "#4B90CC") + geom_line(color = "#4B90CC") +
            labs(x = "year", y = "world ranking") +
            ggtitle("Time series of world ranking") +
            scale_y_continuous(trans = "reverse", breaks = unique(df$rank_lo)) +
            yeukyul_315_theme
         print(p)
      })
   }
   
   
   plot_income = function(id){
      output$income <- renderPlot({
         selectedCollege <- cwur_data[which(cwur_data$ID == id),]
         df <- cwur_data[which(cwur_data$institution == selectedCollege$institution),]
         df <- df[order(df$year), ]
         p <- ggplot(data = df, aes(x = year, y = income)) + 
            geom_point(color = "#4B90CC") + geom_line(color = "#4B90CC") +
            labs(x = "year", y = "university income score") +
            ggtitle("Times series of income score") +
            yeukyul_315_theme
         print(p)
      })
   }
   
   
   # Geographical Profile: shows average score for each continent
   output$bar_plot <- renderPlotly({
      cont <- input$continent
      univ_data_sub <- filter(univ_data, continent == cont)
      avg_score <- tapply(univ_data_sub$score, mean, INDEX = univ_data_sub$country)
      avg_score <- avg_score[!is.na(avg_score)]
      avg_score <- data.frame(avg_score)
      avg_score <- setDT(avg_score, keep.rownames = TRUE)[]
      colnames(avg_score) <- c("country", "avg")
      p1 <- ggplot(data = avg_score, aes(x = country, y = avg)) +
         geom_bar(stat = "identity", fill = "#91AA9D") + 
         theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
         labs(x = "Countries", y = "Average University Score") +
         ggtitle("Average University Score per Country by Continent")
      ggplotly(p1)
   })
   
   # Geographical Profile: top universities for each country
   output$countryTable <- DT::renderDataTable(DT::datatable({
      data <- cwur_data
      data <- data[data$country == input$country & data$year == input$countryYear,]
      data <- data[order(as.integer(data$rank_lo)),c("institution", "country", "world_rank", "total_score")]
      names(data) <- c("Institution", "Country", "World Ranking", "University Score")
      head(data, input$limit)
   }))
   
   # Dataset: show table of dataset
   output$collegetable = renderDataTable({
      display_data
   })
   
   output$plotb1 <- renderPlot({
      scat <- ggplot(data = uni.rank.data[uni.rank.data$world_rank<=as.numeric(input$upper_rank)], 
                     aes(x=precent.f, y=world_rank)) +
         geom_point()+
         xlab("Percent of Female Students") + ylab("World Rank") + 
         ggtitle("Impact of Female Students on Rank")
      print(scat)
      
      if (input$show_trend) {
         smooth <- geom_smooth(aes(x = precent.f, y = world_rank), se = FALSE)
         scat <- scat + smooth
         print(scat)
      }
      if (input$show_lm) {
         lm <- geom_smooth(aes(x = precent.f, y = world_rank), se = FALSE, method = lm, col="red")
         scat <- scat + lm
         print(scat)
      }
   })
   
   
   # College profile: radar chart for two colleges comparisons
   output$radar <- renderChartJSRadar({
      ranking.2015 <- subset(scores, scores$year == 2015)
      total <- length(unique(ranking.2015$institution))
      
      ind1 <- which(ranking.2015$institution == input$uni1)
      ind2 <- which(ranking.2015$institution == input$uni2)
      
      scores <- data.frame("Label"=c("Education", "Almuni", "Faculty", "Publications",  "Influence", "Citations", "Patent"),
                           University1 = c((total-ranking.2015[ind1,]$quality_of_education)/100, (total-ranking.2015[ind1,]$alumni_employment)/100, 
                                           (total-ranking.2015[ind1,]$quality_of_faculty)/100, (total-ranking.2015[ind1,]$publications)/100, 
                                           (total-ranking.2015[ind1,]$influence)/100, (total-ranking.2015[ind1,]$citations)/100, 
                                           (total-ranking.2015[ind1,]$patents)/100),
                           University2 = c((total-ranking.2015[ind2,]$quality_of_education)/100, (total-ranking.2015[ind2,]$alumni_employment)/100, 
                                           (total-ranking.2015[ind2,]$quality_of_faculty)/100, (total-ranking.2015[ind2,]$publications)/100, 
                                           (total-ranking.2015[ind2,]$influence)/100, (total-ranking.2015[ind2,]$citations)/100, 
                                           (total-ranking.2015[ind2,]$patents)/100)
      )
      
      chartJSRadar(scores = scores, maxScale = 10, showToolTipLabel=TRUE)
   })
   
   # College Profile: Time series for college ranking
   output$rankingChange <- renderPlotly({
      ranking.one <- cwur_data[which(ranking.joint$institution == input$uni), c("year", "institution", "rank_lo")]
      if (nrow(ranking.one) <= 1) {
         plot.t = "Not enough data to show ranking change"
      } else {
         plot.t = "Change of World Ranking"
      }
      x <- list(title = "Year")
      y <- list(title = "World Rank", autorange = "reversed")
      p <- plot_ly(ranking.one, x = ~as.integer(year), y = ~as.integer(rank_lo), name = 'World Rank', type = 'scatter', mode = 'lines') %>%
         layout(xaxis = x, yaxis = y, title = plot.t)
      ggplotly(p)
   })
   
   # College Profile: Time series for student body
   output$numberOfStudent <- renderPlotly({
      df <- cwur_data[which(cwur_data$institution == input$uni), c("year", "institution", "international_students")]
      if (nrow(df) <= 1) {
         plot.t = "Not enough data to show student body change"
      } else {
         plot.t = "Change of Number of Students"
      }
      x <- list(title = "Year")
      y <- list(title = "Percentage of International Students")
      p <- plot_ly(df, x = ~as.integer(year), y = ~as.integer(international_students), name = 'Percentage of International Students', type = 'scatter', mode = 'lines') %>%
         layout(xaxis = x, yaxis = y, title = plot.t)
      ggplotly(p)
   })
   
   # College Profile: Time series for research / citation
   output$research_citation <- renderPlotly({
      df <- cwur_data[which(ranking.joint$institution == input$uni), c("year", "institution", "citations...")]
      if (nrow(df) <= 1) {
         plot.t = "Not enough data to show citation change"
      } else {
         plot.t = "Change of Citation Score"
      }
      x <- list(title = "Year")
      y <- list(title = "Citations Score")
      p <- plot_ly(df, x = ~as.integer(year), y = ~as.integer(citations...), name = 'Number of Students', type = 'scatter', mode = 'lines') %>%
         layout(xaxis = x, yaxis = y, title = plot.t)
      ggplotly(p)
   })
   
   # Others: show rank versus influence
   output$main_plot <- renderMetricsgraphics({
      dat_year <- input$influence
      univ_data_sub <- filter(univ_data, year == dat_year)
      p %>%
         mjs_plot(univ_data_sub, x=national_rank, y=influence, width=600, height = 800) %>%
         mjs_point() %>%
         mjs_labs(x="National Rank", y="Influence Rank")
      
   })
   
   # Others: international percent vs world rank
   output$plotb2 <- renderPlot({
      if (input$count_select == "All"){
         de <- ggplot(data = uni.rank.data) + 
            stat_density2d(aes(x = international_precent, y = world_rank,
                               fill = ..density..), geom = "tile", contour = F) +
            scale_fill_gradient(low = "white", high = "blue") + 
            ggtitle("Heat Map of Percent of International Students Attending a University \n and its World Rank") + 
            labs(x = "Percentage of International Students", y = "World Rank", 
                 fill = "Density") +
            yeukyul_315_theme
      }
      else {
         de <- ggplot(data = uni.rank.data[uni.rank.data$country==input$count_select]) + 
            stat_density2d(aes(x = international_precent, y = world_rank,
                               fill = ..density..), geom = "tile", contour = F) +
            scale_fill_gradient(low = "white", high = "blue") + 
            ggtitle("Heat Map of Percent of International Students Attending a University \n and its World Rank") + 
            labs(x = "Percentage of International Students", y = "World Rank", 
                 fill = "Density") +
            yeukyul_315_theme
      }
      print(de)
      
      
      if (input$show_points){
         points <-  geom_point(aes(x = international_precent, y = world_rank))
         de <- de + points
         print(de)
      }
      
   })
   
   ##### Event Listeners #####
   
   # Incremental changes to the map (in this case, replacing the
   # circles when a new color is chosen) should be performed in
   # an observer. Each independent set of things that can change
   # should be managed in its own observer.
   observe({
      if (input$size){
         leafletProxy("map", data = filteredData()) %>%
            clearShapes() %>%
            addCircles(lng = ~Lon, lat = ~Lat, fillColor = "#db4a4a", color = "#db4a4a", 
                       radius = ~sqrt(num_students)*500, weight = 1, layerId = ~ID)
      } else {
         leafletProxy("map", data = filteredData()) %>%
            clearShapes() %>%
            addCircles(lng = ~Lon, lat = ~Lat, fillColor = "#4b90cc", color = "#4b90cc", 
                       radius = 20000, weight = 1, layerId = ~ID)
      }
      
   })
   
   
   # Show a popup at the given location
   showCollegePopup <- function(lat, lon, id) {
      isPlotting = TRUE
      selectedCollege <- cwur_data[which(cwur_data$ID == id),]
      
      content <- as.character(tagList(
         tags$h4(selectedCollege$institution),
         sprintf("Country: %s", selectedCollege$country), tags$br(),
         sprintf("World Ranking: %s", selectedCollege$world_rank), tags$br(),
         sprintf("Number of Students: %s", as.integer(selectedCollege$num_students))
      ))
      leafletProxy("map") %>% addPopups(lon, lat, content, layerId = id)
   }
   
   # When map is clicked, show a popup with city info
   observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      
      if (is.null(event))
         return()
      
      plot_income(event$id)
      plot_international(event$id)
      isolate({
         showCollegePopup(event$lat, event$lng, event$id)
      })
   })
}

shinyServer(server)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load the necessary libraries
library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)

# Load the data
weather_data  <- read_csv("city_temperature.csv")
# weather_data$Date <- as.Date(weather_data$Date, "%m/%d/%Y")


# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        /* Customize the tabs */
        .nav-tabs > li > a {
        border: none;
        background-color: #f0f0f0;
        color: #333;
        font-weight: bold;
        }
        
        .nav-tabs > li > a:hover {
        background-color: #ffaa80;
        }
        
         /* Customize the active tab */
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:hover, 
      .nav-tabs > li.active > a:focus {
        background-color: #cc6600;
        border: none;
        border-bottom-color: transparent;
        color: #ffff;
        cursor: default;
      }
      
        .header{
          text-align: center;
          border-radius: 5px;
          font-weight: 750;
          color: #ffff;
          padding: 30px;
          background-image: linear-gradient(90deg,#cc0000,#cc9900);
          margin-bottom: 3%;
        }
        .panelheader{
        color : #800000;
        padding: 20px 0px
        }
        #city_table{
        display: flex;
        justify-content: center;
        }"
      )
    )
  ),
  # Application title
  
  # titlePanel(div("Hottest City in a Country",class="header")),
  
  div(
    titlePanel("Hottest City in a Country"),
    class = "header"
  ),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input for selecting the country
      selectInput("country", "Select Country:",
                  choices = unique(weather_data$Country)),
      
      # Input for selecting the year
      sliderInput("year", "Select Year:",
                  min = min(weather_data$Year),
                  max = max(weather_data$Year),
                  value = max(weather_data$Year),
                  step = 1),
      
      # Text below inputs
      helpText("For better visualization,choose a country that has more than one city."),
      p("Please note that in the first tab of the table, you can sort the cities column and identify the countries that have more than one city.")
    ),
    
    # Main panel for outputs
    mainPanel(
      
      # Tabs
      tabsetPanel(
        
        # First tab: Country wise count of cities 
        tabPanel("Country wise count of cities",
                 tags$div(p("This tab displays the total number of cities for each country, regardless of the selection made in the side panel.",class="panelheader")),
                 # Render the table in pages
                 DTOutput("city_count_table")
        ),
        
        # Second tab: Number of cities in each country
        tabPanel("Count of cities in the chosen country",
                 tags$div(p("This tab displays the number of cities in the country that has been selected.",class="panelheader")),
                 # Render the table
                 tableOutput("city_table")
        ),
        
        # Third tab: Hottest city in the country
        tabPanel("Hottest City",
                 tags$div(p("This tab displays a comparison of temperatures across all cities within the selected country.",class="panelheader")),
                 # Render the plot
                 plotOutput("city_plot")
        )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  # Find the min and max years for each country
  country_years <- weather_data %>%
    group_by(Country) %>%
    summarise(min_year = min(Year), max_year = max(Year)) %>%
    filter(min_year == max_year)
  
  # Filter the data based on user input and the countries with min and max years same
  filtered_data <- reactive({
    weather_data %>%
      filter(Country == input$country,
             Year == input$year) %>%
      filter(Country %in% country_years$Country) %>%
      select(Country, City, AvgTemperature)
  })
  # Filter the data based on user input
  filtered_data <- reactive({
    weather_data %>%
      filter(Country == input$country,
             Year == input$year) %>%
      select(Country, City, AvgTemperature)
  })
  
  # Second tab shows the data for the selected country irrespective of selected year
  filt_data <- reactive({
    weather_data %>%
      filter(Country == input$country) %>%
      select(Country, City, AvgTemperature)
  })
  
  # Calculate number of cities in each country
  city_counts <- reactive({
    filt_data() %>%
      group_by(Country) %>%
      summarise(Cities = n_distinct(City))
  })
  
  # Render the table of city counts
  output$city_table <- renderTable({
    city_counts()
  })
  
  
  
  # Render the table of city counts
  output$city_count_table <- renderDT({
    city_count <-  weather_data %>% group_by(Country) %>% summarize(Cities = n_distinct(City))
    datatable(city_count, 
              options = list(lengthMenu = c(5, 10, 15), pageLength = 5),
              rownames = FALSE,
              colnames = c("Country", "Cities"),
              class = 'cell-border stripe') %>% 
      formatStyle("Cities", fontWeight = 'bold', backgroundColor = 'lightblue')
  })
  
  
  # Build the plot of the hottest city
  hottest_city_plot <- reactive({
    summarised_data <- filtered_data() %>%
      group_by(City) %>%
      summarise(Temperature = max(AvgTemperature))
    
    hottest_city <- summarised_data$City[which.max(summarised_data$Temperature)]
    
    if (nrow(summarised_data) == 0) {
      ggplot() +
        ggtitle("Sorry there is no data available \n for the selected year \n Please select some other year :)")+
        theme(plot.title = element_text(size = 20, color = "#cc6600", face = "bold", hjust = 0.5))
    } else {
      ggplot(summarised_data, aes(x = City, y = Temperature, fill = City)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = paste("Hottest City in", input$country, input$year, "is", hottest_city),
             x = "City", y = "Average Temperature") +
        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "#cc6600"),
              axis.line = element_line(colour = "black"),
              axis.title= element_text(size = 16,color="#cc6600",face="bold"),
              axis.text = element_text(size = 14),
              legend.title = element_text(size = 14, colour = "#cc6600", face = "bold"),
              legend.text = element_text(size = 12, face = "bold"))
    } 
  })
  
  
  # Render the plot of the hottest city
  output$city_plot <- renderPlot({
    hottest_city_plot()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

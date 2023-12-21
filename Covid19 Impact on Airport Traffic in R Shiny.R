## Author: Yini Rong

setwd("...") # Set your directory

library(readxl)
library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(plotly)

## Read in data
data <- read.csv("covid_impact_on_airport_traffic.csv")
data$city <- paste0(data$City, ", ", data$ISO_3166_2)

# Keep relevant columns
data2 <- data %>% 
  select(c("Date", "AirportName", "city", "Centroid", "PercentOfBaseline")) %>% 
  mutate(Date = as.Date(Date))

# Extract latitude and longitude from Point column
coords <- do.call(rbind, strsplit(gsub("POINT\\(|\\)", "", data2$Centroid), " "))
data2$longitude <- as.numeric(coords[, 1])
data2$latitude <- as.numeric(coords[, 2])


## Pull in COVID cases and deaths
# Confirmed cases and deaths are aggregated across counties adjacent to the county in which the airport locates
counties <- read_excel("counties by city.xlsx")
counties$County_Name <- ifelse(grepl("City", counties$County), counties$County, paste(counties$County, "County"))
counties$State <- substr(counties$State, nchar(counties$State) - 1, nchar(counties$State))

# Confirmed cases
covid_confirmed <- read.csv("covid_confirmed_usafacts.csv")
covid_confirmed$County.Name <- trimws(covid_confirmed$County.Name)
covid_confirmed_clean <- inner_join(covid_confirmed, counties, by = c("County.Name" = "County_Name", "State" = "State")) %>% 
  group_by(City, State) %>% 
  summarise(across(starts_with("X"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_longer(cols = starts_with("X"), names_to = "Date", values_to = "Confirmed") %>% 
  mutate(Date = as.Date(substr(Date, 2, nchar(Date)), format = "%Y.%m.%d"),
         city = paste0(City, ", US-", State))

# Deaths
covid_deaths <- read.csv("covid_deaths_usafacts.csv")
covid_deaths$County.Name <- trimws(covid_deaths$County.Name)
covid_deaths_clean <- inner_join(covid_deaths, counties, by = c("County.Name" = "County_Name", "State" = "State")) %>% 
  group_by(City, State) %>% 
  summarise(across(starts_with("X"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_longer(cols = starts_with("X"), names_to = "Date", values_to = "Deaths") %>% 
  mutate(Date = as.Date(substr(Date, 2, nchar(Date)), format = "%Y.%m.%d"),
         city = paste0(City, ", US-", State)) %>% 
  select(-c("City", "State"))

# Join
data2_cases <- inner_join(data2, covid_confirmed_clean, by = c("Date", "city"))
data2_cases_deaths <- inner_join(data2_cases, covid_deaths_clean, by = c("Date", "city")) %>% 
  mutate(FatalityRate = round(Deaths / Confirmed * 100, 2),
         AirTrafficIndex = PercentOfBaseline/100 - 1)


## Build R Shiny app
# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* CSS to adjust slider input font size */
      .irs-single, .irs-grid-text, .irs-from, .irs-to, .irs-min, .irs-max {
        font-size: 16px; /* Adjust the font size here */
      }
      .title {
        text-align: center; /* Center align the title */
      }
      .blurb {
        text-align: center; /* Center align the blurb */
        font-size: 18px;
      }
      .notes {
        font-size: 16px;
      }
      
    "))
  ),
  titlePanel(
    div(
      class = "title",
      h2("COVID-19 Impact on US Airport Traffic"),
    )
  ),
  div(
    class = "blurb",
    p("This app visualizes the impact of COVID-19 on US airport traffic across 17 major cities from March to December 2020."),
    p("Explore the data using the date slider and the map markers."),
    style = "margin-left: 20px; margin-top: 10px; margin-bottom: 20px;"  # Adjust styling as needed
  ),
  
  fluidRow(
    column(
      width = 12,
      align = "center",
      sliderInput("date_slider", label=NULL,
                  min = as.Date("2020-03-16", "%Y-%m-%d"), 
                  max = as.Date("2020-12-02", "%Y-%m-%d"),
                  value = as.Date("2020-03-16"),
                  timeFormat = "%Y-%m-%d",
                  animate = animationOptions(
                    interval = 500,
                    loop = FALSE,
                    playButton = list(label = "Play"),
                    pauseButton = list(label = "Pause")
                  ))
    )
  ),
  fluidRow(
    column(
      width = 12,
      leafletOutput("map")
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      plotlyOutput("line_chart")
    )
  ),
  div(
    class = "notes",
    p("Note: The Airport Traffic Index represents daily airport traffic volume compared to that during the period from February 1 to March 15 in 2020 (baseline period). 
      Specifically, it is calculated as percentage decrease in airport traffic from the baseline period.
      Covid Fatality Rate estimates the proportion of deaths among all confirmed cases."),
    p("Sources: "),
    p("COVID-19's Impact on Airport Traffic, Kaggle, available at https://www.kaggle.com/datasets/terenceshin/covid19s-impact-on-airport-traffic"),
    p("USAFACTS, COVID-19 Cases and Deaths, available athttps://usafacts.org/visualizations/coronavirus-covid-19-spread-map/"),
    style = "margin-left: 20px; margin-top: 10px; margin-bottom: 20px;" 
  ),
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data2_cases_deaths[data2_cases_deaths$Date == input$date_slider, ]
  })
  
  output$map <- renderLeaflet({
    
    reds <- colorNumeric("Reds", domain=NULL) # Color palette
    
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      setView(lng = mean(data2_cases_deaths$longitude), lat = mean(data2_cases_deaths$latitude), zoom = 3) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~sqrt(Confirmed) * 1/20,  # Change circle radius based on metric
        fillColor = ~colorQuantile("Reds", Confirmed)(Confirmed),  # Color gradient based on metric
        fillOpacity = 0.6,
        stroke = TRUE,
        color = ~ifelse(Confirmed > 0, reds(Confirmed), NA),
        weight = 1,
        label = ~paste0("City: ", City, ", ", State, "; ", "# of Confirmed Cases: ", Confirmed),
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px", direction = "auto"),
        group = "Cities"
      ) %>%
      addLayersControl(overlayGroups = "Cities", options = layersControlOptions(collapsed = FALSE)) %>% 
      addLegend(
        "bottomright",
        pal = reds, values = ~Confirmed,
        title = "Confirmed Cases",
        opacity = 0.5, bins = 4
      )
  })
  
  # Initialize selected inputs
  selected_city_name <- reactiveVal(NULL)
  selected_city_airport <- reactiveVal(NULL)
  selected_date <- reactiveVal(as.Date("2020-03-16"))
  
  # Handle marker click events
  observeEvent(input$map_marker_click, {
    # Get clicked marker's latitude and longitude
    click_lat <- input$map_marker_click$lat
    click_lng <- input$map_marker_click$lng
    
    # Filter data for the selected city
    city_data <- data2_cases_deaths %>%
      filter(
        abs(latitude - click_lat) < 0.05,  # Tolerance for latitude and longitude
        abs(longitude - click_lng) < 0.05,
      )
    
    # Get the name of the selected city and airport
    city_name <- unique(city_data$City)
    airport_name <- unique(city_data$AirportName)
    
    # Update the reactive value
    selected_city_name(city_name)
    selected_city_airport(airport_name)
  })
  
  # Update the selected date when the slider changes
  observe({
    selected_date(input$date_slider)
  })
  
  # Render the line chart for the selected city and time frame
  output$line_chart <- renderPlotly({
    req(selected_city_name())
    req(selected_city_airport())
    req(selected_date())
    
    # Filter data within the desired date range
    city_data <- data2_cases_deaths %>%
      filter(
        City == selected_city_name(),
        AirportName == selected_city_airport(),
        Date >= (selected_date() - 15) & Date <= (selected_date() + 15)
      )
    
    # Create a line chart for Fatality Rate and Airport Traffic Index for the selected city over time
    plot_ly() %>%
      add_lines(x = ~Date, y = ~FatalityRate, data = city_data, name = "Fatality Rate",
                yaxis = "y1", type = "scatter", mode = "lines", line = list(color = "lightcoral")) %>%
      add_lines(x = ~Date, y = ~AirTrafficIndex, data = city_data, name = "Airport Traffic Index",
                yaxis = "y2", type = "scatter", mode = "lines", line = list(color = "lightskyblue")) %>%
      layout(
        title = paste0(selected_city_name(), " - ", selected_city_airport(), ": COVID Fatality Rate and Airport Traffic Index"),
        yaxis = list(title = "Fatality Rate", side = "left", showgrid = FALSE),
        yaxis2 = list(title = "Airport Traffic Index", side = "right", overlaying = "y", showgrid = FALSE),
        legend = list(x = 0.1, y = 0.99),
        margin = list(l=50, r=50, b=50, t=50)
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
  
# EOF

##GEOG324 R Shiny Web application project; by Charlie Hunter, Ashley Dai, Chris Seibert, and Harry Staples.
# Required Libraries-
# Shiny app
library(shiny)
# Better tables
library(DT)
# Plottig
library(plotly)
library(ggthemes)
library(ggplot2)
# Mapping/ spatial data
library(sf)
library(tmap)
library(shinyWidgets)
library(leaflet)
# Data handling
library(lubridate)
library(tidyverse)
# Web/ json
library(httr)
library(jsonlite)
library(geojsonio)
# Creates an 'About' for the application; acknowledging where data has been sourced and the intention with which it was created.
ABOUTText <-
    "<h3> Aotearoa Weather Updates ('AWU')</h3>
  <p>This is an R Shiny application which demonstrates web queries to map and plot live Aotearoa weather updates recorded from
<a href=/'https://api.openweathermap.org//'>OpenWeatherMap</a>.</p>
  <p>&nbsp;</p>
  <hr/>
  <h4>About this application</h4>Aotearoa Weather Updates (‘AWU’) provides a user with an interactive and multi functional platform towards observing live national weather information. This application was initiated in light of a combined desire to create a weather service platform that provides regularly updated information on key weather characteristics. Such an application has therefore been created to present weather information services 24 hours a day, 7 days a week, 365 days to safeguard New Zealanders health and safety.
  <p>
  <hr/>
  <h4>Data providers</h4>
  <p>All data presented are copyright of the respective owners and they are gratefully acknowledged:</p>
  <ul>
	<li>Weather Data: <a href='https://api.openweathermap.org//'>OpenWeatherMap</a>. Data license: <a href='https://creativecommons.org/licenses/by-sa/4.0/'>Creative Commons Attribution 4.0 New Zealand</a>.</li>
  </ul>
  <hr/>
  <h4>App licence</h4>
  <p>&#169; Copyright 2020 Charlie Hunter, Ashley Dai, Chris Siebert, and Harry Staples/ University of Canterbury </p>
  <p>Permission is  granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the \"Software\"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:</p>
  <p>The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.</p>
  <p>THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.</p>
"
# Function for getting the data from the API. This sources the data from the free openweathermap server. The dataset comes with more variables than required so the appropriate variables were selected for the application.
time_var <- substr(date(), 0, 14)
getWeather <- function() {
    
    we.URL <- GET("http://api.openweathermap.org/data/2.5/group?id=2192362,2193733,2179537,2181133,2186280,2185018,2191562,6204696,2206895,7910066,2190767,2186239,2181742,6230919,6228827&appid=46817d1fde890313025e155d12a9c62b")
    we.data <- rawToChar(we.URL$content) %>% fromJSON()
    weather <- list()
    weather$City <- we.data$list$name
    weather$temp <- we.data$list$main$temp
    weather$feel_like <- we.data$list$main$feels_like
    weather$humidity <- we.data$list$main$humidity
    weather$visib <- we.data$list$visibility
    weather$pressure <- we.data$list$main$pressure
    weather$wind_speed <- we.data$list$wind$speed
    weather$wind_degree <- we.data$list$wind$deg
    weather$lon <- we.data$list$coord$lon
    weather$lat <- we.data$list$coord$lat
    weather <- as.data.frame(weather)
    #converting from list to SF by coordinates
    data <- st_as_sf(weather, coords = c("lon","lat"))
    #converting Temperature data from degrees Kelvin to degrees Celsius
    data <- data %>% mutate(temp = temp - 273.15)
    data <- data %>% mutate(feel_like = feel_like - 273.15)
    return(data)
}
# User interface.
ui <- navbarPage("Aotearoa Weather Updates ('AWU')", id="nav",
                 
                 # The application consists of multiple panels all of which are created below:
                 
                 # Map panel-
                 tabPanel("Map",
                          div(class="outer",
                              
                              setBackgroundColor("#bad6df"),
                              
                              tmapOutput(outputId = "map", width="1450", height="1050"),
                              
                              # Creates a panel to store controls.
                              absolutePanel(id = "controls",
                                            draggable = FALSE, left = 40, bottom = 40,
                                            width = "auto", height = "auto",
                                            
                                            # Date of data recorded, rounded to the nearest hour.
                                            h5("Last Updated:", time_var, "00"),
                                            
                                            # Radio buttons to allow the user to plot different variables.
                                            radioButtons(inputId = "varID", label = h4("Variable:"),
                                                         choices = c("Temperature" = "temp", "Pressure" = "Pressure", "Visibility" = "Visibility", "Wind" = "Wind", "Humidity" = "humidity", "All"= "All"),
                                                         inline = TRUE)
                                            
                              ),
                              
                              # Crediting sourced data to OpenWeatherMap.
                              tags$div(id="cite",
                                       'Data credit: ', tags$em('OpenWeatherMap'), ', https://openweathermap.org/api'
                              )
                          )
                 ),
                 
                 # Data table panel-
                 tabPanel("Data table",
                          DT::dataTableOutput(outputId = "datatable"),
                          tags$div(id="cite",
                                   'Data credit: ', tags$em('OpenWeatherMap'), ', https://openweathermap.org/api'
                          )
                 ),
                 
                 
                 # Plots panel-
                 tabPanel("Plots",
                          
                          radioButtons(inputId = "plotID", label = h4("Variable:"),
                                       choices = c("Temperature" = "temp", "Pressure" = "Pressure", "Visibility" = "Visibility", "Wind" = "Wind", "Humidity" = "humidity"),
                                       inline = TRUE),
                          plotlyOutput("scatterPlot", width="100", height="50"),
                          fluidRow(
                              column(12,
                                     plotlyOutput("histDepth")
                              )
                          ),
                          tags$div(id="cite",
                                   'Data credit: ', tags$em('OpenWeatherMap'), ', https://openweathermap.org/api'
                          )
                 ),
                 
                 # About panel-
                 tabPanel("About",
                          htmlOutput("about"),
                          tags$div(id="cite",
                                   'Data credit: ', tags$em('OpenWeatherMap'), ', https://openweathermap.org/api'
                          )
                 )
                 
)
server <- function(input, output, session) {
    # Creates the master dataset.  
    weather <- getWeather()
    weather <- weather %>% rename("Temperature (C) " = temp )
    weather <- weather %>% rename("Feels Like (C) " = feel_like )
    
    # Plot function is created for plotting the data and displaying it in the plot tab.
    plotFunction <- function(Value, Ytitle, title) {
        ggplot(weather, aes(x=City, y=Value))+
            geom_point() +
            labs(x = "City", y = Ytitle, title = title)
    }
    
    # Eventlist reacts to a change in the input$varID or input$plotID enabling the interface to update and display different variables
    eventListen <- reactive({
        cat("reactive (eventListen) | ")
        
        list(input$varID, input$plotID)
    })
    
    # Layer function for mapping the different data in the map tab. The boolean logic is used to display the map with either one variable selected or alternatively with all variables selected.
    weLayerF <- function(weather, var, var2, name ,palette, breaks, boolen) {
        cat("weLayer | ")
        if (boolen == 1 ) {
            tm_shape(weather, name = 'weather') +
                tm_bubbles(size = 0.05,
                           col = var, palette = palette,
                           style = "fixed",
                           popup.vars = c(name, var ,var2),
                           breaks = breaks,
                           id = "name",
                           scale = 10, alpha = 1,
                           zindex = 401) +
                tm_scale_bar(text.size = 100, size = 100, position = c("right", "bottom"))
        } else {
            tm_shape(weather, name = 'weather') +
                tm_bubbles(size = 0.05,
                           col = "Temperature (C) ", palette = palette,
                           style = "fixed",
                           popup.vars = c("City", "Temperature (C) ", "Feels Like (C) ", "Pressure (hPa) ","Visibility (M) ","Wind Speed (m/s) ","Wind Direction", "Humidity (%) "),
                           breaks = breaks,
                           id = "name",
                           scale = 10, alpha = 1,
                           zindex = 401) +
                tm_scale_bar(text.size = 100, size = 100, position = c("right", "bottom"))
            
            
            
        }
        
        
        
        
    }
    
    # Outputs the default map to the temperature variable, when first opening the application.
    output$map <- renderTmap( {
        cat("renderTmap (initialise map) | ") +
            tm_basemap(c("OpenStreetMap.Mapnik","Esri.OceanBasemap","CartoDB.DarkMatter"),alpha = 0.7) +
            weLayerF(weather,"Temperature (C) ", "Feels Like (C) " ,"City","-RdYlBu", seq(from = -20, to = 45, by = 4), 1)
    })
    
    # Outputs the chosen map of any of the variables, when a radio button is selected.
    observeEvent(eventListen(),{
        cat("observeEvent (update map) | ")
        
        # Obtains the set of required Weather data with a call to the weather function, this makes ensures the data is live.
        weather <- getWeather()
        
        weather <- weather %>% rename("Temperature (C) " = temp )
        weather <- weather %>% rename("Pressure (hPa) " = pressure )
        weather <- weather %>% rename("Humidity (%) " = humidity )
        weather <- weather %>% rename("Visibility (M) " = visib )
        weather <- weather %>% rename("Wind Speed (m/s) " = wind_speed )
        weather <- weather %>% rename("Wind Direction" = wind_degree )
        weather <- weather %>% rename("Feels Like (C) " = feel_like )
        
        boolen <- 1
        if (input$varID == "Temperature") {
            var <-  "Temperature (C) "
            var2 <- "Feels Like (C) "
            palette <- "-RdYlBu"
            
            breaks <- seq(from = -20, to = 45, by = 4)
        } else if (input$varID == "humidity") {
            var <-  "Humidity (%) "
            var2 <- NULL
            palette <- "-YlOrRd"
            breaks <- seq(from = 0, to = 100, by = 2)
        } else if (input$varID == "Visibility") {
            print("yup")
            var <-  "Visibility (M) "
            var2 <- NULL
            palette <- "-Greys"
            breaks <- seq(from = 0, to = 10000, by = 500)
        } else if (input$varID == "Pressure") {
            var <-  "Pressure (hPa) "
            var2 <- NULL
            palette <- "PuBu"
            breaks <- seq(from = 980, to = 1030, by = 2)
        } else if (input$varID == "Wind") {
            var <-  "Wind Speed (m/s) "
            var2 <- "Wind Direction"
            palette <- "Oranges"
            breaks <- seq(from = 0, to = 50, by = 2)
        } else if (input$varID == "All") {
            boolen <- 0
            palette <- "-RdYlBu"
            breaks <- seq(from = -20, to = 45, by = 4)
        } else {
            var <-  "Temperature (C) "
            var2 <- "Feels Like (C) "
            palette <- "-RdYlBu"
            breaks <- seq(from = -20, to = 45, by = 4)
        }
        
        # Breaks were chosen in a way which was deemed appropriate for each variable.
        
        # Tmap Proxy was used so that the map does not reset every time a new variables is selected.
        tmapProxy("map", session, {
            weLayerF(weather, var, var2, "City" ,palette, breaks, boolen) # choose the new layer
        })
        
        
        
    }, ignoreInit = TRUE)
    
    # Produces an updated table of all weather variables when the data table tab is selected.
    dataTable <- eventReactive(eventListen(), {
        
        # Obtains the set of required Weather data with a call to the weather function, this makes sure the data is live.
        weather <- getWeather()
        weather2 <- list()
        weather$geometry <- NULL
        weather2 <- weather %>% select(City, temp, pressure ,humidity, visib, wind_speed, wind_degree)
        weather2 <- weather2 %>% mutate(temp = round(temp,1))
        weather2 <- weather2 %>% mutate(pressure = round(pressure,1))
        weather2 <- weather2 %>% mutate(humidity = round(humidity,1))
        weather2 <- weather2 %>% mutate(visib = round(visib,1))
        weather2 <- weather2 %>% mutate(wind_speed = round(wind_speed,1))
        weather2 <- weather2 %>% mutate(wind_degree = round(wind_degree,1))
        weather2 <- weather2 %>% rename("Temperature" = temp )
        weather2 <- weather2 %>% rename("Pressure" = pressure )
        weather2 <- weather2 %>% rename("Humidity" = humidity )
        weather2 <- weather2 %>% rename("Visibility" = visib )
        weather2 <- weather2 %>% rename("Wind Speed" = wind_speed )
        weather2 <- weather2 %>% rename("Wind Direction" = wind_degree )
        DT::datatable(data = weather2, escape = FALSE)
    }, ignoreNULL=FALSE)
    
    # Produces the table with updated data; when dataTable changes, this will react and run.
    output$datatable <- DT::renderDataTable({
        dataTable()
    })
    
    # Outputs the default plot to the temperature variable, when first opening the application.
    observeEvent(eventListen(),{
        output$histDepth <- renderPlotly({
            ggplot(weather, aes(x=City, y=temp, colour=City))+
                geom_point() +
                labs(x = "City", y = "Temperature")
            
        })
    })
    
    # Outputs the chosen plot of any of the variables when a radio button is selected.
    observeEvent(eventListen(),{
        cat("observeEvent (update plot) | ")
        
        # Obtains the set of required Weather data with a call to the weather function, this ensures the data is live.
        weather <- getWeather()
        if (input$plotID == "Temperature") {
            yVar <- weather$temp
            Ytitle <- "Temperature (C)"
            title <- " 	Live temperature per Aotearoa cities"
        } else if (input$plotID == "Pressure") {
            yVar <- weather$pressure
            Ytitle <- "Pressure (hPa)"
            title <- " 	Live air pressure per Aotearoa cities"
            
        } else if (input$plotID == "Visibility") {
            yVar <- weather$visib
            Ytitle <- "Visibility (m)"
            title <- "Live visibility per Aotearoa cities"
            
        } else if (input$plotID == "Wind") {
            yVar <- weather$wind_speed
            Ytitle <- "Wind (m/s)"
            title <- "Live wind speeds per Aotearoa cities"
            
        } else if (input$plotID == "humidity") {
            yVar <- weather$humidity
            Ytitle <- "Humidity (%)"
            title <- "  	Live humidity percentages per Aotearoa cities"
            
        } else {
            
            yVar <- weather$temp
            title <- "Live temperature per Aotearoa cities"
            Ytitle <- "Temperature (C)"
        }
        observeEvent(eventListen(),{
            cat("observeEvent (update plot2) | ")
            output$histDepth <- renderPlotly({
                plotFunction(yVar, Ytitle, title)
                
            })
        })
    })
    
    # Outputs the HTML about text, which is information about the application.
    output$about <- renderText({ ABOUTText })
}
# Runs the application.
shinyApp(ui = ui, server = server)


# AgExplorer Shiny Structure


library(tidyverse)
library(shiny)
library(bslib)
library(here)
library(sf)
library(tsibble)
library(feasts)
library(fable)
library(broom)

#sourced
source(here("R", "cows_trend.R"))
source(here("R", "cows_us_trend.R"))
source(here("R", "emissions_trend.R"))

#read in data
states <- read_csv(here("data", "fiftystatesCAN.csv"))

#wrangling
states_tojoin <- states %>%
    mutate(join_state = toupper(states$region))

cows_tojoin <- cows_trend %>%
    mutate(join_state = toupper(cows_trend$state))

cows_state <- cows_tojoin %>%
    inner_join(states_tojoin, by = "join_state") %>%
    mutate(year = as.numeric(year))

ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),
                
                navbarPage("Ag-Explorer: Looking at Agricultural Trends in the US",
                           tabPanel("Overview",
                                    mainPanel("The purpose of this app is to allow users to explore agricultural production and waste trends over time in the U.S. We have included estimates for the economic potential for converting these waste streams to biogas and digestate with digesters.")
                           ),
                           
                           #chloropleth
                           tabPanel("Trends in Agricultural Production",
                                    sidebarLayout(
                                        sidebarPanel("WIDGETS!",
                                                     sliderInput(inputId = "pick_year", label = "Choose Year",
                                                                 
                                                                 min = 1970,
                                                                 max = 2019,
                                                                 value = 1970,
                                                                 sep = "",
                                                                 ticks = FALSE,
                                                                 animate = TRUE)),
                                        
                                        mainPanel("OUTPUT!",
                                                  plotOutput("state_plot"))
                                    )),
                           
                           tabPanel("Number of Dairy Cows by State",
                                    sidebarLayout(
                                        sidebarPanel("Time Series Selections",
                                                     checkboxGroupInput(inputId = "pick_state_2",
                                                                        
                                                                        label = "Choose State / Total U.S.",
                                                                        choices = unique(cows_us_trend$state))),
                                        mainPanel("Dairy Cow Time Series",
                                                  
                                                  plotOutput("cows_plot"))
                                    )),
                           
                           tabPanel("Total U.S. Dairy Cow and Emissions Trends",
                                    
                                    sidebarLayout(
                                        sidebarPanel("Time Series Selections",
                                                     checkboxGroupInput(inputId = "pick_emissions",
                                                                        label = "Choose Emissions Type",
                                                                        choices = unique(emissions_trend$emissions_type))),
                                        mainPanel("output placeholder",
                                                  plotOutput("emissions_plot"))
                                    )),
                           tabPanel("Digester Economic Potential",
                                    sidebarLayout(
                                        sidebarPanel("Placeholder Widget - date range",
                                                     dateRangeInput(inputId = "dates",
                                                                    label = h3("Date range")),
                                                     hr(),
                                                     fluidRow(column(4, verbatimTextOutput("value")))),
                                        mainPanel("output placeholder")
                                    ))
                ))

server <- function(input, output) {
    
    chloro_reactive <- reactive({
        
        cows_state %>%
            group_by(state) %>%
            filter(year %in% input$pick_year)
        
    })
    
    output$state_plot <- renderPlot(
        
        
        
        ggplot() +
            geom_polygon(data = chloro_reactive(), aes(x = long, y = lat, group = group, fill = cows),
                         color = "white") +
            scale_fill_continuous(limits=c(0,2000), type = "viridis") +
            coord_quickmap() +
            theme_bw()
    )
    
    time_series_cows_reactive <- reactive({
        cows_us_trend %>%
            filter(state %in% input$pick_state_2) %>%
            mutate(as.Date(as.character(year), format = "%Y"))
    })
    
    output$cows_plot <- renderPlot(
        ggplot() +
            
            geom_line(data = time_series_cows_reactive(), aes(x = year, y = cows, color = state)) +
            labs(color = "")
    )
    
    
    time_series_emissions_reactive <- reactive({
        emissions_trend %>%
            filter(emissions_type %in% input$pick_emissions)
    })
    
    output$emissions_plot <- renderPlot(
        ggplot() +
            geom_line(data = time_series_emissions_reactive(), aes(x = year, y = total_emissions, color = emissions_type))
    )
    
    tab4_reactive <- reactive({
        starwars %>%
            filter(height %in% input$dates)
    })
    
}

shinyApp(ui = ui, server = server)

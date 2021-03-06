# agxplorR Shiny Structure


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
source(here("R", "milk_trend.R"))
source(here("R", "cows_us_trend.R"))
source(here("R", "emissions_trend.R"))
source(here("R", "food_impact.R"))
source(here("R", "milk_us_trend.R"))

#read in data
states <- read_csv(here("data", "fiftystatesCAN.csv"))

#wrangling
states_tojoin <- states %>%
    mutate(join_state = toupper(states$region))

milk_tojoin <- milk_trend %>%
    mutate(join_state = toupper(milk_trend$state))

milk_state <- milk_tojoin %>%
    inner_join(states_tojoin, by = "join_state") %>%
    mutate(year = as.numeric(year))

# User Interface

ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),
                navbarPage("AgxploR: Agricultural Trends & Impacts in the US",
                           tabPanel("Overview",
                                    mainPanel("The purpose of this app is to allow users to explore agricultural production and related environmental impacts (EI) over time in the U.S. through interactive visualizations of EI data.", width = 8, offset = 4)
                           ),
                           # Tab 1: Chloropleth and Line Chart
                           tabPanel("National Milk Production Over Time",
                                    sidebarLayout(
                                        sidebarPanel("Select the year for which you'd like to see milk production mapping - or push the play arrow to watch changes over the entire time line",
                                                     sliderInput(inputId = "pick_year", label = "Choose Year",
                                                                 min = 1970,
                                                                 max = 2019,
                                                                 value = 1970,
                                                                 sep = "",
                                                                 ticks = FALSE,
                                                                 animate = TRUE),
                                                     checkboxGroupInput(inputId = "pick_state_2",
                                                                        label = "Choose State / Total U.S.",
                                                                        choices = unique(milk_us_trend$state)
                                                     )),
                                        mainPanel("Popular documentaries such as Cowspiracy have increased public awareness around some of the environmental impacts of meat production - but what about dairy? How do these foods compare to other foods as far as environmental impact? Let's start by looking at national milk production over time - in millions of gallons - to get a sense of how big of an impact milk may have.",
                                                  plotOutput("state_plot"),
                                                  plotOutput("milk_plot"))
                                    )),
                           # Tab 2: Spider Charts
                           tabPanel("Comparing Food Impacts by Serving",
                                    sidebarLayout(
                                        sidebarPanel("Time Series Selections",
                                                     checkboxGroupInput(inputId = "pick_state_2",
                                                                        label = "Choose State / Total U.S.",
                                                                        choices = unique(cows_us_trend$state))),
                                        mainPanel("Dairy Cow Time Series",
                                                  plotOutput("cows_plot"))
                                    )),
                           # Tab 3: Stacked Multi-variable Bar Charts
                           tabPanel("Comparing Total Annual Food Impacts",
                                    sidebarLayout(
                                        sidebarPanel("Time Series Selections",
                                                     checkboxGroupInput(inputId = "pick_emissions",
                                                                        label = "Choose Emissions Type",
                                                                        choices = unique(emissions_trend$emissions_type))),
                                        mainPanel("output placeholder",
                                                  plotOutput("emissions_plot"))
                                    )),
                           # Tab 4: Chloropleth for 8 Foods
                           tabPanel("Food Production Map",
                                    sidebarLayout(
                                        sidebarPanel("Food Selections",
                                                     checkboxGroupInput(inputId = "pick_food",
                                                                        label = "Choose Foods for Comparison",
                                                                        choices = unique(food_impact$product),
                                                                        selected = food_impact[1,1])),
                                        mainPanel("output placeholder",
                                                  plotOutput("impact_chart"))
                                    ))
                ))


# Server

server <- function(input, output) {
    
    # Tab 1: Chloropleth and Line Chart 
    
    # T1: Chloropleth - Dairy
    chloro_reactive <- reactive({
        milk_state %>%
            group_by(state) %>%
            filter(year %in% input$pick_year)
    })
    
    output$state_plot <- renderPlot(
        ggplot() +
            geom_polygon(data = chloro_reactive(), aes(x = long, y = lat, group = group, fill = milk_l_e6),
                         color = "white") +
            scale_fill_continuous(limits=c(min(milk_state$milk_l_e6),max(milk_state$milk_l_e6)), type = "viridis") +
            coord_quickmap() +
            labs(fill = "Annual milk production \n(10^6 liters)") +
            theme_bw()
    )
    
    # T1: Line graph - Dairy
    time_series_milk_reactive <- reactive({
        milk_us_trend %>%
            filter(state %in% input$pick_state_2) %>%
            mutate(as.Date(as.character(year), format = "%Y"))
    })
    output$milk_plot <- renderPlot(
        ggplot() +
            geom_line(data = time_series_milk_reactive(), aes(x = year, y = milk_l_e6, color = state)) +
            labs(color = "")
    )
    
    # Tab 2: Spider Charts
    
    
    
    # Tab 3: Stacked Multi-variable Bar Charts
    
    # T3: Stacked Bar Plot
    time_series_emissions_reactive <- reactive({
        emissions_trend %>%
            filter(emissions_type %in% input$pick_emissions)
    })
    output$emissions_plot <- renderPlot(
        ggplot() +
            geom_line(data = time_series_emissions_reactive(), aes(x = year, y = total_emissions, color = emissions_type))
    )
    
    # Tab 4: Chloropleth for 8 Foods
    
    # T4: Chloropleth
    food_impact_reactive <- reactive({
        food_impact %>%
            filter(product %in% input$pick_food)
    })
    output$impact_chart <- renderPlot(
        ggplot() +
            geom_col(data = food_impact_reactive(), aes(x = effect_type, y = mean, fill = product),
                     position = "dodge")
        #facet_grid(.~effect_type, scales = "free_y")
    )
}


shinyApp(ui = ui, server = server)

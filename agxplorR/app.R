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
library(patchwork)
library(gridExtra)

#sourced
source(here("R", "milk_trend.R"))
source(here("R", "cows_us_trend.R"))
source(here("R", "emissions_trend.R"))
source(here("R", "food_impact.R"))
source(here("R", "milk_us_trend.R"))
source(here("R", "serv_impact.R"))
source(here("R", "total_impact.R"))

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
                                    mainPanel("The purpose of this app is to allow users to explore agricultural production and related environmental impacts (EI) over time in the U.S. through interactive visualizations of EI data.", width = 12)
                           ),
                           # Tab 1: Chloropleth and Line Chart
                           tabPanel("National Milk Production Over Time",
                                    sidebarLayout(
                                        sidebarPanel(h3("Map of U.S. Milk Production"),
                                            "Select the year for which you'd like to see milk production mapping - or push the play arrow to watch changes over the entire time line.",
                                                     br(),
                                                     br(),
                                                     sliderInput(inputId = "pick_year", label = "Choose Year",
                                                                 min = 1970,
                                                                 max = 2019,
                                                                 value = 1970,
                                                                 sep = "",
                                                                 ticks = FALSE,
                                                                 animate = TRUE),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                            h3("Dairy Production by State Over Time"),
                                                     "Select the box next to the states for which you would like to compare dairy production over time.",
                                                     br(),
                                                     br(),
                                                     checkboxGroupInput(inputId = "pick_state_2",
                                                                        label = "Choose State / Total U.S.",
                                                                        choices = unique(milk_us_trend$state),
                                                                        selected = milk_us_trend[1,2]
                                                     )),
                                        mainPanel("Popular documentaries such as Cowspiracy have increased public awareness around some of the environmental impacts of meat production - but what about dairy? How do dairy foods compare to other foods as far as environmental impact? Let's start by looking at national milk production over time - in millions of liters - to get a sense of how the impacts of milk and dairy production may be distributed by state.",
                                                  plotOutput("state_plot"),
                                                  plotOutput("milk_plot"))
                                    )),
                           # Tab 2: Spider Charts
                           tabPanel("Comparing Food Impacts by Serving",
                                    sidebarLayout(
                                        sidebarPanel(h3("Environmental Impact by Food"),
                                                     "Select the box next to the foods for which you would like to compare environmental impacts",
                                                     br(),
                                                     br(),
                                                     checkboxGroupInput(inputId = "pick_state_2",
                                                                        label = "Choose State / Total U.S.",
                                                                        choices = unique(cows_us_trend$state))),
                                        mainPanel("Serving up change: The chart below allows you to compare the relative environmental impacts of each food group by serving. This will allow you to see how your food choices can contribute to, or avoid, some of the negative consequences of food production with every meal.",
                                                  plotOutput("cows_plot"))
                                    )),
                           # Tab 3: Stacked Multi-variable Bar Charts
                           tabPanel("Comparing Total Annual Food Impacts",
                                    sidebarLayout(
                                        sidebarPanel("Food Item Selections",
                                                     checkboxGroupInput(inputId = "pick_food",
                                                                        label = "Choose Food Item",
                                                                        choices = unique(total_impact$commodity))),
                                        mainPanel("output placeholder",
                                                  plotOutput("total_production_plot"),
                                                  plotOutput("total_land_use_plot"),
                                                  plotOutput("total_ghg_plot"),
                                                  plotOutput("total_acid_plot"),
                                                  plotOutput("total_eutroph_plot"),
                                                  plotOutput("total_stresswater_plot"))
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
            labs(color = "",
                 x = "Year",
                 y = "Milk Production (millions of liters)") +
            theme_bw()
    )
    
    # Tab 2: Spider Charts
    
    
    
    # Tab 3: Stacked Multi-variable Bar Charts
    
    # T3: Stacked Bar Plot
    total_impact_reactive <- reactive({
        total_impact %>%
            filter(commodity %in% input$pick_food)
    })
    
    output$total_production_plot <- renderPlot(
        ggplot(data = total_impact_reactive(), aes(x = commodity, y = value, fill = commodity)) +
            geom_col() +
            labs(x = "") +
            labs(y = "Total Production (millions of kg)")
    )
    
    output$total_land_use_plot <- renderPlot(
        ggplot(data = total_impact_reactive(), aes(x = commodity, y = land_use_total, fill = commodity)) +
            geom_col() +
            labs(x = "") +
            labs(y = "Total Land Use (millions of m^2)")
    )
    
    output$total_ghg_plot <- renderPlot(
        ggplot(data = total_impact_reactive(), aes(x = commodity, y = ghg_2013_total, fill = commodity)) +
            geom_col() +
            labs(x = "") +
            labs(y = "GHG emissions (millions of kg of CO2 equivalent)")
    )
    
    output$total_acid_plot <- renderPlot(
        ggplot(data = total_impact_reactive(), aes(x = commodity, y = acid_total, fill = commodity)) +
            geom_col() +
            labs(x = "") +
            labs(y = "Acidification (millions of g of SO2 equivalent)")
    )
    
    output$total_eutroph_plot <- renderPlot(
        ggplot(data = total_impact_reactive(), aes(x = commodity, y = eutroph_total, fill = commodity)) +
            geom_col() +
            labs(x = "") +
            labs(y = "Eutrophication (millions of g PO4^3 equivalent)")
    )
    
    output$total_stresswater_plot <- renderPlot(
        ggplot(data = total_impact_reactive(), aes(x = commodity, y = stresswater_total, fill = commodity)) +
            geom_col() +
            labs(x = "") +
            labs(y = "Stress-weighted Water Use (millions of L)")
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

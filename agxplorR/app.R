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
library(fmsb)

#sourced
source(here("R", "milk_trend.R"))
source(here("R", "cows_us_trend.R"))
source(here("R", "emissions_trend.R"))
source(here("R", "food_impact.R"))
source(here("R", "milk_us_trend.R"))
source(here("R", "serv_impact.R"))

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
                           tabPanel("Milk Production Over Time",
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
                                                     checkboxGroupInput(inputId = "pick_serv",
                                                                        label = "Choose Food.",
                                                                        choices = unique(serv_impact$food_group[3:17]),
                                                                        selected = serv_impact[4,1]
                                                                        )),
                                        
                                        mainPanel("Serving up change: The chart below allows you to compare the relative environmental impacts of each food group by serving. This will allow you to see how your food choices can contribute to, or avoid, some of the negative consequences of food production with every meal.",
                                                  plotOutput("serv_plot", width = "800px", height = "775px"))
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
            labs(color = "",
                 x = "Year",
                 y = "Milk Production (millions of liters)") +
            theme_bw()
    )
    
    # Tab 2: Spider Charts
    serv_react <- reactive({
        serv_impact %>% 
            filter(food_group %in% c(15,1,input$pick_serv)) %>%
            column_to_rownames(var = "food_group")
    })
    
    legend_react <- reactive({
        serv_impact %>% 
            filter(food_group %in% input$pick_serv) %>%
            column_to_rownames(var = "food_group")
    })
    
    output$serv_plot <- renderPlot({
        radarchart(serv_react(),
                   plty = 1,
                   plwd = 3,
                   vlcex = 1.3,
                   pcol = serv_pal
                   )
        legend(x = 0.95, y = 0.2, legend = rownames(legend_react()), col = serv_pal, bty = "n", pch=20 , text.col = "black", cex=1.1, pt.cex=3)
    })
    
    
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

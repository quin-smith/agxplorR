# agxplorR Shiny App

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
source(here("R", "py_state.R"))

#read in data
states <- read_csv(here("data", "fiftystatesCAN.csv"))

#wrangling/joining
states_tojoin <- states %>%
    mutate(join_state = toupper(states$region))

milk_tojoin <- milk_trend %>%
    mutate(join_state = toupper(milk_trend$state))

milk_state <- milk_tojoin %>%
    inner_join(states_tojoin, by = "join_state") %>%
    mutate(year = as.numeric(year))

food_tojoin <- py_state %>% 
    mutate(join_state = toupper(py_state$state))

food_state <- food_tojoin %>% 
    inner_join(states_tojoin, by = "join_state")



# USER INTERFACE

ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),
                navbarPage("agxplorR: Agricultural Trends & Impacts in the US",

                           
                           # OVERVIEW PANEL: APP PURPOSE

                           tabPanel("Overview",
                                    tabsetPanel(
                                        tabPanel("About", 
                                                 h3("agxplorR: Purpose and Background"), 
                                                 p("Agriculture is a major contributer to greenhouse gas emissions, water usage and contamination, and land use change around the globe. The food choices we make individually and the agricultural practices we rely on collectively have a major impact on our environment. This app was created to allow users to explore agricultural production and related environmental impacts (EI) over time in the U.S. through interactive visualizations. See the papers and data sources cited below for more detailed background and in-depth explanations of data and findings."),
                                                 br(),
                                                 HTML("<p> App Created By: <a href= https://craigkopulsky.github.io/ck_website/>Craig Kopulsky</a>, <a href= https://kerry-nixon.github.io/kn_website/>Kerry Nixon</a>, and <a href= https://quin-smith.github.io/quinlan_smith/>Quin Smith</a>.</p>"),
                                                 p("Affiliation: Bren School of Environmental Science & Management, MESM Program, UCSB"),
                                                 br(),
                                                 img(src = "Fruits banner.jpg", width = "100%", style="display: block; margin-left: auto; margin-right: auto;"),
                                                 pre("LISOVSKAYA / GETTY IMAGES"),
                                                 br(),
                                                 br()),
                                        
                                        tabPanel("Citations",
                                             h3("Citations"),
                                             br(),
                                             HTML("<p> Clark, M. A., Springmann, M., Hill, J., & Tilman, D. (2019). Multiple health and environmental impacts of foods. 116(46), 23357–23362. <a href= https://www.pnas.org/content/116/46/23357.long>https://www.pnas.org/content/116/46/23357.long</a>.</p>"),
                                             br(),
                                             HTML("<p> Niklas. (2020, August 6). How Much Does an Egg Weigh? (Complete Chart of Various Sizes). Weight of Stuff. <a href = 'https://weightofstuff.com/how-much-does-an-egg-weigh/'>https://weightofstuff.com/how-much-does-an-egg-weigh/</a>.</p>"),
                                             br(),
                                             HTML("<p>Poore, J., & Nemecek, T. (2018). Reducing food’s environmental impacts through producers and consumers. 360, 987–992. <a href = 'https://science.sciencemag.org/content/360/6392/987.long'>https://science.sciencemag.org/content/360/6392/987.long</a>.</p>"),
                                             br(),
                                             HTML("<p> USDA, NASS, BEEF, SLAUGHTER - PRODUCTION, MEASURED IN LB, (2019); <a href= 'https://quickstats.nass.usda.gov/'>https://quickstats.nass.usda.gov/</a>.</p>"),
                                             br(),
                                             HTML("<p> USDA, NASS, CHICKENS, BROILERS - PRODUCTION, MEASURED IN LB, (2019); <a href= 'https://quickstats.nass.usda.gov/'>https://quickstats.nass.usda.gov/</a>.</p>"),
                                             br(),
                                             HTML("<p> USDA, NASS, EGGS - PRODUCTION, MEASURED IN EGGS, (2020); <a href= 'https://quickstats.nass.usda.gov/'>https://quickstats.nass.usda.gov/</a>.</p>"),
                                             br(),
                                             HTML("<p> USDA, NASS, MILK - PRODUCTION, MEASURED IN LB, (1970-2020); <a href= 'https://quickstats.nass.usda.gov/'>https://quickstats.nass.usda.gov/</a>.</p>"),
                                             br(),
                                             HTML("<p> USDA, NASS, TREE NUT TOTALS, UTILIZED - PRODUCTION, MEASURED IN TONS, (2019); <a href= 'https://quickstats.nass.usda.gov/'>https://quickstats.nass.usda.gov/</a>.</p>"),
                                             br(),
                                             HTML("<p> USDA, NASS, PEAS, DRY EDIBLE - PRODUCTION, MEASURED IN CWT, (2020); <a href= 'https://quickstats.nass.usda.gov/'>https://quickstats.nass.usda.gov/</a>.</p>"),
                                             br(),
                                             HTML("<p> USDA, NASS, HOGS - PRODUCTION, MEASURED IN LB, (2019); <a href= 'https://quickstats.nass.usda.gov/'>https://quickstats.nass.usda.gov/</a>.</p>"),
                                             br(),
                                             HTML("<p> USDA, NASS, POTATOES - PRODUCTION, MEASURED IN CWT, (2020); <a href= 'https://quickstats.nass.usda.gov/'>https://quickstats.nass.usda.gov/</a>.</p>"),
                                             br(),
                                             HTML("<p> USDA, NASS, RICE - PRODUCTION, MEASURED IN CWT, (2020); <a href= 'https://quickstats.nass.usda.gov/'>https://quickstats.nass.usda.gov/</a>.</p>"),
                                             br(),
                                             br())
                                    )),
                           
                          
                           
                            # TAB 1: Chloropleth and Line Chart
                           tabPanel("Milk Production Over Time",
                                    h3("Got Milk?"),
                                    "Popular documentaries such as Cowspiracy have increased public awareness around some of the environmental impacts of meat production - but what about dairy? How do dairy foods compare to other foods with respect to environmental impact? Let's start by looking at milk production - in millions of liters - to get a sense of how milk production is distributed by state, and how it has changed over time. Production data was sourced from the USDA.",
                                    br(),
                                    br(),
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
                                            h3("Dairy Production by State Over Time"),
                                                     "Select the box next to the states for which you would like to compare dairy production over time.",
                                                     br(),
                                                     br(),
                                                     checkboxGroupInput(inputId = "pick_state_2",
                                                                        label = "Choose State / Total U.S.",
                                                                        choices = unique(milk_us_trend$state),
                                                                        selected = milk_us_trend[1,2])
                                            ),
                                        mainPanel(
                                                  plotOutput("state_plot"),
                                                  br(),
                                                  br(),
                                                  plotOutput("milk_plot"))
                                    )),
                           
                           
                           # TAB 2: Spider Charts
                           tabPanel("Comparing Food Impacts by Serving",
                                    h3("Serving up Change"),
                                    p("The chart below allows you to visually compare the relative environmental impacts of each food group. Foods are rank-ordered by environmental impact per serving of food consumed, per day. Lower environmental impacts are considered better and higher environmental impacts are considered worse. As with the original data from Clark et.al. (2019), 'Foods are plotted so that the food group with the lowest mean impact for each environmental indicator has a value of 1 (innermost circle), and the food group with the highest mean impact for a given indicator has a value of 15 (outermost circle)'. This will allow you to better understand how your food choices are effecting the environment."),
                                    sidebarLayout(
                                        sidebarPanel(h3("Environmental Impact by Food"),
                                                     "Select the box next to the foods for which you would like to compare environmental impacts. SSBs is an abbreviation for sugar sweetened beverages.",
                                                     br(),
                                                     br(),
                                                     checkboxGroupInput(inputId = "pick_serv",
                                                                        label = "Choose Food.",
                                                                        choices = unique(serv_impact$food_group[3:17]),
                                                                        selected = serv_impact[4,1]
                                                                        )),
                                        
                                        mainPanel(
                                            plotOutput("serv_plot", width = "800px", height = "775px"))
                                    )),
                           
                           
                           # TAB 3: Stacked Multi-variable Bar Charts
                           tabPanel("Comparing Annual Food Impacts",
                                    h3("How Do These Ag Commodities Stack Up?"),
                                    p("Now that you have had a chance to compare environmental impacts of food per serving, let's look at things are a larger scale. The bar graphs below compare the total annual production of a selection of agricultural commodities and their related environmental impacts in the United States. Production data was obtained from the USDA and emissions data for each food type were drawn from Poore and Nemecek (2018). The production volumes are for the most recent year of available data for each commodity, and in all cases were for either 2019 or 2020."),
                                    sidebarLayout(
                                        sidebarPanel(h3("Comparing Total Annual Environmental Impact and Production of Foods"),
                                                     "Select the box next to the foods for which you would like to compare environmental impacts and production.",
                                                     br(),
                                                     br(),
                                                     checkboxGroupInput(inputId = "pick_food",
                                                                        label = "Choose Food Item",
                                                                        choices = unique(total_impact$commodity),
                                                                        selected = total_impact$commodity[4:5])),
                                        mainPanel(
                                                  plotOutput("total_production_plot"),
                                                  plotOutput("total_land_use_plot"),
                                                  plotOutput("total_ghg_plot"),
                                                  plotOutput("total_acid_plot"),
                                                  plotOutput("total_eutroph_plot"),
                                                  plotOutput("total_stresswater_plot"))
                                    )),
                           
                           
                           # TAB 4: Chloropleth for 8 Foods
                           tabPanel("Mapping Food Production",
                                    h3("Where Does Your Food Come From?"),
                                    "The map below allows you to view the distribution of agricultural commodity production by state in the U.S. This can indicate where environmental impacts associated with each commodity originate. While GHG emissions will have a global impact regardless of their point of origin, other impacts such as eutrophication and acidification may have more local or regional impacts to the area where commodities are produced.",
                                    br(),
                                    br(),
                                    "Production data was obtained from the USDA for the most recent year available for each agricultural commodity.",
                                    br(),
                                    br(),
                                    sidebarLayout(
                                        sidebarPanel(h3("Mapping Food Production"),
                                                     "Select the commodity from the drop-down menu for which you'd like to see annual production mapped across the U.S.",
                                                     selectInput("food_select", label = h3("Select box"), 
                                                                 choices = unique(py_state$commodity)
                                                     )),
                                        mainPanel(plotOutput("impact_chart"))
                                    ))
                ))


# SERVER

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
            labs(fill = "Annual milk production \n(10^6 liters)",
                 x = "",
                 y = "") +
            theme_bw() +
            theme(legend.title = element_text(size = 16),
                  legend.text = element_text(size = 16))
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
                 x = "\nYear",
                 y = "Milk Production (millions of liters)\n") +
            theme_bw() +
            theme(axis.title.x = element_text(size = 16),
                  axis.title.y = element_text(size = 16),
                  legend.text = element_text(size = 16)) +
            expand_limits(y=c(0, NA))
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
                   cglwd = 2,
                   cglcol = "darkgray",
                   pcol = serv_pal
                   )
        legend(x = 0.95, y = 0.2, legend = rownames(legend_react()), col = serv_pal, bty = "n", pch=20 , text.col = "black", cex=1.1, pt.cex=3)
    })
    
    
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
            labs(y = "millions of kg\n") +
            labs(fill = "") +
            labs(title = "Total Production") +
            theme_bw() +
            theme(legend.position = "none",
                  axis.text.y = element_text(size=16),
                  axis.text.x = element_text(size=16),
                  title = element_text(size=20))
    )
    
    output$total_land_use_plot <- renderPlot(
        ggplot(data = total_impact_reactive(), aes(x = commodity, y = land_use_total, fill = commodity)) +
            geom_col() +
            labs(x = "") +
            labs(y = "millions of m^2\n") +
            labs(fill = "") +
            labs(title = "Total Land Use") +
            theme_bw() +
            theme(legend.position = "none",
                  axis.text.y = element_text(size=16),
                  axis.text.x = element_text(size=16),
                  title = element_text(size=20))
    )
    
    output$total_ghg_plot <- renderPlot(
        ggplot(data = total_impact_reactive(), aes(x = commodity, y = ghg_2013_total, fill = commodity)) +
            geom_col() +
            labs(x = "") +
            labs(y = "millions of kg of CO2 equivalent\n") +
            labs(fill = "") +
            labs(title = "GHG Emissions") +
            theme_bw() +
            theme(legend.position = "none",
                  axis.text.y = element_text(size=16),
                  axis.text.x = element_text(size=16),
                  title = element_text(size=20))

    )
    
    output$total_acid_plot <- renderPlot(
        ggplot(data = total_impact_reactive(), aes(x = commodity, y = acid_total, fill = commodity)) +
            geom_col() +
            labs(x = "") +
            labs(y = "millions of g of SO2 equivalent\n") +
            labs(fill = "") +
            labs(title = "Acidification") +
            theme_bw() +
            theme(legend.position = "none",
                  axis.text.y = element_text(size=16),
                  axis.text.x = element_text(size=16),
                  title = element_text(size=20))
    )
    
    output$total_eutroph_plot <- renderPlot(
        ggplot(data = total_impact_reactive(), aes(x = commodity, y = eutroph_total, fill = commodity)) +
            geom_col() +
            labs(x = "") +
            labs(y = "millions of g PO4^3 equivalent\n") +
            labs(fill = "") +
            labs(title = "Eutrophication") +
            theme_bw() +
            theme(legend.position = "none",
                  axis.text.y = element_text(size=16),
                  axis.text.x = element_text(size=16),
                  title = element_text(size=20))
    )
    
    output$total_stresswater_plot <- renderPlot(
        ggplot(data = total_impact_reactive(), aes(x = commodity, y = stresswater_total, fill = commodity)) +
            geom_col() +
            labs(x = "") +
            labs(y = "millions of L\n") +
            labs(fill = "") +
            labs(title = "Stress-weighted Water Use") +
            theme_bw() +
            theme(legend.position = "none",
                  axis.text.y = element_text(size=16),
                  axis.text.x = element_text(size=16),
                  title = element_text(size=20))
    )
    
   
    
    # Tab 4: Chloropleth for 8 Foods
    
    # T4: Chloropleth
    impact_react <- reactive({
        food_state %>%
            filter(commodity %in% input$food_select)
    })
    
    othst_react <- reactive({
        thing <- py_state %>% 
            filter(commodity %in% input$food_select & state == "OTHER STATES") %>% 
            select(production)
            
        return(round(thing[[1]], digits = 0))
    })
    
    output$impact_chart <- renderPlot(
        ggplot() +
            geom_polygon(data = impact_react(), aes(x = long, y = lat, group = group, fill = production),
                         color = "white") +
            scale_fill_continuous(limits=c(min(food_state$production),max(food_state$production)), type = "viridis") +
            coord_quickmap() +
            labs(fill = "Annual commidity production \n(10^6 kilograms)",
                 x = sprintf("%i million kilograms in all other states", othst_react()),
                 y = "") +
            theme_bw() +
            theme(axis.title.x = element_text(size = 16),
                  legend.text = element_text(size = 16),
                  legend.title = element_text(size = 16))
    )
}


shinyApp(ui = ui, server = server)

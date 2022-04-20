## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(rgdal)
library(gt)

ui <- dashboardPage(
        dashboardHeader(title = "Donations in Canada Dashboard"),
        dashboardSidebar(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Education", tabName = "education", icon = icon("school")),
                menuItem("Map", tabName = "map", icon = icon("map"))
        ),
        dashboardBody(
                tabItems(
                        tabItem(tabName = "dashboard",
                                fluidRow(
                box(plotlyOutput("plot1"), width = 6),
                box(plotOutput("pie"), width = 6))),
                
                tabItem(tabName = "education",
                        fluidRow(
                                box(plotlyOutput("plot3"), width = 12))),
                
                tabItem(tabName = "map", 
                        fluidRow(
                                box(leafletOutput("plot2"), width = 8)
                                ),
                        fluidRow(
                                box(gt_output("table"), width = 12)
                        ),
                        )
                )
        )
)
                

server <- function(input, output) {
        
        output$plot1 <- renderPlotly({
                        ggplotly(p)
                })
        
        output$plot2 <- renderLeaflet({leaflet() %>% 
                        addTiles() %>% 
                        setView(-74.09, 45.7,  zoom = 2) %>% 
                        addPolygons(data = subset(region, name %in% data2$Province), color = "#444444", opacity = 1.0, fillOpacity = 0.75,
                                    fillColor = ~colorQuantile("Greens", data2$Donations)(data2$Donations),
                                    weight = 1)
        })
        
        output$plot3 <- renderPlotly({ggplotly(edu_plot)})
        
        output$table <- render_gt({
                data2 %>% 
                        pivot_wider(names_from = Province, values_from = Donations) %>%
                        gt()})
        
        output$pie <- renderPlot({pie_plot})
}
data = readRDS('data.rds')

data2 <- data %>%
        group_by(Province) %>%
        summarise(Donations = sum(VALUE))
library(rgdal)

region <- readRDS('region.rds')
data2$Province <- c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Ontario", "Prince Edward Island", "QuÃ©bec", "Saskatchewan")

p <- data %>%
        group_by(Province) %>%
        summarise(Donations = sum(VALUE)) %>%
        ggplot(aes(x = Province, y = Donations, fill = Province)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        theme(axis.text.x = element_text(angle = 90), legend.position='none')
edu_plot <- data %>%
        group_by(Education) %>%
        rename(Donations = VALUE) %>%
        ggplot(aes(y= Donations, x = Education, fill = Education)) +
        geom_boxplot() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())

pie_plot <- data %>%
        group_by(Province) %>%
        summarise(Donations = sum(VALUE)) %>%
        ggplot(aes(x = '', y = Donations, fill = Province)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void()

shinyApp(ui, server)

library(rgdal)
library(sp)
library(tidyverse)
library(rgeos)
library(leaflet)
library(ggthemes)
library(shiny)
library(shinydashboard)
Boston <- readOGR("/Users/heguo/Desktop/615/Public_Schools/Public_Schools.shp")
Boston<-data.frame(Boston)

m <- leaflet(Boston) %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(~coords.x1, ~coords.x2, popup=~SCH_NAME)

m2<-qmplot(coords.x1, coords.x2, data = Boston[(1:20),])
Boston.south<-subset(Boston,Boston$CITY=="South Boston")
S.boston<-qmplot(coords.x1, coords.x2, data = Boston.south)
Boston.DT<-subset(Boston,Boston$CITY=="Boston")
B.boston<-qmplot(coords.x1, coords.x2, data = Boston.DT)
Boston.east<-subset(Boston,Boston$CITY=="East Boston")
E.boston<-qmplot(coords.x1, coords.x2, data = Boston.east)

ui <- dashboardPage(
    dashboardHeader(title = "Boston Public School"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "Intro", icon = icon("dashboard")),
            menuItem("Public School", tabName = "pu", icon = icon("th")),
            menuItem("City", tabName = "city", icon = icon("th"))
        )## sidebarMenu
    ),#Sidebar
    dashboardBody(
        tabItems(
            tabItem(tabName = "Intro",
                    fluidRow(column(12,
                                    box(width=50,solidHeader = TRUE, status = "primary",
                                        title="Introduction",
                                        h4("There are 131 public schools in Boston area. 
                                           You can click on the mark on the map, and it will show the school name.
                                           If users want to see public schools in one city, there are some maps can display",
                                           size = 10,style = "font-family: 'Arial'," )
                                    )#box
                    )#column
                    
                    )# fluidRow      
            ),#tabItemIntro
            tabItem(tabName = "pu",
                    fluidRow(column(12,
                                    box(width=50,solidHeader = TRUE, status = "primary",
                                        title="Public School in Boston Area",
                                        leafletOutput("mapPlot"))#leafletOutput
                                
                    )#column
                    
                    )#fluidRow
                   
                    ),#tabItempu
            tabItem(tabName = "city",
                    fluidPage(
                        
                        # Copy the line below to make a set of radio buttons
                        radioButtons("radio", label = h3("Radio buttons"),
                                     choices = list("East Boston" = 1, "Boston" = 2, "South Boston" = 3), 
                                     selected = 1),
                        
                        hr(),
                        fluidRow(column(10, plotOutput("cityPlot")))
                        
                    )#column
                    
                    )#fluidRow
                    
            
        )#tabItems
    )
)#Dpage


server <- function(input, output) {
    output$mapPlot <- renderLeaflet({
        m
    })
    output$cityPlot<- renderPlot({
        if(input$radio=="1"){
            E.boston
        }else if(input$radio=="2"){
            B.boston
        }else{
            S.boston
        }
        
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
